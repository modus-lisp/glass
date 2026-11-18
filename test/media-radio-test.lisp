;;;; test/media-radio-test.lisp -- Media's Stations panel: the stations file, the list and its pages,
;;;; a station tuned through spool's radio player (found by name) into Media's mixer, the live title,
;;;; pause/rejoin and stop, one-thing-plays with warp's file player, the narrow layout, and closing.
;;;;   sbcl --script test/media-radio-test.lisp      (exits 0 iff every check passes)
;;;;
;;;; No network and no sound device: the "station" is an Icecast-shaped server on 127.0.0.1 in this
;;;; process, sending an MP3 with ICY metadata; the mixer is a clockless one ticked here.  Set
;;;; MEDIA_PNG_DIR to also write the window as PNGs (480x520 and 400x700).

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (make-pathname :name nil :type nil
                     :defaults (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))
      asdf:*central-registry*)
(require :sb-bsd-sockets)
(require :sb-posix)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
    (asdf:load-system :glass/desk-apps)
    (asdf:load-system :glass/audio)
    (asdf:load-system :warp-media/glass)
    (asdf:load-system :spool)
    (asdf:load-system :png-pure)))

(defpackage #:glass-media-radio-test (:use #:cl))
(in-package #:glass-media-radio-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&MEDIA-RADIO ~:[FAIL~;ok  ~] ~a~%" ok name)
  (unless ok (incf *fails*))
  ok)

(defun wait-for (seconds fn &optional mixer)
  "FN true within SECONDS, ticking MIXER (20 ms a tick) meanwhile."
  (loop with end = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        for v = (funcall fn)
        until (or v (> (get-internal-real-time) end))
        do (if mixer (glass:mixer-tick mixer) nil) (sleep 0.02)
        finally (return v)))

;;; ---- the fixture: a music folder with a track and a stations file --------------------------------

(defvar *dir* (format nil "~a/glass-media-radio-~d/" (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                      (sb-posix:getpid)))
(ensure-directories-exist *dir*)
(defvar *mp3* (merge-pathnames "Music/tone-sweep-440-880.mp3" (user-homedir-pathname)))
(defvar *mp3-bytes*
  (with-open-file (s *mp3* :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence b s) b)))
(with-open-file (o (merge-pathnames "sweep.mp3" *dir*) :direction :output :if-exists :supersede
                                                       :element-type '(unsigned-byte 8))
  (write-sequence *mp3-bytes* o))

;;; ---- an Icecast-shaped server: an MP3, looped, with a StreamTitle every 4096 bytes ---------------

(defvar *title* "Fixture Artist - Fixture Song")
(defvar *listener* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
(setf (sb-bsd-sockets:sockopt-reuse-address *listener*) t)
(sb-bsd-sockets:socket-bind *listener* #(127 0 0 1) 0)
(sb-bsd-sockets:socket-listen *listener* 4)
(defvar *port* (nth-value 1 (sb-bsd-sockets:socket-name *listener*)))
(defvar *requests* '())

(defun serve-one (sock)
  (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t :element-type '(unsigned-byte 8)
                                                   :buffering :full)))
    (unwind-protect
         (handler-case
             (let ((req (with-output-to-string (o)
                          (loop with last = '()
                                for b = (read-byte s nil nil) while b
                                do (write-char (code-char b) o)
                                   (setf last (last (append last (list b)) 4))
                                   (when (equal last '(13 10 13 10)) (return))))))
               (push req *requests*)
               (flet ((ascii (str) (write-sequence (map '(vector (unsigned-byte 8)) #'char-code str) s)))
                 (ascii (format nil "HTTP/1.0 200 OK~c~cContent-Type: audio/mpeg~c~cicy-name: Fixture FM~c~cicy-metaint: 4096~c~c~c~c"
                                #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                #\Return #\Newline))
                 (let* ((meta (format nil "StreamTitle='~a';" *title*))
                        (blocks (ceiling (length meta) 16))
                        (padded (make-array (* 16 blocks) :element-type '(unsigned-byte 8) :initial-element 0))
                        (pos 0) (n (length *mp3-bytes*)))
                   (replace padded (map '(vector (unsigned-byte 8)) #'char-code meta))
                   (loop
                     (dotimes (i 4096)
                       (write-byte (aref *mp3-bytes* pos) s)
                       (setf pos (mod (1+ pos) n)))
                     (write-byte blocks s)
                     (write-sequence padded s)
                     (finish-output s)
                     (sleep 0.02)))))
           (error () nil))
      (ignore-errors (close s :abort t)))))

(defvar *server*
  (sb-thread:make-thread
   (lambda ()
     (loop (let ((c (ignore-errors (sb-bsd-sockets:socket-accept *listener*))))
             (if c
                 (sb-thread:make-thread (lambda () (serve-one c)) :name "fixture-icecast-conn")
                 (return)))))
   :name "fixture-icecast"))

(defvar *url* (format nil "http://127.0.0.1:~d/live" *port*))
(with-open-file (o (merge-pathnames "stations.txt" *dir*) :direction :output :if-exists :supersede)
  (format o "# my stations~%Fixture FM | ~a~%~%  Second | http://127.0.0.1:1/none  ~%Secure | https://example.com/x~%no bar here http://x.example/~%| http://nameless.example/~%Third | http://www.third.example:8000/a~%Fourth | http://four.example/~%Fifth | http://five.example/~%Sixth | http://six.example/~%Seventh | http://seven.example/~%" *url*))

;;; ---- the stations file ------------------------------------------------------------------------------

(let ((st (glass.desk:media-stations (glass.desk:media-stations-file *dir*))))
  (check "stations.txt: 7 stations, in order, https/bar-less/nameless lines dropped"
         (and (= 7 (length st)) (equal (car (first st)) "Fixture FM") (equal (cdr (first st)) *url*)
              (equal (second st) '("Second" . "http://127.0.0.1:1/none"))
              (not (find "Secure" st :key #'car :test #'string=)))))
(check "no stations file: the defaults"
       (equal (glass.desk:media-stations (merge-pathnames "nope/stations.txt" *dir*)) glass.desk:*default-stations*))
(sb-posix:setenv "GLASS_STATIONS" "/elsewhere/stations.txt" 1)
(check "GLASS_STATIONS names the file" (equal (glass.desk:media-stations-file *dir*) "/elsewhere/stations.txt"))
(sb-posix:unsetenv "GLASS_STATIONS")
(check "else stations.txt in Media's folder"
       (equal (glass.desk:media-stations-file (string-right-trim "/" *dir*)) (format nil "~astations.txt" *dir*)))

;;; ---- the window --------------------------------------------------------------------------------------

(defun save-png (fb path)
  (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb))
         (px (make-array (* w h 3) :element-type '(unsigned-byte 8))))
    (dotimes (y h) (dotimes (x w)
      (let ((c (glass:fb-get fb x y)) (i (* 3 (+ x (* y w)))))
        (setf (aref px i) (ldb (byte 8 16) c) (aref px (+ i 1)) (ldb (byte 8 8) c) (aref px (+ i 2)) (ldb (byte 8 0) c)))))
    (png-pure:write-png-file px w h path)))

(defun labels-of (panel) (remove nil (mapcar #'glass.desk::rt-label (glass.desk::panel-targets panel))))

(defun target (panel label)
  (find label (glass.desk::panel-targets panel) :key #'glass.desk::rt-label :test #'equal))

(defun open-media (w h)
  "Media in a W x H framebuffer: (values fb on-pointer dirty-p close-fn panel)."
  (let ((fb (glass:make-framebuffer w h)))
    (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn resize-fn text-p selection-fn)
        (glass.desk:media-app fb)
      (declare (ignore on-key copy-p))
      (check (format nil "~dx~d: no RESIZE-FN, TEXT-P or SELECTION-FN, as before" w h)
             (and (null resize-fn) (null text-p) (null selection-fn)))
      ;; the panel is the one this window made: the newest with this framebuffer
      (values fb on-pointer dirty-p close-fn))))

(defun tap (on-pointer x y) (funcall on-pointer 1 x y) (funcall on-pointer 0 x y))

(defun tap-label (on-pointer panel label)
  (let ((tg (target panel label)))
    (when tg
      (tap on-pointer (+ (glass.desk::rt-x tg) (floor (glass.desk::rt-w tg) 2))
           (+ (glass.desk::rt-y tg) (floor (glass.desk::rt-h tg) 2)))
      t)))

(defvar *panels* '())
(defmethod initialize-instance :after ((p glass.desk:radio-panel) &key) (push p *panels*))

(let* ((mixer (glass:make-mixer :rate 48000))
       (glass.desk:*media-mixer* mixer)
       (glass.desk:*media-root* *dir*)
       (glass.desk::*media-library* nil)
       (tu glass.desk:*radio-tuner*)
       (png-dir (sb-ext:posix-getenv "MEDIA_PNG_DIR")))
  (glass:mixer-subscribe mixer :name "ear" :rate 48000)
  (multiple-value-bind (fb on-pointer dirty-p close-fn) (open-media 480 520)
    (let ((panel (first *panels*)))
      (check "it paints when opened" (funcall dirty-p))
      (check "and then is quiet (no repaint for 1.5 s with nothing playing)"
             (loop repeat 75 never (progn (sleep 0.02) (funcall dirty-p))))
      (check "the panel takes the bottom of the window, the file list the rest"
             (and (= (glass.desk::panel-y panel) (- 520 (glass.desk::panel-height panel)))
                  (<= (glass.desk::panel-height panel) 260)))
      (check "warp's file list is listed above it (the track in Media's folder)"
             (let ((lib glass.desk::*media-library*))
               (and lib (find "sweep" (warp-media:folder-tracks (warp-media:library-root lib))
                              :key #'pathname-name :test #'string=))))
      (check "and painted: the top half is not blank"
             (let ((seen (make-hash-table)))
               (loop for y from 0 below (glass.desk::panel-y panel) by 3
                     do (loop for x from 0 below 480 by 3 do (setf (gethash (glass:fb-get fb x y) seen) t)))
               (> (hash-table-count seen) 4)))
      (check "page 1: the first five stations, a row each, every row a fingertip tall and the full width"
             (let ((rows (remove-if-not (lambda (tg) (find (glass.desk::rt-label tg) (glass.desk:panel-stations panel)
                                                           :key #'car :test #'equal))
                                        (glass.desk::panel-targets panel))))
               (and (= 5 (length rows))
                    (equal (sort (mapcar #'glass.desk::rt-label rows) #'string<)
                           (sort (list "Fixture FM" "Second" "Third" "Fourth" "Fifth") #'string<))
                    (every (lambda (tg) (and (>= (glass.desk::rt-h tg) 36) (= 480 (glass.desk::rt-w tg)))) rows))))
      (check "more than a page: a pager" (and (target panel "Next page") (target panel "Previous page")))
      (tap-label on-pointer panel "Next page") (funcall dirty-p)
      (check "Next page: the other two" (and (= 1 (glass.desk:panel-page panel))
                                             (target panel "Sixth") (target panel "Seventh")
                                             (not (target panel "Fixture FM"))))
      (tap-label on-pointer panel "Next page") (funcall dirty-p)
      (check "and no further" (= 1 (glass.desk:panel-page panel)))
      (tap-label on-pointer panel "Previous page") (funcall dirty-p)
      (check "Previous page: back to the first" (and (= 0 (glass.desk:panel-page panel)) (target panel "Fixture FM")))
      ;; no mixer: tuning says so, and nothing is made
      (let ((glass.desk:*media-mixer* nil))
        (tap-label on-pointer panel "Second") (funcall dirty-p))
      (check "with no mixer, tuning says there is no sound output"
             (and (search "No sound output" (or (glass.desk:tuner-note tu) "")) (null (glass.desk:tuner-player tu))))
      ;; tune the fixture station
      (tap-label on-pointer panel "Fixture FM")
      (check "a tap on a station makes spool's player, by name, and puts it on Media's mixer as \"radio\""
             (and (glass.desk:tuner-player tu) (typep (glass.desk:tuner-player tu) 'spool::player)
                  (glass:mixer-find-source mixer "radio")))
      (check "and tunes that station's URL"
             (and (equal (glass.desk:tuner-station tu) (cons "Fixture FM" *url*))
                  (wait-for 3 (lambda () (let ((r (spool::player-radio (glass.desk:tuner-player tu))))
                                           (and r (equal (spool::radio-url r) *url*)))))))
      (let ((p (glass.desk:tuner-player tu)))
        (check "it plays (the fixture server answers; the mixer pulls it)"
               (wait-for 15 (lambda () (eq (spool:player-state p) :playing)) mixer))
        (check "the server was asked for ICY metadata" (some (lambda (r) (search "Icy-MetaData: 1" r)) *requests*))
        (check "the live title arrives (ICY StreamTitle)"
               (wait-for 10 (lambda () (equal (spool:player-stream-title p) *title*)) mixer))
        (wait-for 1.2 (lambda () nil) mixer)
        (check "the window repaints for it" (wait-for 2 (lambda () (funcall dirty-p)) mixer))
        (multiple-value-bind (l1 l2) (glass.desk::%ra-now tu)
          (check "now playing: the title, then the station and LIVE with the time"
                 (and (equal l1 *title*) (search "Fixture FM" l2) (search "LIVE" l2))))
        (check "the station's row is marked" (glass.desk::panel-shown panel))
        (when png-dir (save-png fb (format nil "~a/media-480x520.png" (string-right-trim "/" png-dir))))
        (check "the radio is heard on the mix"
               (wait-for 3 (lambda () (> (glass:mixer-level mixer) 0.001d0)) mixer))
        ;; pause and rejoin
        (tap-label on-pointer panel "Play/Pause")
        (check "Play/Pause pauses it" (eq (spool:player-state p) :paused))
        (funcall dirty-p)
        (tap-label on-pointer panel "Play/Pause")
        (check "and again rejoins the live edge (buffering, then playing)"
               (and (member (spool:player-state p) '(:buffering :playing))
                    (wait-for 10 (lambda () (eq (spool:player-state p) :playing)) mixer)))
        ;; a tap on the station that is on pauses it rather than restarting it
        (funcall dirty-p)
        (tap-label on-pointer panel "Fixture FM")
        (check "a tap on the playing station's row pauses it" (eq (spool:player-state p) :paused))
        (tap-label on-pointer panel "Fixture FM")
        (wait-for 10 (lambda () (eq (spool:player-state p) :playing)) mixer)
        ;; the narrow window, while it plays
        (multiple-value-bind (nfb non-pointer ndirty nclose) (open-media 400 700)
          (declare (ignore non-pointer))
          (let ((np (first *panels*)))
            (check "400x700: it paints" (funcall ndirty))
            (check "400x700: all seven stations on one page, no pager, rows full width"
                   (and (= 1 (glass.desk::%ra-pages np)) (not (target np "Next page"))
                        (every (lambda (s) (let ((tg (target np (car s)))) (and tg (= 400 (glass.desk::rt-w tg)))))
                               (glass.desk:panel-stations np))))
            (check "400x700: the panel still leaves the file list most of the window"
                   (>= (glass.desk::panel-y np) 350))
            (when png-dir (save-png nfb (format nil "~a/media-400x700.png" (string-right-trim "/" png-dir))))
            (funcall nclose)
            (check "closing one of two windows leaves the station playing"
                   (progn (sleep 0.3) (eq (spool:player-state p) :playing)))))
        ;; one thing plays: a file started in warp's list stops the station
        (let ((fp (warp-media:library-player glass.desk::*media-library*)))
          (warp-media:play-path fp (merge-pathnames "sweep.mp3" *dir*))
          (check "starting a file stops the station (within the panel's once-a-second look)"
                 (wait-for 4 (lambda () (funcall dirty-p) (eq (spool:player-state p) :idle)) mixer))
          ;; and tuning pauses the file
          (wait-for 1 (lambda () (eq (warp-media:player-state fp) :playing)) mixer)
          (tap-label on-pointer panel "Fixture FM")
          (check "tuning a station pauses a playing file"
                 (and (not (eq (warp-media:player-state fp) :playing))
                      (wait-for 10 (lambda () (eq (spool:player-state p) :playing)) mixer))))
        ;; stop
        (funcall dirty-p)
        (tap-label on-pointer panel "Stop")
        (check "Stop stops it" (wait-for 4 (lambda () (eq (spool:player-state p) :idle)) mixer))
        (multiple-value-bind (l1 l2) (glass.desk::%ra-now tu)
          (check "and says so, remembering the station" (and (equal l1 "Fixture FM") (search "stopped" l2))))
        (tap-label on-pointer panel "Play/Pause")
        (check "Play after Stop tunes the station again"
               (wait-for 10 (lambda () (eq (spool:player-state p) :playing)) mixer))
        ;; close the last window: the station stops and leaves the mixer
        (funcall close-fn)
        (check "closing the last Media window stops the station and takes it off the mixer"
               (wait-for 4 (lambda () (and (eq (spool:player-state p) :idle)
                                           (null (glass:mixer-find-source mixer "radio"))))
                         mixer))))))

;;; ---- without spool: the panel says so ----------------------------------------------------------------
(let ((glass.desk:*media-root* *dir*) (glass.desk::*media-library* nil))
  (sb-ext:without-package-locks
    (rename-package "SPOOL" "SPOOL-HIDDEN"))
  (unwind-protect
       (let* ((fb (glass:make-framebuffer 480 520)))
         (multiple-value-bind (k on-pointer dirty-p c close-fn) (glass.desk:media-app fb)
           (declare (ignore k c))
           (let ((panel (first *panels*)))
             (check "without spool it still opens and paints" (funcall dirty-p))
             (check "and the Stations panel says spool is missing"
                    (search "spool" (glass.desk::%ra-now glass.desk:*radio-tuner*)))
             (tap-label on-pointer panel "Second")
             (check "a tap on a station says so too, and fails nothing"
                    (search "spool" (or (glass.desk:tuner-note glass.desk:*radio-tuner*) "")))
             (funcall close-fn))))
    (rename-package "SPOOL-HIDDEN" "SPOOL")))

(ignore-errors (sb-bsd-sockets:socket-close *listener*))
(format t "~&MEDIA-RADIO ~:[FAIL (~d)~;PASS~]~%" (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
