;;;; media-radio.lisp -- internet radio in the Media window: a Stations panel under warp's player.
;;;;
;;;; backend/music-app.lisp (the McCLIM Music window) could play a station as well as a file; the
;;;; desk's Media window could only play files.  This is the stations half of that window, moved
;;;; into Media rather than kept as a second music player: a list of Icecast / Shoutcast stations
;;;; under the file list, a now-playing line with the station and what it says is on (ICY
;;;; StreamTitle), and a pause and a stop for it.
;;;;
;;;; IT DOES NOT FETCH OR DECODE ANYTHING.  spool does (spool/src/radio.lisp: PLAY-URL, its fetcher
;;;; and decoder threads, the ICY metadata), reached BY NAME as music-app.lisp reaches it, so glass
;;;; does not depend on spool: a desk built without spool still has Media, and the Stations panel
;;;; says what is missing instead of failing.
;;;;
;;;; ONE TUNER, ITS OWN PLAYER.  Not warp's file player (its engine has no live stream) and not
;;;; spool.glass's podcast singleton (a station would stop your episode with no way to tell why).
;;;; Its source goes on the mixer Media's own player plays into -- *MEDIA-MIXER*, the compositor's --
;;;; as "radio", so the Mixer window shows it as its own row.  With no *MEDIA-MIXER* Media plays
;;;; files silently against the wall clock; a live stream has no wall clock to follow (nothing would
;;;; pull it), so tuning says there is no sound output instead.
;;;;
;;;; ONE THING PLAYS.  Tuning a station pauses a file that is playing, and starting a file stops the
;;;; station (noticed by the panel's once-a-second look), because two sources talking over each
;;;; other is never what a tap on a list meant.
;;;;
;;;; LIVE MEANS NO SEEK.  The transport is play/pause and stop.  Pause is spool's: a paused stream
;;;; plays silence, and play REJOINS the live edge (radio.lisp drops what was buffered) -- there is
;;;; nothing to resume from.  A dropped or stopped station's play button tunes it again.
;;;;
;;;; NOTHING SLOW ON THE WINDOW MANAGER'S THREAD.  PLAY-URL and STOP join spool's radio threads
;;;; (up to seconds), so they run on a thread of their own, one at a time; making the player and
;;;; putting its source on the mixer are quick and happen on the caller's thread.
;;;;
;;;; THE STATIONS FILE.  `Name | http://...` lines in stations.txt in Media's folder (*MEDIA-ROOT*),
;;;; or the file GLASS_STATIONS names; `#` for comments.  http: only (the player has no TLS here);
;;;; a missing or empty file is *DEFAULT-STATIONS*.  Read when a Media window opens.

(in-package #:glass.desk)

(export '(*default-stations* *media-import* media-stations media-stations-file *radio-tuner*
          tuner-player tuner-station tuner-note radio-panel panel-stations panel-page panel-rows))

;;; ---- spool and the mixer, by name ------------------------------------------------------------

(defun %ra-fn (name &optional (pkg "SPOOL"))
  (let ((s (and (find-package pkg) (find-symbol name pkg))))
    (and s (fboundp s) s)))

(defun %ra-spool-p ()
  (and (%ra-fn "MAKE-PLAYER") (%ra-fn "PLAY-URL") (%ra-fn "PLAYER-SOURCE") t))

(defun %ra-call (name &rest args)
  "spool's NAME applied to ARGS, or NIL when this spool has no such function."
  (let ((f (%ra-fn name))) (and f (apply f args))))

;;; ---- stations ------------------------------------------------------------------------------------

(defparameter *default-stations*
  '(("SomaFM Groove Salad" . "http://ice1.somafm.com/groovesalad-128-mp3")
    ("SomaFM Drone Zone"   . "http://ice1.somafm.com/dronezone-128-mp3")
    ("SomaFM Secret Agent" . "http://ice1.somafm.com/secretagent-128-mp3")
    ("Radio Paradise"      . "http://stream.radioparadise.com/mp3-128"))
  "What a fresh desktop offers before anyone has written a stations file.")

(defun %ra-dir-pathname (dir)
  (let ((s (namestring dir)))
    (pathname (if (and (plusp (length s)) (char/= (char s (1- (length s))) #\/))
                  (concatenate 'string s "/")
                  s))))

(defun media-stations-file (&optional (dir (or *media-root* "./")))
  "GLASS_STATIONS, else stations.txt in DIR (Media's folder).  Read at use, not at load: a saved
   core is a snapshot of load time and the environment is a runtime thing."
  (let ((env (sb-ext:posix-getenv "GLASS_STATIONS")))
    (if (and env (plusp (length env)))
        env
        (namestring (merge-pathnames "stations.txt" (%ra-dir-pathname dir))))))

(defun media-stations (&optional (file (media-stations-file)))
  "The stations to show, ((NAME . URL) ...): `Name | http://...` lines of FILE, `#` comments.  A
   missing or unreadable file is the defaults, not an error -- the file is how you add a station,
   not a requirement to have one.  Only http: URLs are taken; a line this player cannot play is
   better absent than listed and failing."
  (let ((rows '()))
    (ignore-errors
     (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
       (when in
         (loop for line = (read-line in nil) while line
               do (let* ((l (string-trim '(#\Space #\Tab #\Return) line))
                         (bar (position #\| l)))
                    (when (and bar (plusp (length l)) (char/= (char l 0) #\#))
                      (let ((name (string-trim '(#\Space #\Tab) (subseq l 0 bar)))
                            (url (string-trim '(#\Space #\Tab) (subseq l (1+ bar)))))
                        (when (and (plusp (length name))
                                   (> (length url) 7)
                                   (string-equal "http://" url :end2 7))
                          (push (cons name url) rows)))))))))
    (if rows (nreverse rows) *default-stations*)))

(defun %ra-host (url)
  "The host of an http URL, for the row's right-hand note."
  (let* ((s (if (and (> (length url) 7) (string-equal "http://" url :end2 7)) (subseq url 7) url))
         (end (or (position-if (lambda (c) (member c '(#\/ #\: #\?))) s) (length s)))
         (h (subseq s 0 end)))
    (if (and (> (length h) 4) (string-equal "www." h :end2 4)) (subseq h 4) h)))

;;; ---- the tuner: the one radio player, shared by every Media window -------------------------------

(defclass radio-tuner ()
  ((player :initform nil :accessor tuner-player :documentation "spool's player, made on first tune.")
   (mixer :initform nil :accessor tuner-mixer :documentation "The mixer its source is on.")
   (source :initform nil :accessor tuner-source :documentation "Its handle on that mixer.")
   (station :initform nil :accessor tuner-station
            :documentation "(NAME . URL) last tuned: what play means after a stop or a drop.")
   (note :initform nil :accessor tuner-note :documentation "Something to tell the listener, or NIL.")
   (windows :initform 0 :accessor tuner-windows :documentation "Media windows open.")
   (lock :initform (sb-thread:make-mutex :name "glass-radio") :reader tuner-lock)
   (op-lock :initform (sb-thread:make-mutex :name "glass-radio-op") :reader tuner-op-lock
            :documentation "Held by the one slow operation (tune, stop) in flight."))
  (:documentation "Internet radio for the Media window: spool's player, by name, on Media's mixer."))

(defvar *radio-tuner* (make-instance 'radio-tuner))

(defun %ra-state (tu)
  (let ((p (tuner-player tu))) (and p (ignore-errors (%ra-call "PLAYER-STATE" p)))))

(defun %ra-active-p (tu) (member (%ra-state tu) '(:playing :buffering :loading)))

(defun %ra-get (tu name)
  (let ((p (tuner-player tu))) (and p (ignore-errors (%ra-call name p)))))

(defun %ra-attach (tu mixer)
  "The player, its source on MIXER: made once, moved if Media's mixer is a different one now."
  (sb-thread:with-mutex ((tuner-lock tu))
    (unless (tuner-player tu)
      (setf (tuner-player tu) (%ra-call "MAKE-PLAYER")))
    (let ((p (tuner-player tu)))
      (when (and p (not (and (tuner-source tu) (eq (tuner-mixer tu) mixer))))
        (when (tuner-source tu)
          (ignore-errors (funcall (%ra-fn "MIXER-REMOVE-SOURCE" "GLASS") (tuner-mixer tu) (tuner-source tu))))
        (setf (tuner-source tu)
              (ignore-errors (funcall (%ra-fn "MIXER-ADD-SOURCE" "GLASS") mixer (%ra-call "PLAYER-SOURCE" p)
                                      :name "radio" :gain 1d0 :ears nil))
              (tuner-mixer tu) mixer))
      p)))

(defun %ra-async (tu what fn)
  "FN on its own thread, one operation at a time, a failure a note rather than a dead image (a
   desktop runs under --disable-debugger, where a condition escaping any thread takes it).

   ON MODUS, RIGHT HERE: a thread there may not store into the player or this tuner (another
   thread's objects), and spool's PLAY-URL and STOP only post to the station's own thread now --
   nothing in them waits -- so there is nothing to move off the caller's thread."
  #+modus
  (sb-thread:with-mutex ((tuner-op-lock tu))
    (handler-case (funcall fn)
      (serious-condition (e) (setf (tuner-note tu) (format nil "~a failed: ~a" what e)))))
  #-modus
  (sb-thread:make-thread
   (lambda ()
     (sb-thread:with-mutex ((tuner-op-lock tu))
       (handler-case (funcall fn)
         (serious-condition (e) (setf (tuner-note tu) (format nil "~a failed: ~a" what e))))))
   :name (format nil "glass-radio ~a" what)))

(defun radio-tune (tu station mixer)
  "Play STATION ((NAME . URL)) on MIXER.  Returns at once; the connection is spool's business."
  (setf (tuner-note tu) nil)
  (cond ((not (%ra-spool-p)) (setf (tuner-note tu) "Radio needs spool loaded."))
        ((null mixer) (setf (tuner-note tu) "No sound output here: nothing would hear the station."))
        ((not (%ra-fn "MIXER-ADD-SOURCE" "GLASS")) (setf (tuner-note tu) "No mixer in this image."))
        (t (setf (tuner-station tu) station)
           (let ((p (%ra-attach tu mixer)))
             (if (null p)
                 (setf (tuner-note tu) "Could not make a player.")
                 (%ra-async tu "tune" (lambda () (%ra-call "PLAY-URL" p (cdr station) :name (car station)))))))))

(defun radio-toggle (tu mixer)
  "The play/pause button: pause a live station, rejoin a paused one, retune a dropped one."
  (let ((st (%ra-state tu)))
    (cond ((member st '(:playing :buffering :paused)) (%ra-call "TOGGLE" (tuner-player tu)))
          ((tuner-station tu) (radio-tune tu (tuner-station tu) mixer))
          (t (setf (tuner-note tu) "Tap a station to listen.")))))

(defun radio-stop (tu &key forget)
  (let ((p (tuner-player tu)))
    (when forget (setf (tuner-station tu) nil))
    (when (and p (%ra-fn "STOP"))
      (%ra-async tu "stop" (lambda () (%ra-call "STOP" p))))))

(defun %ra-teardown (tu)
  "The last Media window closed: silence the station and take its source off the mixer.  Without
   this the stream plays on with no window left to stop it."
  (let ((p (tuner-player tu)) (m (tuner-mixer tu)) (src (tuner-source tu)))
    (sb-thread:with-mutex ((tuner-lock tu)) (setf (tuner-source tu) nil (tuner-mixer tu) nil))
    (when (or p src)
      (%ra-async tu "close"
                 (lambda ()
                   (when (and p (%ra-fn "STOP")) (%ra-call "STOP" p))
                   (when src (ignore-errors (funcall (%ra-fn "MIXER-REMOVE-SOURCE" "GLASS") m src))))))))

;;; ---- warp's file player, by name ------------------------------------------------------------------

(defun %ra-file-player ()
  (let ((f (%ra-fn "LIBRARY-PLAYER" "WARP-MEDIA")))
    (and f *media-library* (ignore-errors (funcall f *media-library*)))))

(defun %ra-file-playing-p ()
  (let ((p (%ra-file-player)) (f (%ra-fn "PLAYER-STATE" "WARP-MEDIA")))
    (and p f (eq (ignore-errors (funcall f p)) :playing))))

(defun %ra-pause-file ()
  (when (%ra-file-playing-p)
    (ignore-errors (funcall (%ra-fn "PAUSE" "WARP-MEDIA") (%ra-file-player)))))

;;; ---- look: warp's Media palette, so the panel reads as part of the window ---------------------

(defparameter +ra-bg+      #x0c0e12)
(defparameter +ra-row+     #x161a20)
(defparameter +ra-sel+     #x26303c)
(defparameter +ra-head+    #x1e2530)
(defparameter +ra-control+ #x232a36)
(defparameter +ra-fg+      #xdce4ec)
(defparameter +ra-dim+     #x8a949c)
(defparameter +ra-on+      #x5abe82)
(defparameter +ra-bad+     #xf04646)

(defparameter *ra-row-h* 36 "A station row: a fingertip tall, the whole row the target.")
(defparameter *ra-head-h* 36)
(defparameter *ra-now-h* 44)
(defparameter *ra-control-w* 48 "As wide as Media's own transport buttons.")

(defun %ra-baseline (y h size)
  "The baseline that centres a SIZE-px line in the row Y..Y+H (warp-glass's ROW-BASELINE)."
  (let ((font (glass:default-font)))
    (+ y (floor (- h size) 2) (round (* (scribe:font-ascent font) size) (scribe:font-units-per-em font)))))

(defun %ra-text (fb x baseline text &key (size 12) (color +ra-fg+))
  (let ((font (glass:default-font)))
    (glass:fb-text fb x (- baseline (round (* (scribe:font-ascent font) size) (scribe:font-units-per-em font)))
                   text :size size :color color)))

(defun %ra-width (text size) (glass:text-width text :size size))

(defun %ra-fit (text width size)
  "TEXT shortened with an ellipsis until it fits WIDTH (a binary search: the font is proportional)."
  (let ((text (or text "")))
    (cond ((<= (%ra-width text size) width) text)
          ((<= width (%ra-width "..." size)) "")
          (t (let ((lo 0) (hi (length text)))
               (loop while (> (- hi lo) 1)
                     do (let ((mid (floor (+ lo hi) 2)))
                          (if (<= (%ra-width (concatenate 'string (subseq text 0 mid) "...") size) width)
                              (setf lo mid) (setf hi mid))))
               (concatenate 'string (string-right-trim " " (subseq text 0 lo)) "..."))))))

(defun %ra-icon (fb kind cx cy color)
  "Transport glyphs as shapes, 14 px: a glyph missing from the font would read as a bug."
  (ecase kind
    (:play (loop for i from 0 to 14
                 do (glass:fb-rect fb (- cx 5) (+ cy -7 i) (1+ (- 7 (abs (- i 7)))) 1 color)))
    (:pause (glass:fb-rect fb (- cx 6) (- cy 7) 4 15 color)
            (glass:fb-rect fb (+ cx 2) (- cy 7) 4 15 color))
    (:stop (glass:fb-rect fb (- cx 6) (- cy 6) 13 13 color))
    ;; the pager's chevrons: NOT the transport's |< >|, which would read as previous/next track
    (:left (loop for i from -6 to 6
                 do (glass:fb-rect fb (+ cx -4 (floor (* 3 (abs i)) 4)) (+ cy i) 3 1 color)))
    (:right (loop for i from -6 to 6
                  do (glass:fb-rect fb (- (+ cx 2) (floor (* 3 (abs i)) 4)) (+ cy i) 3 1 color)))))

;;; ---- the panel ------------------------------------------------------------------------------------

(defclass radio-target ()
  ((x :initarg :x :reader rt-x) (y :initarg :y :reader rt-y)
   (w :initarg :w :reader rt-w) (h :initarg :h :reader rt-h)
   (action :initarg :action :reader rt-action)
   (label :initarg :label :initform nil :reader rt-label
          :documentation "What it is, for a test to find it by, as a person would."))
  (:documentation "Somewhere a press does something, recorded as it is drawn."))

(defclass radio-panel ()
  ((fb :initarg :fb :reader panel-fb)
   (tuner :initarg :tuner :initform *radio-tuner* :reader panel-tuner)
   (stations :initarg :stations :accessor panel-stations)
   (y :initarg :y :accessor panel-y :documentation "Its top in the window.")
   (height :initarg :height :accessor panel-height)
   (rows :initarg :rows :accessor panel-rows :documentation "Station rows per page.")
   (page :initform 0 :accessor panel-page)
   (targets :initform '() :accessor panel-targets)
   (dirty :initform t :accessor panel-dirty)
   (shown :initform nil :accessor panel-shown)
   (checked :initform 0 :accessor panel-checked)
   (radio-was :initform nil :accessor panel-radio-was))
  (:documentation "The Stations panel at the bottom of one Media window."))

(defun radio-panel-height (window-height n-stations &key whole)
  "(values HEIGHT ROWS): a head, the now-playing line, and as many station rows as there are,
   up to half the window -- the rest stays the file list's.  Paged beyond that.  WHOLE: there is
   no file list (warp's player is not loaded), so the panel is the window."
  (let* ((fixed (+ *ra-head-h* *ra-now-h*))
         (room (if whole window-height (floor window-height 2)))
         (fit (max 1 (floor (- room fixed) *ra-row-h*)))
         (rows (max 1 (min (max 1 n-stations) fit))))
    (values (if whole window-height (+ fixed (* rows *ra-row-h*))) rows)))

(defun make-radio-panel (fb &key (stations (media-stations)) (tuner *radio-tuner*) whole)
  (multiple-value-bind (h rows) (radio-panel-height (glass:fb-height fb) (length stations) :whole whole)
    (make-instance 'radio-panel :fb fb :tuner tuner :stations stations :rows rows :height h
                                :y (- (glass:fb-height fb) h))))

(defun radio-panel-resized (pn &key whole)
  "PN's framebuffer has a new size: its rows, height and place again, from the first page."
  (multiple-value-bind (h rows) (radio-panel-height (glass:fb-height (panel-fb pn)) (length (panel-stations pn))
                                                    :whole whole)
    (setf (panel-height pn) h (panel-rows pn) rows (panel-y pn) (- (glass:fb-height (panel-fb pn)) h)
          (panel-page pn) 0 (panel-dirty pn) t (panel-shown pn) nil)))

(defun %ra-target (pn x y w h action &optional label)
  (push (make-instance 'radio-target :x x :y y :w w :h h :action action :label label) (panel-targets pn)))

(defun %ra-pages (pn) (max 1 (ceiling (length (panel-stations pn)) (panel-rows pn))))

(defun %ra-control (pn x y w h icon label action)
  (let ((fb (panel-fb pn)))
    (glass:fb-rect fb x y w h +ra-control+)
    (glass:fb-rect fb x y 1 h +ra-bg+)
    (%ra-icon fb icon (+ x (floor w 2)) (+ y (floor h 2)) +ra-fg+)
    (%ra-target pn x y w h action label)))

(defun %ra-mixer () *media-mixer*)

(defun %ra-now (tu)
  "(values LINE1 LINE2 LINE2-COLOUR ACTIVE) for the now-playing line."
  (let* ((st (%ra-state tu))
         (station (or (%ra-get tu "PLAYER-STATION") (car (tuner-station tu))))
         (title (and (member st '(:playing :buffering :paused)) (%ra-get tu "PLAYER-STREAM-TITLE")))
         (note (tuner-note tu)))
    (cond ((not (%ra-spool-p))
           (values "Radio needs spool loaded" "This image has no stream player." +ra-dim+ nil))
          ((and (null station) (not (member st '(:playing :buffering :paused))))
           (values "Not playing" (or note "Tap a station to listen.") +ra-dim+ nil))
          (t
           (values (or title station "Radio")
                   (if note
                       note
                       (format nil "~@[~a  ~]~a" (and title station)
                               (case st
                                 (:playing (format nil "LIVE  ~a" (%ra-mmss (%ra-get tu "PLAYER-POSITION"))))
                                 (:buffering "connecting...")
                                 (:paused "paused")
                                 (:error (or (%ra-get tu "PLAYER-ERROR") "error"))
                                 (t "stopped"))))
                   (cond (note +ra-dim+) ((eq st :error) +ra-bad+) ((eq st :playing) +ra-on+) (t +ra-dim+))
                   (member st '(:playing :buffering :paused)))))))

(defun %ra-mmss (secs)
  (if (and (realp secs) (>= secs 0))
      (multiple-value-bind (m s) (floor (floor secs) 60)
        (if (>= m 60)
            (multiple-value-bind (h m) (floor m 60) (format nil "~d:~2,'0d:~2,'0d" h m s))
            (format nil "~d:~2,'0d" m s)))
      "--:--"))

(defun radio-panel-redraw (pn)
  (let* ((fb (panel-fb pn)) (tu (panel-tuner pn))
         (w (glass:fb-width fb)) (y0 (panel-y pn))
         (n (length (panel-stations pn))) (per (panel-rows pn))
         (pages (%ra-pages pn))
         (page (setf (panel-page pn) (max 0 (min (panel-page pn) (1- pages)))))
         (st (%ra-state tu)))
    (setf (panel-targets pn) '())
    (glass:fb-rect fb 0 y0 w (panel-height pn) +ra-bg+)
    ;; the head: what this is, and the pager when there is more than a page
    (glass:fb-rect fb 0 y0 w *ra-head-h* +ra-head+)
    (%ra-text fb 8 (%ra-baseline y0 *ra-head-h* 12) "Stations" :size 12)
    (let ((info (if (> pages 1)
                    (format nil "~d stations  ~d/~d" n (1+ page) pages)
                    (format nil "~d station~:p" n)))
          (right (- w 8)))
      (when (> pages 1)
        (decf right (* 2 *ra-control-w*))
        (%ra-control pn (- w (* 2 *ra-control-w*)) y0 *ra-control-w* *ra-head-h* :left "Previous page"
                     (lambda () (setf (panel-page pn) (max 0 (1- page)))))
        (%ra-control pn (- w *ra-control-w*) y0 *ra-control-w* *ra-head-h* :right "Next page"
                     (lambda () (setf (panel-page pn) (min (1- pages) (1+ page)))))
        (decf right 8))
      (%ra-text fb (- right (%ra-width info 10)) (%ra-baseline y0 *ra-head-h* 10) info :size 10 :color +ra-dim+))
    ;; now playing: the stream's title, the station and LIVE, play/pause and stop
    (let ((ny (+ y0 *ra-head-h*)))
      (multiple-value-bind (line1 line2 colour active) (%ra-now tu)
        (glass:fb-rect fb 0 ny w *ra-now-h* (if active +ra-sel+ +ra-row+))
        (let ((room (- w 16 (if (%ra-spool-p) (* 2 *ra-control-w*) 0))))
          (%ra-text fb 10 (+ ny 18) (%ra-fit line1 room 13) :size 13 :color (if active +ra-fg+ +ra-dim+))
          (%ra-text fb 10 (+ ny 36) (%ra-fit line2 room 10) :size 10 :color colour)))
      (when (%ra-spool-p)
        (%ra-control pn (- w (* 2 *ra-control-w*)) ny *ra-control-w* *ra-now-h*
                     (if (member st '(:playing :buffering)) :pause :play) "Play/Pause"
                     (lambda () (radio-toggle tu (%ra-mixer)) (%ra-noted-file pn)))
        (%ra-control pn (- w *ra-control-w*) ny *ra-control-w* *ra-now-h* :stop "Stop"
                     (lambda () (setf (tuner-note tu) nil) (radio-stop tu)))))
    ;; the stations: a row each, the whole row the target
    (let ((top (+ y0 *ra-head-h* *ra-now-h*))
          (tuned (tuner-station tu))
          (on (member st '(:playing :buffering :paused))))
      (loop for s in (subseq (panel-stations pn) (min n (* page per)) (min n (* (1+ page) per)))
            for ry from top by *ra-row-h*
            do (let* ((s s)
                      (this (and on tuned (string= (cdr s) (cdr tuned))))
                      (host (%ra-host (cdr s))))
                 (glass:fb-rect fb 0 ry w *ra-row-h* (if this +ra-sel+ +ra-row+))
                 (glass:fb-rect fb 0 (+ ry *ra-row-h* -1) w 1 +ra-bg+)
                 (%ra-text fb (- w 10 (%ra-width host 10)) (%ra-baseline ry *ra-row-h* 10) host
                           :size 10 :color +ra-dim+)
                 (%ra-text fb 10 (%ra-baseline ry *ra-row-h* 12)
                           (format nil "~a ~a" (cond ((not this) " ") ((eq st :paused) "||") (t ">"))
                                   (%ra-fit (car s) (- w 40 (%ra-width host 10)) 12))
                           :size 12 :color (if this +ra-on+ (if (%ra-spool-p) +ra-fg+ +ra-dim+)))
                 (%ra-target pn 0 ry w *ra-row-h*
                             (lambda ()
                               ;; the station that is on: a tap means pause (or rejoin), never restart
                               (if (and this (member (%ra-state tu) '(:playing :buffering :paused)))
                                   (radio-toggle tu (%ra-mixer))
                                   (progn (%ra-pause-file) (radio-tune tu s (%ra-mixer))))
                               (%ra-noted-file pn))
                             (car s)))))
    t))

(defun %ra-noted-file (pn)
  "After a tap that may have started the radio: remember it is ours, so a file found playing
   later means somebody started one."
  (setf (panel-radio-was pn) (and (%ra-active-p (panel-tuner pn)) t)))

(defun radio-panel-press (pn x y)
  "A press at window X,Y: the last-drawn target under it, if any.  T if one was hit."
  (let ((tg (find-if (lambda (r) (and (<= (rt-x r) x (+ (rt-x r) (rt-w r) -1))
                                      (<= (rt-y r) y (+ (rt-y r) (rt-h r) -1))))
                     (panel-targets pn))))
    (when tg
      (handler-case (funcall (rt-action tg))
        (serious-condition (e) (setf (tuner-note (panel-tuner pn)) (princ-to-string e))))
      (setf (panel-dirty pn) t)
      t)))

(defun %ra-signature (pn)
  (let ((tu (panel-tuner pn)))
    (list (%ra-spool-p) (%ra-state tu)
          (let ((pos (%ra-get tu "PLAYER-POSITION"))) (and (realp pos) (floor pos)))
          (%ra-get tu "PLAYER-STREAM-TITLE") (%ra-get tu "PLAYER-STATION") (%ra-get tu "PLAYER-ERROR")
          (tuner-note tu) (tuner-station tu))))

(defun radio-panel-dirty-p (pn)
  "Repaint iff something it shows changed: a tap (the DIRTY flag) or, looked at once a second,
   the player.  The same look stops the station if a file has started since."
  (let ((now (get-internal-real-time)))
    (when (>= (- now (panel-checked pn)) internal-time-units-per-second)
      (setf (panel-checked pn) now)
      (let ((tu (panel-tuner pn)))
        (when (and (panel-radio-was pn) (%ra-active-p tu) (%ra-file-playing-p))
          (radio-stop tu))
        (setf (panel-radio-was pn) (and (%ra-active-p tu) (not (%ra-file-playing-p)))))
      (let ((sig (%ra-signature pn)))
        (unless (equal sig (panel-shown pn))
          (setf (panel-shown pn) sig (panel-dirty pn) t)))))
  (when (panel-dirty pn)
    (setf (panel-dirty pn) nil)
    (radio-panel-redraw pn)))

(defun radio-panel-open (pn)
  (sb-thread:with-mutex ((tuner-lock (panel-tuner pn))) (incf (tuner-windows (panel-tuner pn))))
  pn)

(defun radio-panel-close (pn)
  (let* ((tu (panel-tuner pn))
         (left (sb-thread:with-mutex ((tuner-lock tu)) (setf (tuner-windows tu) (max 0 (1- (tuner-windows tu)))))))
    (when (zerop left) (%ra-teardown tu))))

;;; ---- Media: the Music, Stations, Pictures and Documents tabs ------------------------------------

(defparameter *media-tab-h* 36 "Media's tab strip: a fingertip tall.")
(defparameter *media-hidden-look* 0.5d0 "Seconds between looks at the tabs Media is not showing.")

(defparameter +media-tab-labels+ '((:music . "Music") (:stations . "Stations") (:pictures . "Pictures")
                                   (:documents . "Documents")))

(defparameter +media-lazy-tabs+
  '((:pictures "PICTURES-APP" "PIGMENT") (:documents "DOCUMENTS-APP" "FOLIO"))
  "The tabs made from another system, found by name: (KEY APP-FUNCTION PACKAGE-IT-NEEDS).  Each is
   made the first time it is shown.")

(defun %media-lazy-fn (key)
  "KEY's app function (glass/media-pictures' PICTURES-APP, glass/media-documents' DOCUMENTS-APP),
   found by name, or NIL when its system (or the decoder it brings) is not loaded -- then Media has
   no such tab."
  (destructuring-bind (name pkg) (cdr (assoc key +media-lazy-tabs+))
    (let ((s (find-symbol name "GLASS.DESK")))
      (and s (fboundp s) (find-package pkg) s))))

(defun %media-pictures-fn () (%media-lazy-fn :pictures))

(defun %media-tab-keys ()
  "The tabs this image's Media has: Music with warp's player, Stations always, Pictures with
   glass/media-pictures, Documents with glass/media-documents."
  (append (and (%warp-media-p) '(:music)) '(:stations)
          (loop for (key) in +media-lazy-tabs+ when (%media-lazy-fn key) collect key)))

(defvar *media-import* nil
  "A function of no arguments that brings files into the media folder from elsewhere -- on the
   phone, the system's document picker (iCloud Drive and the rest), whose picks are copied into
   *MEDIA-ROOT* -- or NIL.  When set, Media's tab strip ends in a square + that calls it.  Nothing
   is told when the files arrive: the Music, Pictures and Documents tabs re-read their folders.")

(defun %media-strip-share (w)
  "How much of a strip W wide the tabs share: all of it, or all but the + square at the right."
  (if *media-import* (max 1 (- w *media-tab-h*)) w))

(defun %media-import ()
  "Call *MEDIA-IMPORT*, if there is one; what it signals is not the tab strip's to handle."
  (let ((f *media-import*))
    (when f (handler-case (funcall f) (error () nil)))))

(defun %media-tab-at (keys w x)
  "The tab under X in a strip W wide: equal shares, the last taking what division leaves; :IMPORT
   on the + square when there is one."
  (let ((n (length keys)) (tw (%media-strip-share w)))
    (if (>= x tw)
        :import
        (nth (max 0 (min (1- n) (floor (max 0 x) (max 1 (floor tw n))))) keys))))

(defun %media-tabs (fb tab &optional (keys '(:music :stations)))
  "Media's tab strip across the top of FB, one equal share of the width per KEYS, TAB the one
   showing.  Four across a phone-narrow window are still each a fingertip target; a label too wide
   for its share is drawn a size smaller."
  (let* ((fw (glass:fb-width fb)) (w (%media-strip-share fw)) (h *media-tab-h*) (n (length keys))
         (share (floor w n)))
    (glass:fb-rect fb 0 0 fw h +ra-head+)
    (when (< w fw)                       ; the + square: two bars, a third of the square long
      (let* ((cx (+ w (floor (- fw w) 2))) (cy (floor h 2)) (r (floor h 6)))
        (glass:fb-rect fb w 6 1 (- h 12) +ra-bg+)
        (glass:fb-rect fb (- cx r) (1- cy) (1+ (* 2 r)) 2 +ra-fg+)
        (glass:fb-rect fb (1- cx) (- cy r) 2 (1+ (* 2 r)) +ra-fg+)))
    (loop for key in keys
          for i from 0
          for x = (* i share)
          for tw = (if (= i (1- n)) (- w x) share)
          for label = (cdr (assoc key +media-tab-labels+))
          for size = (if (<= (%ra-width label 13) (- tw 12)) 13 11)
          for on = (eq key tab)
          do (when on (glass:fb-rect fb x 0 tw h +ra-sel+))
             (let ((label (%ra-fit label (- tw 8) size)))
               (%ra-text fb (+ x (floor (- tw (%ra-width label size)) 2)) (%ra-baseline 0 (- h 3) size) label
                         :size size :color (if on +ra-fg+ +ra-dim+)))
             (when on (glass:fb-rect fb x (- h 3) tw 3 +ra-on+))
             (when (plusp i) (glass:fb-rect fb x 6 1 (- h 12) +ra-bg+)))))

(defun media-app (fb)
  "Media: tabs -- Music (warp's player over *MEDIA-ROOT*, which needs warp-media/glass loaded),
   Stations (media-radio.lisp: internet radio through spool, found by name), Pictures
   (glass/media-pictures, found by name) and Documents (glass/media-documents, found by name) -- the
   one showing given the whole window under the tab strip.  Each draws into a framebuffer of its own,
   kept painted while hidden, and the one showing is copied into FB; a press goes to whichever part
   it started in until it is released.  Pictures and Documents are made the first time they are
   shown: a window that never looks reads no folder and decodes nothing.  With only Stations there
   is nothing to choose between, so the window is the Stations alone.  RESIZABLE (a phone's rack
   fills a turned screen with it): every part's framebuffer follows, each lays itself out again.
   No TEXT-P."
  (let ((keys (%media-tab-keys)))
    (when (null (cdr keys))
      ;; NO FILE PLAYER, NO PICTURES, NO DOCUMENTS HERE: the window is the Stations, rather than a
      ;; pane saying what is missing -- radio does not need warp.
      (let ((panel (radio-panel-open (make-radio-panel fb :whole t))) (down nil))
        (return-from media-app
          (values nil
                  (lambda (mask x y)          ; on the press, not on every move while held
                    (let ((now (logbitp 0 mask)))
                      (when (and now (not down)) (radio-panel-press panel x y))
                      (setf down now)))
                  (lambda () (radio-panel-dirty-p panel))
                  nil
                  (lambda () (radio-panel-close panel))
                  (lambda (w h) (glass:fb-resize fb w h +ra-bg+) (radio-panel-resized panel :whole t))
                  nil))))
    (let* ((w (glass:fb-width fb)) (th *media-tab-h*)
           (bh (max 1 (- (glass:fb-height fb) th)))
           (music (and (member :music keys) (glass:make-framebuffer w bh #x0c0e12 (glass:fb-format fb))))
           (stations (glass:make-framebuffer w bh +ra-bg+ (glass:fb-format fb)))
           ;; the lazy tabs: KEY -> (FB . PART), PART (ON-KEY ON-POINTER DIRTY-P CLOSE-FN) once shown
           (lazy (loop for (key) in +media-lazy-tabs+
                       when (member key keys)
                         collect (cons key (cons (glass:make-framebuffer w bh +ra-bg+ (glass:fb-format fb)) nil))))
           (panel (radio-panel-open (make-radio-panel stations :whole t)))
           ;; a station already on is what this window opens on
           (tab (if (%ra-active-p *radio-tuner*) :stations (first keys)))
           (shown nil)                        ; the tab last copied into FB, NIL = draw it all
           (hidden-next 0)                    ; when the hidden tabs are next looked at
           (grab nil))
      (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn music-resize)
          (if music (%warp-media-window music) (values nil nil nil nil nil nil))
        (declare (ignore copy-p))          ; a move hint in a part's coordinates is not one in FB's
        (labels ((part (key)
                   (let ((cell (cdr (assoc key lazy))))
                     (or (cdr cell)
                         (setf (cdr cell) (multiple-value-bind (k p d c x r) (funcall (%media-lazy-fn key) (car cell))
                                            (declare (ignore c))
                                            (list k p d x r))))))
                 (part-fb (key)
                   (case key (:music music) (:stations stations) (t (cadr (assoc key lazy))))))
          (values (lambda (down keysym)
                    (case tab
                      (:music (when on-key (funcall on-key down keysym)))
                      (:stations nil)
                      (t (funcall (first (part tab)) down keysym))))
                  (lambda (mask x y)
                    (let ((to (or grab (if (< y th) :tabs tab))))
                      (when (and (logbitp 0 mask) (null grab))
                        (setf grab to)
                        (case to
                          (:tabs (let ((want (%media-tab-at keys w x)))
                                   (cond ((eq want :import) (%media-import))
                                         ((not (eq want tab)) (setf tab want shown nil)))))
                          (:stations (radio-panel-press panel x (- y th)))))
                      (case to
                        ((:tabs :stations) nil)
                        (:music (when on-pointer (funcall on-pointer mask x (max 0 (- y th)))))
                        (t (funcall (second (part to)) mask x (max 0 (- y th)))))
                      (when (zerop mask) (setf grab nil))))
                  (lambda ()
                    ;; every part stays painted, so a tab shows at once what it last showed -- but a
                    ;; HIDDEN part is looked at only every *MEDIA-HIDDEN-LOOK* seconds.  Music's look is
                    ;; warp's whole pass (its layout re-derived and diffed), about 1 ms on a Mac and
                    ;; several on a phone; made every frame behind Stations it held the main thread --
                    ;; the speaker's loop -- 500-900 ms of each second, and the radio broke up.
                    (let* ((now (get-internal-real-time))
                           (look (>= now hidden-next))
                           (changed (list (cons :music (and dirty-p (or look (eq tab :music))
                                                            (funcall dirty-p) t))
                                          (cons :stations (radio-panel-dirty-p panel)))))
                      (when look
                        (setf hidden-next (+ now (floor (* *media-hidden-look* internal-time-units-per-second)))))
                      (loop for (key . (nil . made)) in lazy
                            when (or (eq tab key) (and made look))
                              do (push (cons key (and (funcall (third (part key))) t)) changed))
                      (cond ((not (eq shown tab))
                             (%media-tabs fb tab keys)
                             (glass:fb-blit-clipped fb (part-fb tab) 0 th)
                             (setf shown tab)
                             t)
                            ((cdr (assoc tab changed)) (glass:fb-blit-clipped fb (part-fb tab) 0 th) t))))
                  nil
                  (lambda ()
                    (unwind-protect (when close-fn (funcall close-fn))
                      (unwind-protect
                           (loop for (nil . (nil . made)) in lazy
                                 when made do (ignore-errors (funcall (fourth made))))
                        (radio-panel-close panel))))
                  ;; RESIZE-FN: the window, then every part under the tab strip -- the ones not made
                  ;; yet just get the new size, and draw at it when first shown
                  (lambda (nw nh)
                    (let ((nbh (max 1 (- nh th))))
                      (glass:fb-resize fb nw nh +ra-bg+)
                      (setf w nw)
                      (when music
                        (if music-resize (funcall music-resize nw nbh) (glass:fb-resize music nw nbh #x0c0e12)))
                      (glass:fb-resize stations nw nbh +ra-bg+)
                      (radio-panel-resized panel :whole t)
                      (loop for (nil . (pfb . made)) in lazy
                            do (if (and made (fifth made))
                                   (funcall (fifth made) nw nbh)
                                   (glass:fb-resize pfb nw nbh +ra-bg+)))
                      (setf shown nil)))
                  nil))))))
