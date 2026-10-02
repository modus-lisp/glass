;;;; music-app.lisp — a music player, in the shape everybody already knows.
;;;;
;;;; A playlist, a transport, and a time.  Winamp's arrangement, because it was the right
;;;; one: the list is the thing you are working with, the transport is four controls that
;;;; never move, and the clock tells you where you are.  Nothing here is novel and that is
;;;; the point — a music player that needs explaining has got something wrong.
;;;;
;;;; IT DOES NOT DECODE ANYTHING, AND IT DOES NOT PLAY ANYTHING.  reed decodes (MP3, AAC and
;;;; Opus, all in Lisp, no FFI), spool's player turns that into a source thunk with a
;;;; position and a seek, and the session mixer plays it to whoever is listening — a viewer,
;;;; a VNC client, a WebRTC peer.  All three existed before this file.  What was missing was
;;;; a way to point them at a file on disk, which is SPOOL:PLAY-FILE, and a window.
;;;;
;;;; ITS OWN PLAYER, NOT SPOOL.GLASS'S.  That one is a singleton registered as "podcast",
;;;; and sharing it would mean starting a song stops your episode halfway through with no
;;;; way to tell why.  Two players, two sources, two rows in the Mixer — which is also the
;;;; honest picture: a podcast and an album are two things that can both be playing.
;;;;
;;;; Found by name, like the other optional windows, so a desktop built without spool still
;;;; builds this one and it says what is missing instead of not existing.

(defpackage #:glass-music
  (:use #:cl)
  (:export #:music-box #:run #:register #:*music-dir* #:*extensions*))

(in-package #:glass-music)

(defparameter *music-dir*
  (or (sb-ext:posix-getenv "GLASS_MUSIC")
      (namestring (merge-pathnames "Music/" (user-homedir-pathname))))
  "Where the music is.  GLASS_MUSIC, else ~/Music — read at USE and not at load, for the
reason every other path here is: a saved core is a snapshot of load time and the environment
is a runtime thing.")

(defparameter *extensions* '("mp3" "m4a" "aac" "opus" "oga" "ogg")
  "What reed can decode.  Listed rather than attempted, so a directory of photographs does
not become a playlist of errors — and named after the CODECS, so when reed grows one this is
the line that says so.")

(defun ui-font (&optional (size 13)) (clim:make-text-style :sans-serif :roman size))
(defun ui-bold (&optional (size 13)) (clim:make-text-style :sans-serif :bold size))

;;; ---- what spool gives us, by name -------------------------------------------

(defun %fn (name &optional (pkg "SPOOL"))
  (let ((s (and (find-package pkg) (find-symbol name pkg))))
    (and s (fboundp s) s)))

(defun %spool-p () (and (%fn "MAKE-PLAYER") (%fn "PLAY-FILE") t))

(defvar *player* nil)
(defvar *source-id* nil)
(defvar *lock* (sb-thread:make-mutex :name "glass-music"))

(defun %ensure-player (on-end)
  "The music player, on the mixer, made once.

   Registered as its own source so the Mixer window shows a podcast and an album as two
   things — which they are, and either of them being audible is a separate question."
  (sb-thread:with-mutex (*lock*)
    (unless *player*
      (let ((make (%fn "MAKE-PLAYER")))
        (when make (setf *player* (funcall make :on-end on-end)))))
    (when (and *player* (not *source-id*))
      (let ((src (%fn "PLAYER-SOURCE")))
        (when src
          (setf *source-id*
                (ignore-errors
                 (glass:mixer-add-source (glass:session-mixer)
                                         (funcall src *player*)
                                         :name "music" :gain 1.0d0 :ears nil))))))
    *player*))

(defun %state ()   (let ((f (%fn "PLAYER-STATE")))    (and *player* f (funcall f *player*))))
(defun %pos ()     (let ((f (%fn "PLAYER-POSITION"))) (and *player* f (ignore-errors (funcall f *player*)))))
(defun %dur ()     (let ((f (%fn "PLAYER-DURATION"))) (and *player* f (ignore-errors (funcall f *player*)))))
(defun %title ()   (let ((f (%fn "PLAYER-TITLE")))    (and *player* f (ignore-errors (funcall f *player*)))))
(defun %live-p ()  (let ((f (%fn "PLAYER-LIVE-P")))   (and *player* f (ignore-errors (funcall f *player*)))))
(defun %stream-title () (let ((f (%fn "PLAYER-STREAM-TITLE"))) (and *player* f (ignore-errors (funcall f *player*)))))
(defun %station ()      (let ((f (%fn "PLAYER-STATION")))      (and *player* f (ignore-errors (funcall f *player*)))))

;;; ---- stations ----------------------------------------------------------------

(defparameter *default-stations*
  '(("SomaFM Groove Salad"  . "http://ice1.somafm.com/groovesalad-128-mp3")
    ("SomaFM Drone Zone"    . "http://ice1.somafm.com/dronezone-128-mp3")
    ("SomaFM Secret Agent"  . "http://ice1.somafm.com/secretagent-128-mp3")
    ("Radio Paradise"       . "http://stream.radioparadise.com/mp3-128"))
  "What a fresh desktop offers before anyone has written a stations file.")

(defun stations-file ()
  (or (sb-ext:posix-getenv "GLASS_STATIONS")
      (namestring (merge-pathnames "stations.txt" (pathname *music-dir*)))))

(defun stations ()
  "The stations to show: `Name | http://…` lines from stations.txt in the music directory
(GLASS_STATIONS names another file), `#` for comments.  A missing or unreadable file is
the defaults, not an error — the file is how you add a station, not a requirement to have one.
Only http: URLs are taken; this player has no TLS, and a line it cannot play is better
absent than listed and failing."
  (let ((rows '()))
    (ignore-errors
     (with-open-file (in (stations-file) :external-format :utf-8)
       (loop for line = (read-line in nil) while line
             do (let* ((l (string-trim '(#\Space #\Tab #\Return) line))
                       (bar (position #\| l)))
                  (when (and bar (plusp (length l)) (char/= (char l 0) #\#))
                    (let ((name (string-trim " " (subseq l 0 bar)))
                          (url (string-trim " " (subseq l (1+ bar)))))
                      (when (and (plusp (length name))
                                 (> (length url) 7)
                                 (string-equal "http://" url :end2 7))
                        (push (cons name url) rows))))))))
    (if rows (nreverse rows) *default-stations*)))

;;; ---- the playlist ------------------------------------------------------------

(defun playable-p (path)
  (let ((type (pathname-type path)))
    (and type (member (string-downcase type) *extensions* :test #'string=) t)))

(defun tracks (&optional (dir *music-dir*))
  "Every playable file under DIR, sorted.

   ONE LEVEL, deliberately: a music directory is usually artist folders and walking the whole
   tree turns opening a window into a filesystem crawl.  Subdirectories are shown as places to
   go rather than flattened into the list."
  (let ((d (ignore-errors (directory (merge-pathnames "*.*" (pathname dir))))))
    (sort (remove-if-not #'playable-p (or d '()))
          #'string< :key (lambda (p) (string-downcase (file-namestring p))))))

(defun subdirs (&optional (dir *music-dir*))
  (let ((d (ignore-errors (directory (merge-pathnames "*/" (pathname dir))))))
    (sort (or d '()) #'string< :key (lambda (p) (string-downcase (namestring p))))))

(defun mmss (secs)
  (if (and secs (numberp secs) (>= secs 0))
      (multiple-value-bind (m s) (floor (round secs) 60) (format nil "~d:~2,'0d" m s))
      "--:--"))

;;; ---- the frame ---------------------------------------------------------------

(clim:define-presentation-type music-track ())
(clim:define-presentation-type music-place ())
(clim:define-presentation-type music-button ())
(clim:define-presentation-type music-station ())

(clim:define-application-frame music-box ()
  ((dir :initform *music-dir* :accessor app-dir)
   (list :initform '() :accessor app-list)
   (index :initform -1 :accessor app-index)
   (stations :initform '() :accessor app-stations)
   (tuned :initform nil :accessor app-tuned)
   (shown :initform nil :accessor app-shown)
   (note :initform nil :accessor app-note)
   (ticker :initform nil :accessor app-ticker))
  (:menu-bar nil)
  (:panes
   (transport :application :display-function 'draw-transport
              :scroll-bars nil :height 64 :text-style (ui-font)
              :background (clim:make-gray-color 0.94))
   (playlist :application :display-function 'draw-playlist
             :scroll-bars :vertical :text-style (ui-font)
             :background clim:+white+))
  (:layouts (default (clim:vertically () transport playlist))))

(defun draw-transport (frame stream)
  (let* ((st (%state)) (pos (%pos)) (dur (%dur)) (live (%live-p)))
    (clim:with-text-style (stream (ui-bold 14))
      (format stream "~&~a~%"
              (if live
                  (or (%stream-title) (%station) "—")
                  (or (%title frame) (%title) "—"))))
    (format stream "~&")
    ;; The four that never move.  A transport whose buttons appear and disappear is one you
    ;; have to look at before you can press.
    (dolist (b '((:prev . "|<") (:play . ">/||") (:next . ">|") (:stop . "[]")))
      (clim:with-output-as-presentation (stream (car b) 'music-button)
        (clim:with-text-style (stream (ui-bold 13))
          (format stream "  ~a  " (cdr b)))))
    ;; A live stream has no length and nowhere to seek, so it shows how long you have been
    ;; listening rather than a duration that would read "--:--" forever.
    (if live
        (format stream "   LIVE ~a" (mmss pos))
        (format stream "   ~a / ~a" (mmss pos) (mmss dur)))
    (format stream "   ~a"
            (case st
              (:playing "playing") (:paused "paused") (:loading "loading…")
              (:buffering "buffering…")
              (:ended "ended")     (:error (or (let ((f (%fn "PLAYER-ERROR")))
                                                 (and f *player* (funcall f *player*)))
                                               "error"))
              (t "stopped")))
    (when (and live (%stream-title) (%station))
      (clim:with-text-style (stream (ui-font 11)) (format stream "~%~a" (%station))))
    (when (app-note frame) (format stream "~%~a" (app-note frame)))))

(defun %title (&optional frame)
  (declare (ignore frame))
  (let ((f (%fn "PLAYER-TITLE"))) (and *player* f (ignore-errors (funcall f *player*)))))

(defun draw-playlist (frame stream)
  (cond
    ((not (%spool-p))
     (clim:with-text-style (stream (ui-bold 14)) (format stream "~&No player in this image.~%"))
     (format stream "~&This desktop was built without spool, which is what decodes and plays.~%"))
    (t
     (when (and (%fn "PLAY-URL") (app-stations frame))
       (clim:with-text-style (stream (ui-bold)) (format stream "~&Radio~%"))
       (dolist (st (app-stations frame))
         (clim:with-output-as-presentation (stream st 'music-station)
           (clim:with-text-style (stream (if (eq st (app-tuned frame)) (ui-bold) (ui-font)))
             (format stream "  ~a ~a~%" (if (eq st (app-tuned frame)) ">" " ") (car st)))))
       (terpri stream))
     (format stream "~&~a~%~%" (app-dir frame))
     (dolist (d (subdirs (app-dir frame)))
       (clim:with-output-as-presentation (stream d 'music-place)
         (format stream "  [ ~a ]~%" (car (last (pathname-directory d))))))
     (let ((ts (app-list frame)))
       (if (null ts)
           (format stream "~&  no ~{~a~^, ~} here.~%" *extensions*)
           (loop for tr in ts for i from 0
                 do (clim:with-output-as-presentation (stream tr 'music-track)
                      (clim:with-text-style (stream (if (= i (app-index frame)) (ui-bold) (ui-font)))
                        (format stream "  ~a ~a~%"
                                (if (= i (app-index frame)) ">" " ")
                                (pathname-name tr))))))))))

;;; ---- commands ------------------------------------------------------------------

(defun %advance (&rest ignore)
  "Called by the player when a track ends: the next one, or stop at the end of the list."
  (declare (ignore ignore))
  (let ((frame (find-if (lambda (f) (typep f 'music-box))
                        (list clim:*application-frame*))))
    (when frame (ignore-errors (clim:execute-frame-command frame '(com-next))))))

(defun %play-index (frame i)
  (let ((ts (app-list frame)))
    (when (and ts (< -1 i (length ts)))
      (setf (app-index frame) i (app-tuned frame) nil (app-note frame) nil)
      (let ((p (%ensure-player #'%advance)) (play (%fn "PLAY-FILE")))
        (if (and p play)
            (unless (ignore-errors (funcall play p (namestring (nth i ts))))
              (setf (app-note frame) "could not read that file"))
            (setf (app-note frame) "spool is not loaded"))))))

(define-music-box-command (com-play-track) ((tr 'music-track :gesture :select))
  (let ((frame clim:*application-frame*))
    (%play-index frame (or (position tr (app-list frame) :test #'equal) 0))))

(defun %tune (frame st)
  (setf (app-tuned frame) st (app-index frame) -1 (app-note frame) nil)
  (let ((p (%ensure-player #'%advance)) (play (%fn "PLAY-URL")))
    (if (and p play)
        (funcall play p (cdr st) :name (car st))
        (setf (app-note frame) "this spool cannot play streams"))))

(define-music-box-command (com-tune) ((st 'music-station :gesture :select))
  (%tune clim:*application-frame* st))

(define-music-box-command (com-enter-place) ((d 'music-place :gesture :select))
  (let ((frame clim:*application-frame*))
    (setf (app-dir frame) (namestring d)
          (app-list frame) (tracks d)
          (app-index frame) -1)))

(define-music-box-command (com-next) ()
  (let ((frame clim:*application-frame*))
    (%play-index frame (1+ (app-index frame)))))

(define-music-box-command (com-prev) ()
  (let ((frame clim:*application-frame*))
    (%play-index frame (max 0 (1- (app-index frame))))))

(define-music-box-command (com-toggle) ()
  (let ((f (%fn "TOGGLE")) (frame clim:*application-frame*))
    (cond ((and *player* f (member (%state) '(:playing :paused :buffering))) (funcall f *player*))
          ;; A station that dropped or was stopped: play means "tune it again", not "start
          ;; the file list at the top".
          ((app-tuned frame) (%tune frame (app-tuned frame)))
          ;; Nothing loaded yet, so the play button means "start at the top", which is what
          ;; pressing play on a fresh playlist has always meant.
          (t (%play-index frame (max 0 (app-index frame)))))))

(define-music-box-command (com-stop) ()
  (let ((f (%fn "STOP"))) (when (and *player* f) (funcall f *player*))))

(define-music-box-command (com-press) ((b 'music-button :gesture :select))
  (case b
    (:prev (com-prev)) (:next (com-next)) (:stop (com-stop)) (t (com-toggle))))

(define-music-box-command (com-tick :name nil) ()
  (let ((frame clim:*application-frame*))
    (setf (app-shown frame) (%status frame))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'transport) :force-p t)))

(defun %status (frame)
  (list (%state) (round (or (%pos) 0)) (app-index frame) (app-note frame)
        (and (%live-p) (%stream-title))))

(defun %start-ticker (frame)
  "Redraw the transport when the clock or the state would read differently — a second's
   resolution, because that is what the clock shows and repainting faster is bytes on every
   connection for a number that has not changed."
  (sb-thread:make-thread
   (lambda ()
     (sleep 0.4)
     (setf (app-list frame) (tracks (app-dir frame)))
     (loop
       (sleep 0.4)
       (unless (member (clim:frame-state frame) '(:enabled :shrunk)) (return))
       (handler-case
           (unless (equal (%status frame) (app-shown frame))
             (clim:execute-frame-command frame '(com-tick)))
         (error () (return)))))
   :name "glass-music-ticker"))

(defvar *open-frames* nil
  "The Music windows that are open.  The player is one per session, shared by all of them.
   A list of frames rather than a count, so a window that died without saying so is pruned by
   its state instead of holding the last-window teardown off forever.")

(defun %live-frames ()
  (setf *open-frames*
        (remove-if-not (lambda (f) (member (ignore-errors (clim:frame-state f)) '(:enabled :shrunk)))
                       *open-frames*)))

(defun %teardown ()
  "Silence the session: stop the player (and its radio thread) and take its source off the
   mixer.  Without this, closing the window leaves the player running with nobody to stop it."
  (sb-thread:with-mutex (*lock*)
    (let ((stop (%fn "STOP")))
      (when (and *player* stop) (ignore-errors (funcall stop *player*))))
    (when *source-id*
      (ignore-errors (glass:mixer-remove-source (glass:session-mixer :start nil) *source-id*))
      (setf *source-id* nil))))

(defun stop-music ()
  "Stop whatever the music player is doing, with or without a window.  For a control socket."
  (%teardown))

(defmethod clim:run-frame-top-level :around ((frame music-box) &key)
  (setf (app-list frame) (tracks (app-dir frame))
        (app-stations frame) (stations)
        (app-ticker frame) (%start-ticker frame))
  (sb-thread:with-mutex (*lock*) (pushnew frame *open-frames*))
  (unwind-protect (call-next-method)
    (ignore-errors (sb-thread:terminate-thread (app-ticker frame)))
    (when (sb-thread:with-mutex (*lock*)
            (setf *open-frames* (remove frame *open-frames*))
            (null (%live-frames)))
      (%teardown))))

(defun run (&key (width 520) (height 420))
  (clim:run-frame-top-level (clim:make-application-frame 'music-box :width width :height height)))

(defun register (&key (label "Music") (width 520) (height 420))
  "Put the player in the glass desktop's root menu."
  (let ((fn (and (find-package "CLIM-GLASS") (find-symbol "REGISTER-APP" "CLIM-GLASS"))))
    (when (and fn (fboundp fn))
      (funcall fn label (list 'music-box :width width :height height :title label))
      label)))
