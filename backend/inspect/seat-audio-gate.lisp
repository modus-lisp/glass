;;;; seat-audio-gate.lisp — two people, one session, and whose words go where.
;;;;
;;;; seat-gate.lisp holds two seats over one set of windows and reads PIXELS.  This holds the
;;;; same two seats and reads the other three things a seat owns that a screen cannot show:
;;;;
;;;;   * ITS KEYBOARD.  GLASS:*KEY-INJECTOR* is filled by SERVE, so before seats had audio the
;;;;     SECOND seat's listener — being the one that started last — owned the session's typing,
;;;;     and anything that typed without naming a seat landed in the newest person's window.
;;;;     The primary seat keeps it now, and every seat keeps its own besides.
;;;;   * ITS MICROPHONE.  Two seats, two ports, two headsets; each ear's source hands back the
;;;;     room its own seat is in, checked on the SAMPLES (one room is loud and one is quiet, and
;;;;     the levels are read off the frames the ear would have decoded).
;;;;   * ITS DICTATION.  An utterance finishing on seat B's ear is typed into the window SEAT B
;;;;     has focused, through seat B's keyboard, while seat A's focus and seat A's window are
;;;;     untouched — which is the claim the old single *KEY-INJECTOR* could not make.
;;;;
;;;; The recognizer is not what is under test here and is not loaded: an utterance is delivered
;;;; at the boundary stave would deliver it at (%HEARING-ANNOUNCE, which is what LISTENER-FINISH
;;;; feeds), so everything from the ear's listener list to the pixels' keyboard is the real path.
;;;; The mic -> ear half IS real, over real sockets, and is checked on samples.
;;;;
;;;; In-process; the RFB listeners are real (that is what fills a seat's injector) but nothing
;;;; ever connects to them.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --non-interactive --load backend/inspect/seat-audio-gate.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (asdf:load-system :glass/headset)
    (asdf:load-system :glass/hearing)
    (asdf:load-system :glass/dictation)
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(defvar *fail* 0)
(defun check (ok fmt &rest args)
  (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args)
  (finish-output)
  (unless ok (incf *fail*)))

(defun tone (hz rate frame amp)
  (let ((i 0))
    (lambda ()
      (let ((v (reed:make-pcm16 frame)))
        (dotimes (j frame)
          (setf (aref v j) (round (* amp (sin (/ (* 2 pi hz (+ i j)) rate))))))
        (incf i frame)
        v))))

(defun rms (pcm)
  (if (or (null pcm) (zerop (length pcm)))
      0d0
      (let ((s 0d0))
        (dotimes (i (length pcm)) (incf s (expt (float (aref pcm i) 1d0) 2)))
        (sqrt (/ s (length pcm))))))

;;; A surface that writes down what was typed into it: "who has the keyboard" with an answer
;;; on paper rather than an inference.  (Same trick as seat-gate's typist.)
(defun add-typist (port title)
  (let* ((typed (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
         (surf (add-surface port
                            (lambda (fb) (glass:fb-fill fb (glass:rgb 240 240 240))
                              (values (lambda (down k)
                                        (when (and down (<= 32 k 126))
                                          (vector-push-extend (code-char k) typed)))
                                      nil (constantly nil)))
                            :title title :width 300 :height 200)))
    (values surf (lambda () (coerce typed 'string)))))

;;; Seat ports: the audio pair follows from each (5941 -> 5951/5952, 5961 -> 5971/5972).
(defparameter +a-rfb+ 5941)
(defparameter +b-rfb+ 5961)

(defvar *port* (make-instance 'glass-port :port +a-rfb+))
(setf (glass-port-wm-p *port*) t)

(defvar *a* (glass-port-default-seat *port*))
(setf (seat-screen-w *a*) 800 (seat-screen-h *a*) 600
      (seat-fb *a*) (glass:make-framebuffer 800 600 +wm-teal+))
(defvar *b* (add-seat *port* :name "seat-B" :port-num +b-rfb+ :width 640 :height 480
                             :fb (glass:make-framebuffer 640 480 +wm-teal+)))

;;; The servers, in the order that used to break this: A first, B second.  If the second
;;; listener to start owns the session's keyboard, the check below fails.
(start-seat-server *a*)
(start-seat-server *b*)
(sleep 0.4)

(format t "~&~%[the keyboard each seat types on]~%")
(check (and (seat-injector *a*) (seat-injector *b*)) "each seat kept its own :ON-KEY")
(check (not (eq (seat-injector *a*) (seat-injector *b*))) "and they are not the same function")
(check (eq glass:*key-injector* (seat-injector *a*))
       "the SESSION's keyboard is the PRIMARY seat's, though B's listener started last")
(check (not (eq glass:*key-injector* (seat-injector *b*)))
       "a second person joining does not take the session's typing with them")

;;; ---- sound, per seat ---------------------------------------------------------

(format t "~%[each seat's sound]~%")
(defvar *ha* (start-seat-audio *port* :seat *a*))
(defvar *hb* (start-seat-audio *port* :seat *b*))
(check (and *ha* *hb*) "both seats have a headset")
(check (eq (glass:headset-mix *ha*) (glass:mixer-default-mix (glass:session-mixer)))
       "the primary seat's mix IS the session's — a one-seat desktop is unchanged")
(check (not (eq (glass:headset-mix *ha*) (glass:headset-mix *hb*)))
       "the second seat composites its own")
(check (and (= (glass:headset-audio-port *ha*) (glass:seat-audio-port +a-rfb+))
            (= (glass:headset-audio-port *hb*) (glass:seat-audio-port +b-rfb+)))
       "each on the audio port beside its screen's (:~d and :~d)"
       (glass:headset-audio-port *ha*) (glass:headset-audio-port *hb*))
(check (eq (glass:headset-injector *hb*) (seat-injector *b*))
       "and each headset holds ITS seat's keyboard, which is what dictation types on")

;;; A source on the session bus: content, once, heard by both.
(defvar *src* (glass:mixer-add-source (glass:session-mixer) (tone 440 48000 960 8000)
                                      :name "an-app"))

;;; ---- two microphones ---------------------------------------------------------

(format t "~%[two microphones, and no crossing]~%")
(defvar *sa* (glass:make-mic-sender :port (glass:headset-mic-port *ha*) :name "phone-a"))
(defvar *sb* (glass:make-mic-sender :port (glass:headset-mic-port *hb*) :name "phone-b"))
(sleep 0.8)

;; the ears — each seat's own, made the way a seat makes one.  No recognizer is installed, so
;; each ear's decode thread reports that and stops; what is under test is which MICROPHONE its
;; source hands back, and that is decided in the source thunk, per frame.
(defvar *ea* (glass:headset-listen *ha*))
(defvar *eb* (glass:headset-listen *hb*))
(check (and *ea* *eb*) "each seat has an ear")
(check (not (eq *ea* *eb*)) "two seats, two ears")
(check (eq *ea* glass:*session-ears*)
       "the primary seat's IS the session's ear — what the Listen window shows")
(check (eq (glass::ear-mic-stream *eb*) (glass:headset-mic *hb*))
       "and the second seat's ear listens on the second seat's microphone port")

;; two rooms, told apart by level: A's is quiet, B's is loud
(let ((quiet (tone 300 8000 160 900))
      (loud  (tone 300 8000 160 12000)))
  (dotimes (i 50)
    (glass:mic-send *sa* (funcall quiet))
    (glass:mic-send *sb* (funcall loud))
    (sleep 0.02)))
(sleep 0.3)

(let ((la 0d0) (lb 0d0) (na 0) (nb 0))
  (dotimes (i 40)
    (let ((fa (funcall (glass::ear-source *ea*)))
          (fb (funcall (glass::ear-source *eb*))))
      (when (and fa (plusp (rms fa))) (incf la (rms fa)) (incf na))
      (when (and fb (plusp (rms fb))) (incf lb (rms fb)) (incf nb))))
  (let ((qa (/ la (max 1 na))) (qb (/ lb (max 1 nb))))
    (check (and (plusp na) (plusp nb)) "each ear is being fed (~d and ~d frames)" na nb)
    (check (eq (glass::ear-listening-to *ea*) :peer)
           "seat A's ear is listening to a microphone and not to the mix")
    (check (eq (glass::ear-listening-to *eb*) :peer) "and so is seat B's")
    (check (> qb (* 4 qa))
           "and the samples are each seat's OWN room (A ~,0f, B ~,0f)" qa qb)))

;;; ---- two windows, two focuses ------------------------------------------------

(format t "~%[dictation goes to the window ITS seat has focused]~%")
(multiple-value-bind (surf-a read-a) (add-typist *port* "A's window")
  (multiple-value-bind (surf-b read-b) (add-typist *port* "B's window")
    (setf (seat-focus-surface *a*) surf-a
          (seat-focus-surface *b*) surf-b)
    (check (not (eq (seat-focus-surface *a*) (seat-focus-surface *b*)))
           "the two seats have focused two different windows")

    ;; dictation on for both people, each through their own headset
    (let ((da (glass:headset-dictate *ha*))
          (db (glass:headset-dictate *hb*)))
      (check (and da db) "both seats are dictating")
      (check (not (eq da db)) "and each has its own switch")
      (check (eq da glass:*session-dictation*)
             "the primary seat's IS the session's — the Listen window's switch")
      (check glass:*dictating* "so the session reads as dictating")

      ;; an utterance FINISHES on each ear, exactly where stave delivers one
      (setf glass::*dictation-quiet-until* 0
            (glass::dict-quiet-until da) 0
            (glass::dict-quiet-until db) 0)
      (glass::%hearing-announce *eb* "HELLO FROM SEAT B")
      (sleep 1.5)
      (check (search "Hello from seat b" (funcall read-b))
             "B's window has what B said: ~s" (funcall read-b))
      (check (zerop (length (funcall read-a)))
             "A's window has nothing at all — it was not A who spoke (~s)" (funcall read-a))
      (check (eq (seat-focus-surface *a*) surf-a) "and A's focus was never touched")

      (glass::%hearing-announce *ea* "AND NOW SEAT A")
      (sleep 1.5)
      (check (search "And now seat a" (funcall read-a))
             "A's window has what A said: ~s" (funcall read-a))
      (check (not (search "And now seat a" (funcall read-b)))
             "and B's window did not get a second copy of it")
      (check (= 1 (glass::dict-typed da) (glass::dict-typed db))
             "each seat typed exactly one utterance (~d and ~d)"
             (glass::dict-typed da) (glass::dict-typed db))

      ;; the session counters follow the session's dictation and not the other person's
      (check (equal glass:*dictation-last* (glass::dict-last da))
             "the session's counters are the primary seat's (~s)" glass:*dictation-last*)
      (check (not (equal glass:*dictation-last* (glass::dict-last db)))
             "and not the second seat's")

      (format t "~%~a~%~a~%" (glass:dictation-report da) (glass:dictation-report db)))))

;;; ---- put it all back ---------------------------------------------------------

(glass:mic-sender-stop *sa*) (glass:mic-sender-stop *sb*)
(sleep 0.2)
(glass:stop-listening *ea*) (glass:stop-listening *eb*)
(stop-seat-audio *port* *a*) (stop-seat-audio *port* *b*)
(glass:mixer-remove-source (glass:session-mixer) *src*)
(glass:mixer-stop (glass:session-mixer))

(format t "~&~%~:[~d check~:p FAILED~;all checks passed~]~%" (zerop *fail*) *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
