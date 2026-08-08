;;;; inspect/headset-gate.lisp — two people, one session, and the sound each of them gets.
;;;;
;;;; The mixer already had the claims a session with two LISTENERS makes good on (audio-gate).
;;;; This is the claims a session with two PEOPLE makes good on, which is a different axis and
;;;; the one seats introduced:
;;;;
;;;;   * one source is PULLED ONCE however many people are listening.  A source is a destructive
;;;;     pull, not a framebuffer: give each seat its own mixer over the same sources and both of
;;;;     them get alternate frames of the podcast — double speed, half missing, and it sounds
;;;;     exactly like a network problem.  The counter is checked against the clock, because that
;;;;     is the failure this whole design exists to make unwritable.
;;;;   * with nobody muting anything, two seats' mixes are SAMPLE-IDENTICAL.  A second seat must
;;;;     be a second composite of the same content and not a second, drifting rendering of it.
;;;;   * one seat muting a source silences it FOR THAT SEAT ONLY — checked as exact zeros on one
;;;;     socket while the other socket carries the same sound it was carrying.
;;;;   * a gain is a gain: a quarter is a quarter, measured.
;;;;   * a source may be ADDRESSED (its audience), which is what saying one person's selection
;;;;     out loud needs.
;;;;   * two microphones do not meet.  Each seat's arrives on its own port and reaches its own
;;;;     headset; neither is in any mix, so neither is played back at the person who said it.
;;;;   * a one-seat desktop is unchanged: the session's mix on 5913 and its microphone on 5914,
;;;;     the same objects, reached by the same calls — which is what the live WebRTC gateway is
;;;;     connected to and must not notice.
;;;;
;;;; Everything crosses a REAL SOCKET, through the same GLASS:MAKE-AUDIO-TAP and
;;;; GLASS:MAKE-MIC-SENDER the WebRTC gateway uses, and every claim is checked on SAMPLES.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load inspect/headset-gate.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass/headset)))

(defpackage #:glass-headset-gate (:use #:cl)) (in-package #:glass-headset-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (progn (incf *pass*) (format t "  ok   ~a = ~s~%" name got))
      (progn (incf *fail*) (format t "  FAIL ~a: got ~s, want ~s~%" name got want)))
  (finish-output))
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

;;; ---- sound to look at --------------------------------------------------------

(defun tone-source (hz rate frame &key (amp 8000))
  "An endless sine, phase-continuous across frames — a source that never ends, like an app."
  (let ((i 0))
    (lambda ()
      (let ((v (reed:make-pcm16 frame)))
        (dotimes (j frame)
          (setf (aref v j) (round (* amp (sin (/ (* 2 pi hz (+ i j)) rate))))))
        (incf i frame)
        v))))

(defun rms (pcm)
  (if (zerop (length pcm))
      0d0
      (let ((s 0d0))
        (dotimes (i (length pcm)) (incf s (expt (float (aref pcm i) 1d0) 2)))
        (sqrt (/ s (length pcm))))))

(defun tap-listen (tap secs)
  "Everything TAP hands out over SECS, as one vector.  The tap never blocks, so this is what a
sender's clock would have collected."
  (let ((out '()) (deadline (+ (get-internal-real-time)
                               (round (* secs internal-time-units-per-second)))))
    (loop while (< (get-internal-real-time) deadline)
          do (let ((f (glass:tap-next-frame tap)))
               (if f (push f out) (sleep 0.005))))
    (let* ((frames (nreverse out))
           (total (reduce #'+ frames :key #'length :initial-value 0))
           (all (reed:make-pcm16 total))
           (at 0))
      (dolist (f frames all) (replace all f :start1 at) (incf at (length f))))))

(defun all-zero-p (pcm) (every #'zerop pcm))

;;; The RFB ports these seats would be watching on; the audio ports follow from them, which is
;;; the point (5950 -> 5960 out / 5961 in).  Deliberately nowhere near a live desktop's.
(defparameter +seat-a-rfb+ 5950)
(defparameter +seat-b-rfb+ 5970)

(format t "~&~%-- the ports a seat's sound is on --~%")
(check "audio is the screen port plus ten" (glass:seat-audio-port 5903) 5913)
(check "the microphone is one past that"   (glass:seat-mic-port 5903) 5914)
(check-that "which is exactly what a one-seat desktop has always served"
            (and (= (glass:seat-audio-port 5903) glass:*audio-stream-port*)
                 (= (glass:seat-mic-port 5903) glass:*mic-stream-port*))
            "so the WebRTC gateway's GLASS_AUDIO_PORT default still finds it")

;;; ---- the one-seat desktop, exactly as its startup script starts it ------------
;;;
;;; warren/desktop-5903.lisp says, and says only, this:
;;;
;;;   (glass:start-session-audio :port 5913 :address "127.0.0.1" :file nil)
;;;   (glass:start-session-mic   :port 5914 :address "127.0.0.1")
;;;
;;; and an application that makes noise (spool's podcast) says exactly this:
;;;
;;;   (glass:mixer-add-source (glass:session-mixer) thunk :name "podcast")
;;;
;;; So that is what this checks, on the ports it would use if the live desktop were not on
;;; them.  It is the whole of the backward-compatibility claim: same calls, same session
;;; mixer, same wire, and the same GLASS:MAKE-AUDIO-TAP / GLASS:MAKE-MIC-SENDER the WebRTC
;;; gateway holds — which is a separate process running an OLDER glass and will not be
;;; reloaded, so the protocol is what has to be identical and not the API.

(format t "~&~%-- a one-seat desktop, started the way the startup script starts one --~%")
(defparameter +one-seat-audio+ 5993)
(defparameter +one-seat-mic+ 5994)

(defvar *session-out* (glass:start-session-audio :port +one-seat-audio+ :address "127.0.0.1"
                                                 :file nil))
(defvar *session-in* (glass:start-session-mic :port +one-seat-mic+ :address "127.0.0.1"))
(check-that "the session's audio came up" (and *session-out* *session-in*))
(check-that "on the session's own mixer, with one mix in it"
            (and (eq (glass:audio-stream-mix *session-out*)
                     (glass:mixer-default-mix (glass:session-mixer)))
                 (= 1 (length (glass:mixer-mixes (glass:session-mixer)))))
            "a desktop with one person has exactly one composite")

;; an application makes noise — the one call spool makes
(defvar *podcast* (glass:mixer-add-source (glass:session-mixer) (tone-source 440 48000 960)
                                          :name "podcast"))
(defvar *one-tap* (glass:make-audio-tap :port +one-seat-audio+ :name "gateway"))
(defvar *one-mic* (glass:make-mic-sender :port +one-seat-mic+ :name "gateway"))
(sleep 1.2)
(check-that "a listener dialling in is connected" (glass:audio-tap-connected *one-tap*)
            (glass:tap-report *one-tap*))
(let ((pcm (tap-listen *one-tap* 0.8)))
  (check-that "and hears what the application is playing" (> (rms pcm) 100)
              (format nil "~d samples, rms ~,0f" (length pcm) (rms pcm))))

(let ((room (funcall (tone-source 300 8000 160 :amp 9000))))
  (dotimes (i 30) (glass:mic-send *one-mic* room) (sleep 0.02)))
(sleep 0.3)
(check-that "a microphone arriving on the session port is the SESSION's microphone"
            (and (glass:session-mic) (glass:mic-live-p (glass:session-mic)))
            (glass:mic-report (glass:session-mic)))
(check-that "which is what an ear with no seat to ask about listens to"
            (let ((mic (glass:session-mic)))
              (and mic (> (rms (glass:mic-next-frame mic)) 100))))

;; and a headset for the one person here ADOPTS all of that rather than opening a second one
(defvar *only* (glass:make-headset :name "the-only-seat" :rfb-port 5983
                                   :primary t :mixer (glass:session-mixer)))
(check-that "a primary seat's headset adopts the session's ports, it does not race them"
            (and (eq (glass:headset-audio *only*) *session-out*)
                 (eq (glass:headset-mic *only*) *session-in*))
            "so starting one on a running desktop cannot take the gateway's socket away")
(check-that "and its mix is the session's" (eq (glass:headset-mix *only*)
                                               (glass:mixer-default-mix (glass:session-mixer))))
(glass:stop-headset *only*)
(check-that "taking that seat away leaves the desktop's own sound running"
            (and (glass:audio-stream-running *session-out*)
                 (glass:audio-tap-connected *one-tap*))
            "the adopted ports are not a seat's to close")

(glass:tap-stop *one-tap*)
(glass:mic-sender-stop *one-mic*)
(sleep 0.2)
(glass:stop-audio-stream *session-out*)
(glass:stop-mic-stream *session-in*)
(glass:mixer-remove-source (glass:session-mixer) *podcast*)
(glass:mixer-stop (glass:session-mixer))

;;; ---- the session, and two people on it ---------------------------------------

(defvar *m* (glass:make-mixer))
(defvar *tone* (glass:mixer-add-source *m* (tone-source 440 48000 960) :name "podcast"))
(defvar *bell* (glass:mixer-add-source *m* (tone-source 1000 48000 960 :amp 6000) :name "voice"))
(glass:mixer-start *m*)

(defvar *a* (glass:make-headset :name "seat-a" :rfb-port +seat-a-rfb+ :mixer *m* :primary t))
(defvar *b* (glass:make-headset :name "seat-b" :rfb-port +seat-b-rfb+ :mixer *m*))

(format t "~&~%-- one session, two headsets --~%")
(check-that "the primary seat's mix IS the session's"
            (eq (glass:headset-mix *a*) (glass:mixer-default-mix *m*))
            "a one-seat desktop has one mix and it is this one")
(check-that "the second seat's is its own"
            (not (eq (glass:headset-mix *b*) (glass:headset-mix *a*))))
(check "and both are composited on the one bus" (length (glass:mixer-mixes *m*)) 2)
(check-that "each serves its own port"
            (and (= (glass:headset-audio-port *a*) (glass:seat-audio-port +seat-a-rfb+))
                 (= (glass:headset-audio-port *b*) (glass:seat-audio-port +seat-b-rfb+)))
            (format nil ":~d and :~d"
                    (glass:headset-audio-port *a*) (glass:headset-audio-port *b*)))

;;; ---- ONE PULL, however many people ------------------------------------------
;;;
;;; The crux.  If a seat's mix were a seat's MIXER, this number would be two per frame and both
;;; seats would be hearing half a podcast each.

(format t "~&~%-- one pull per frame, not one per listener --~%")
(let ((seq0 (glass:mix-seq (glass:headset-mix *a*)))
      (pulled0 (glass:src-frames *tone*)))
  (sleep 1.0)
  (let ((frames (- (glass:mix-seq (glass:headset-mix *a*)) seq0))
        (pulls (- (glass:src-frames *tone*) pulled0)))
    (check-that "the source was asked exactly once per composited frame"
                (= pulls frames)
                (format nil "~d pulls for ~d frames, with 2 people listening" pulls frames))
    (check-that "and both mixes advanced together on the one clock"
                (< (abs (- (glass:mix-seq (glass:headset-mix *a*))
                           (glass:mix-seq (glass:headset-mix *b*))))
                   2))))

;;; ---- sample-identical until somebody says otherwise --------------------------

(format t "~&~%-- two seats, nothing muted: the same samples --~%")
(let ((sa (glass:mixer-subscribe (glass:headset-mix *a*) :name "probe-a" :rate 48000 :lead 0))
      (sb (glass:mixer-subscribe (glass:headset-mix *b*) :name "probe-b" :rate 48000 :lead 0)))
  (sleep 0.3)
  (let ((same 0) (differ 0))
    (dotimes (i 8)
      (let ((fa (glass:sink-next-frame sa)) (fb (glass:sink-next-frame sb)))
        (when (and fa fb) (if (equalp fa fb) (incf same) (incf differ))))
      (sleep 0.02))
    (check-that "every frame is identical, sample for sample"
                (and (plusp same) (zerop differ))
                (format nil "~d identical, ~d different" same differ)))
  ;; and one seat's opinion does not reach the other's samples
  (glass:mix-mute (glass:headset-mix *b*) *tone*)
  (glass:mix-mute (glass:headset-mix *b*) *bell*)
  (sleep 0.2)
  (let ((za 0) (zb 0))
    (dotimes (i 8)
      (let ((fa (glass:sink-next-frame sa)) (fb (glass:sink-next-frame sb)))
        (when (and fa fb)
          (when (all-zero-p fa) (incf za))
          (when (all-zero-p fb) (incf zb))))
      (sleep 0.02))
    (check-that "muted for B is digital silence for B" (plusp zb) (format nil "~d silent frames" zb))
    (check-that "and A hears everything it was hearing" (zerop za)))
  (glass:mix-unmute (glass:headset-mix *b*) *tone*)
  (glass:mix-unmute (glass:headset-mix *b*) *bell*)
  (glass:sink-unsubscribe sa)
  (glass:sink-unsubscribe sb))

;;; ---- over the real sockets ---------------------------------------------------
;;;
;;; The same claims again, this time through the transport a WebRTC gateway uses: one connection
;;; per person, a MIXER-SUBSCRIBE behind it, 8 kHz on the wire.

(format t "~&~%-- and over the wire, one connection per person --~%")
(defvar *ta* (glass:make-audio-tap :port (glass:headset-audio-port *a*) :name "phone-a"))
(defvar *tb* (glass:make-audio-tap :port (glass:headset-audio-port *b*) :name "phone-b"))
(sleep 1.0)
(check-that "both listeners are connected"
            (and (glass:audio-tap-connected *ta*) (glass:audio-tap-connected *tb*))
            (format nil "~a | ~a" (glass:tap-report *ta*) (glass:tap-report *tb*)))

(let* ((pa (tap-listen *ta* 0.8)) (pb (tap-listen *tb* 0.8))
       (ra (rms pa)) (rb (rms pb)))
  (check-that "both are carrying the session's sound" (and (> ra 100) (> rb 100))
              (format nil "rms A ~,0f over ~d samples, B ~,0f over ~d" ra (length pa) rb (length pb)))
  (check-that "at the same level, because it is the same mix"
              (< (abs (- ra rb)) (* 0.05 (max ra rb)))
              (format nil "~,0f vs ~,0f" ra rb)))

(format t "~&~%-- B turns the podcast down; A is not consulted --~%")
(setf (glass:mix-source-gain (glass:headset-mix *b*) *tone*) 0.25d0)
(sleep 0.3)
(let* ((pa (tap-listen *ta* 0.8)) (pb (tap-listen *tb* 0.8))
       (ra (rms pa)) (rb (rms pb)))
  (check-that "B's stream got quieter" (< rb (* 0.9 ra)) (format nil "rms A ~,0f  B ~,0f" ra rb))
  (check-that "A's did not" (> ra 100)))

(format t "~&~%-- B mutes it entirely --~%")
(glass:mix-mute (glass:headset-mix *b*) *tone*)
(glass:mix-mute (glass:headset-mix *b*) *bell*)
(sleep 0.4)
(let* ((pa (tap-listen *ta* 0.8)) (pb (tap-listen *tb* 0.8)))
  (check-that "B's socket carries exact zeros" (and (plusp (length pb)) (all-zero-p pb))
              (format nil "~d samples, all zero" (length pb)))
  (check-that "A's socket carries the sound it always did" (> (rms pa) 100)
              (format nil "rms ~,0f" (rms pa)))
  (check-that "and the stream never stopped — silence is frames, not a pause"
              (> (length pb) 3000)
              (format nil "~d samples of it" (length pb))))
(glass:mix-unmute (glass:headset-mix *b*) *tone*)
(glass:mix-unmute (glass:headset-mix *b*) *bell*)

;;; ---- a sound addressed to one person -----------------------------------------

(format t "~&~%-- a source with an audience: said to B, not to the room --~%")
(glass:mix-mute (glass:headset-mix *b*) *tone*)          ; leave only the addressed sound in B
(setf (glass:src-audience *bell*) (list (glass:headset-mix *b*)))
(sleep 0.4)
(let* ((pa (tap-listen *ta* 0.8)) (pb (tap-listen *tb* 0.8)))
  (check-that "B hears it" (> (rms pb) 100) (format nil "rms ~,0f" (rms pb)))
  (check-that "A hears only what was never addressed" (> (rms pa) 100))
  ;; A's mix is now the podcast alone; B's is the addressed sound alone.  Different sounds on
  ;; two sockets from one bus is the whole claim.
  (check-that "and the two sockets are carrying different sound"
              (> (abs (- (rms pa) (rms pb))) 1)
              (format nil "A ~,0f  B ~,0f" (rms pa) (rms pb))))
(setf (glass:src-audience *bell*) nil)
(glass:mix-unmute (glass:headset-mix *b*) *tone*)

;;; ---- two microphones, and no crossing ----------------------------------------

(format t "~&~%-- my microphone is mine --~%")
(check-that "each seat listens on its own port"
            (/= (glass:headset-mic-port *a*) (glass:headset-mic-port *b*))
            (format nil ":~d and :~d" (glass:headset-mic-port *a*) (glass:headset-mic-port *b*)))
(check-that "and the primary seat's is the SESSION's, so an ear with no seat to ask finds it"
            (eq (glass:headset-mic *a*) glass:*session-mic-stream*))

(defvar *sa* (glass:make-mic-sender :port (glass:headset-mic-port *a*) :name "phone-a"))
(defvar *sb* (glass:make-mic-sender :port (glass:headset-mic-port *b*) :name "phone-b"))
(sleep 0.8)
;; two rooms, told apart by how loud they are: A is quiet, B is loud
(let ((quiet (funcall (tone-source 300 8000 160 :amp 1000)))
      (loud  (funcall (tone-source 300 8000 160 :amp 12000))))
  (dotimes (i 40)
    (glass:mic-send *sa* quiet)
    (glass:mic-send *sb* loud)
    (sleep 0.02)))
(sleep 0.3)
(let ((ma (glass:stream-mic (glass:headset-mic *a*)))
      (mb (glass:stream-mic (glass:headset-mic *b*))))
  (check-that "both microphones arrived" (and ma mb)
              (format nil "~a | ~a" (glass:mic-report ma) (glass:mic-report mb)))
  (check-that "they are two different microphones" (not (eq ma mb)))
  (check-that "and each is live" (and (glass:mic-live-p ma) (glass:mic-live-p mb)))
  ;; the samples: A's room is quiet and B's is loud, on their own ports, with nothing shared
  (let ((ra 0d0) (rb 0d0) (na 0) (nb 0))
    (dotimes (i 30)
      (let ((fa (glass:mic-next-frame ma)) (fb (glass:mic-next-frame mb)))
        (when fa (incf ra (rms fa)) (incf na))
        (when fb (incf rb (rms fb)) (incf nb))))
    (let ((la (/ ra (max 1 na))) (lb (/ rb (max 1 nb))))
      (check-that "each headset hears its own room and not the other's"
                  (and (> lb (* 4 la)) (> la 1))
                  (format nil "A's room rms ~,0f over ~d frames, B's ~,0f over ~d" la na lb nb)))))

(format t "~&~%-- and no microphone is in anybody's mix --~%")
(check-that "the bus carries only what the session PLAYS"
            (= 2 (length (glass:mixer-sources *m*)))
            (format nil "sources: ~{~a~^ ~}"
                    (mapcar #'glass:src-name (glass:mixer-sources *m*))))

;;; ---- the report a control socket reads ---------------------------------------

(format t "~&~%~a~%" (glass:mixer-report *m*))
(format t "~&~a~%" (glass:headset-report *b*))

;;; ---- put everything back -----------------------------------------------------

(glass:tap-stop *ta*) (glass:tap-stop *tb*)
(glass:mic-sender-stop *sa*) (glass:mic-sender-stop *sb*)
(sleep 0.3)
(glass:stop-headset *a*)
(glass:stop-headset *b*)
(glass:mixer-stop *m*)

(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
