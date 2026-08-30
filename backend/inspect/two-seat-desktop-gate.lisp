;;;; two-seat-desktop-gate.lisp — ONE REAL DESKTOP, TWO PEOPLE AT IT.
;;;;
;;;; Multi-seat was proved in isolated gates and never as a desktop.  seat-gate.lisp builds
;;;; a GLASS-PORT by hand and drives it with no event loop and no wires; seat-transport-gate
;;;; runs a real session but only ever has one seat; clim-handoff-gate and popup-attach-gate
;;;; hold two seats over one CLIM frame and ask about McCLIM's single-valued geometry.  Each
;;;; of them is right about its own question and none of them stands a desktop up.
;;;;
;;;; This one does: RUN-WM, in a thread, with its own compositing loop ticking at 60Hz, a
;;;; real terminal with a real shell in it, a real RFB listener, and a second person joining
;;;; it the way a second person actually joins one — ADD-WM-SEAT against the running port.
;;;; Everything below is then asked of THAT, while it runs.
;;;;
;;;; THE CONTRACT IT HOLDS TO (backend/seat.lisp, docs/seats-and-transports.md):
;;;;
;;;;   SHARED — the applications, each window's CONTENT framebuffer, and each window's SIZE.
;;;;   PER SEAT — the screen and its size, position, z-order, focus, cursor, keyboard and
;;;;   pointer state, the open menu, the clipboard, the mix.
;;;;
;;;; WHERE THE CLAIM IS VISUAL IT IS ASKED OF PIXELS, and mostly of a DIGEST of them: "seat
;;;; A's screen did not change" is a statement about 558,000 pixels and the only honest way
;;;; to make it is to hash them before and after.  Two seats agreeing about a data structure
;;;; while disagreeing about what is on the screen is precisely the bug this design can have,
;;;; and a model-vs-model comparison structurally cannot see it.
;;;;
;;;; NOTHING HERE TOUCHES A LIVE DESKTOP.  Its ports are chosen by asking the kernel which
;;;; are free, its socket files go in a runtime directory under /tmp, its seats are told not
;;;; to mint identities, and the home directory's key stores are checked at the end.  What it
;;;; serves, it serves on 127.0.0.1.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --non-interactive --load backend/inspect/two-seat-desktop-gate.lisp
;;;; With a directory argument it also writes a PNG of each seat's screen per scene:
;;;;   ... --load backend/inspect/two-seat-desktop-gate.lisp -- /tmp/two-seat

(require :asdf)
(require :sb-posix)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
    (ignore-errors (ql:quickload :zpng))
    ;; …relative to THIS FILE, not to the container it was written in: a gate that names
    ;; /home/claude only runs where it was born, and that is how a client-side regression
    ;; once stayed invisible to the very gate that tests for it.
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)))
(in-package :clim-glass)

(defvar *fail* 0)
(defun check (ok fmt &rest args)
  (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args)
  (finish-output)
  (unless ok (incf *fail*)))
(defun banner (s) (format t "~&~%== ~a ==~%" s) (finish-output))
(defun note (fmt &rest args) (format t "     ~?~%" fmt args) (finish-output))

(defun sh (cmd)
  (with-output-to-string (o)
    (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output o :search nil :error nil)))

;;; ---- pixels ------------------------------------------------------------------
;;; A digest, because the claims are about whole screens.  FNV-1a over the pixel words:
;;; it is not a cryptographic hash and does not need to be — nobody here is adversarial,
;;; and what is wanted is a number that changes when any pixel does.

(defun rect-digest (fb &optional x y w h)
  "FNV-1a of FB's pixels, over the whole screen or just the (X,Y,W,H) rectangle of it."
  (let* ((fw (glass:fb-width fb)) (fh (glass:fb-height fb)) (px (glass:fb-pixels fb))
         (x (or x 0)) (y (or y 0)) (w (or w fw)) (h (or h fh))
         (hash 14695981039346656037))
    (loop for row from y below (min fh (+ y h)) do
      (loop for col from x below (min fw (+ x w)) do
        (setf hash (ldb (byte 64 0)
                        (* (logxor hash (aref px (+ (* row fw) col))) 1099511628211)))))
    hash))

(defun digest (fb) (rect-digest fb))

(defun pixel-at (fb x y)
  (if (and (< -1 x (glass:fb-width fb)) (< -1 y (glass:fb-height fb)))
      (logand (aref (glass:fb-pixels fb) (+ (* y (glass:fb-width fb)) x)) #xffffff)
      :off-screen))

(defun count-workspace (fb)
  "How many of FB's pixels are bare workspace.  A menu, a window or a wireframe is
   whatever it is; what matters is that it is not the teal."
  (let ((n 0) (px (glass:fb-pixels fb)))
    (dotimes (i (length px) n)
      (when (= (logand (aref px i) #xffffff) (logand +wm-teal+ #xffffff)) (incf n)))))

(defun quiescent-p (fb &optional (settle 0.4))
  "Is FB still?  Digested twice across SETTLE seconds — the desktop's tick loop is running
   throughout this suite, so `nothing changed' has to mean nothing changed WHILE TIME PASSED
   and not merely between two adjacent statements."
  (let ((before (digest fb)))
    (sleep settle)
    (= before (digest fb))))

;;; ---- PNGs, when somebody wants to look ---------------------------------------

(defparameter *png-dir* (first (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
(defvar *scene* 0)

(defun save-png (fb path)
  (let ((png-class (and (find-package '#:zpng) (find-symbol "PNG" '#:zpng))))
    (when png-class
      (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (px (glass:fb-pixels fb))
             (png (make-instance png-class :width w :height h :color-type :truecolor))
             (d (funcall (find-symbol "DATA-ARRAY" '#:zpng) png)))
        (dotimes (y h)
          (dotimes (x w)
            (let ((p (aref px (+ (* y w) x))))
              (setf (aref d y x 0) (ldb (byte 8 16) p)
                    (aref d y x 1) (ldb (byte 8 8) p)
                    (aref d y x 2) (ldb (byte 8 0) p)))))
        (funcall (find-symbol "WRITE-PNG" '#:zpng) png path)))))

(defun shoot (label &rest seats)
  (when *png-dir*
    (ensure-directories-exist (format nil "~a/" *png-dir*))
    (incf *scene*)
    (dolist (s seats)
      (save-png (seat-fb s) (format nil "~a/~2,'0d-~a-~a.png" *png-dir* *scene* label (seat-name s))))
    (note "-> ~a/~2,'0d-~a-*.png" *png-dir* *scene* label)))

;;; ---- asking the operating system ---------------------------------------------

(defun listening-on (address port)
  "Does `ss -ltn' show something listening on exactly ADDRESS:PORT?  Asked of the kernel,
   because `which interface did we bind' is the question this suite exists to answer
   honestly and our own slot would have said whatever we wrote in it."
  (plusp (length (string-trim '(#\Space #\Newline)
                              (sh (format nil "ss -ltn 2>/dev/null | grep -F ' ~a:~d ' || true"
                                          address port))))))

(defun anything-listening-on (port)
  (plusp (length (string-trim '(#\Space #\Newline)
                              (sh (format nil "ss -ltn 2>/dev/null | grep -E '[:.]~d ' || true" port))))))

(defun free-port-p (port)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (handler-case
             (progn (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
                    (sb-bsd-sockets:socket-bind sock (sb-bsd-sockets:make-inet-address "0.0.0.0") port)
                    (sb-bsd-sockets:socket-listen sock 1)
                    t)
           (error () nil))
      (ignore-errors (sb-bsd-sockets:socket-shutdown sock :direction :io))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defvar *taken* '())
(defun free-port (&optional (from 5980))
  "A port nothing is using, and that this suite has not already claimed."
  (loop for p from from below 6100
        when (and (not (member p *taken*)) (free-port-p p) (not (anything-listening-on p)))
          do (push p *taken*) (return p)
        finally (error "no free port in 5980..6100")))

(defun rfb-greeting (port &optional (host "127.0.0.1"))
  "The twelve bytes an RFB server opens with, or NIL if nothing would take the connection."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address host) port)
          (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                           :element-type '(unsigned-byte 8)
                                                           :buffering :full :timeout 3))
                (b (make-array 12 :element-type '(unsigned-byte 8))))
            (read-sequence b s)
            (map 'string #'code-char b)))
      (error () nil)
      (:no-error (v) (progn (ignore-errors (sb-bsd-sockets:socket-close sock)) v)))))

;;; ---- the suite's own hands ---------------------------------------------------
;;;
;;; SEAT-MOVE-WINDOW IS A PURE SETTER — no damage, no composite — and that is correct: the
;;; drag path damages the union of where the window was and where it now is, which it can
;;; only do by knowing both, and a setter that composited would composite twice per drag
;;; step.  But it means that driving a seat programmatically changes nothing anybody can see
;;; until somebody composites, and a suite whose helper forgot that would "pass" every
;;; per-seat pixel claim by never drawing anything.
;;;
;;; So the suite moves windows the way the WINDOW MANAGER does, through its own drag step,
;;; and asserts the distinction below (§4) so that nobody later "fixes" the setter by
;;; putting a composite inside it.

(defvar *port* nil)
(defun gate-move (seat window x y)
  "Move WINDOW to (X,Y) on SEAT's screen AND put the pixels there — WM-DRAG-MOVE-OPAQUE,
   which is the step a person's drag runs, damage and CopyRect hint and all."
  (wm-drag-move-opaque *port* window x y seat))

(defun gate-raise (seat window)
  (wm-raise *port* window seat)
  (composite-seat seat))

(defun gate-click (seat window dx dy)
  "Press and release inside WINDOW's content on SEAT's screen — the way a click arrives."
  (let ((x (+ (seat-window-x seat window) dx)) (y (+ (seat-window-y seat window) dy)))
    (glass-on-pointer *port* 1 x y seat)
    (glass-on-pointer *port* 0 x y seat)))

;;; A surface whose content is a flat colour, so one pixel names a window; and one that
;;; writes down what was typed into it, so "who has the keyboard" has an answer on paper.
;;; (The same two fixtures seat-gate and seat-audio-gate use, for the same reasons.)

(defconstant +ink-red+   #xcc2222)
(defconstant +ink-green+ #x22aa44)
(defconstant +ink-blue+  #x2244cc)

(defun add-block (port x y w h colour title)
  (let ((surf (add-surface port (lambda (fb) (glass:fb-fill fb colour)
                                  (values nil nil (constantly nil)))
                           :title title :width w :height h)))
    (setf (wm-surface-x surf) x (wm-surface-y surf) y)
    surf))

(defun add-typist (port x y w h colour title)
  (let* ((typed (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
         (surf (add-surface port (lambda (fb) (glass:fb-fill fb colour)
                                   (values (lambda (down k)
                                             (when (and down (<= 32 k 126))
                                               (vector-push-extend (code-char k) typed)))
                                           nil (constantly nil)))
                            :title title :width w :height h)))
    (setf (wm-surface-x surf) x (wm-surface-y surf) y)
    (values surf (lambda () (coerce typed 'string)))))

;;; ==============================================================================
(banner "0. a fixture that cannot reach a live desktop")
;;; ==============================================================================

(defparameter *runtime* (format nil "/tmp/glass-two-seat-gate-~d/" (sb-posix:getpid)))
(setf glass:*runtime-dir* *runtime*)
;; A seat asks for an identity when it is made.  Without :glass/nostr that is already a
;; no-op, and this makes it one whatever else gets loaded into this image later.
(setf *seat-identity* nil)

(defparameter *home-port*    (free-port 5980))
(defparameter *guest-port*   (free-port 5980))
(defparameter *late-port*    (free-port 5980))   ; a seat's port is a SETTING; this one never serves
(defparameter *control-port* (free-port 5980))

(defparameter *watched*
  (list (namestring (merge-pathnames ".glass-seats" (user-homedir-pathname)))
        (namestring (merge-pathnames ".glass-devices" (user-homedir-pathname)))
        (namestring (merge-pathnames ".glass/site-key" (user-homedir-pathname)))))
(defun watched-state () (mapcar (lambda (f) (and (probe-file f) (file-write-date f))) *watched*))
(defparameter *watched-before* (watched-state))

(check (eql 0 (search "/tmp/" *runtime*)) "the runtime directory is under /tmp: ~a" *runtime*)
(check (every (lambda (p) (not (member p '(5901 5903 5910 5911 4008 4009))))
              (list *home-port* *guest-port* *late-port* *control-port*))
       "the ports are not a live desktop's: ~d (home) ~d (guest) ~d (late) ~d (control)"
       *home-port* *guest-port* *late-port* *control-port*)
(check (every #'free-port-p (list *home-port* *guest-port* *late-port* *control-port*))
       "…and all of them are free, asked of the kernel by binding them")
(check (null *seat-identity*) "new seats will mint no identity, so no key store is opened")

;;; ==============================================================================
(banner "1. one real desktop, running")
;;; ==============================================================================
;;; RUN-WM blocks — it IS the desktop — so it runs in a thread and everything after this is
;;; done to it from outside while its compositing loop ticks.  The terminal is a real shell
;;; in a real pty: it is what makes "a content change appears on both screens" a claim about
;;; an application and not about a fixture we wrote to change.

(defparameter *session*
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (run-wm '((:terminal :cols 60 :rows 14 :ppem 13))
                 :port *home-port* :width 900 :height 620
                 ;; loopback, deliberately: this suite serves nothing anybody off the box
                 ;; can reach, and it must be able to say so.
                 :address "127.0.0.1")
       (error (e) (format t "~&!! the session died: ~a~%" e))))
   :name "two-seat-gate-session"))

(sleep 6)                               ; the CLIM event loop, the shell, the first composites

(defparameter *p* (find-glass-port :port *home-port*))
(setf *port* *p*)
(defparameter *a* (and *p* (port-seat *p*)))

(check (and *p* (glass-port-wm-p *p*)) "the session is up and in window-manager mode")
(check (sb-thread:thread-alive-p *session*) "…its RUN-WM thread is running (the tick loop)")
(check (and *a* (glass:framebuffer-p (seat-fb *a*))
            (= 900 (glass:fb-width (seat-fb *a*))) (= 620 (glass:fb-height (seat-fb *a*))))
       "…the home seat has a 900x620 screen")
(defparameter *term* (first (glass-port-surfaces *p*)))
(check (and *term* (wm-surface-p *term*)) "…and a real terminal window: ~a" (and *term* (wm-surface-title *term*)))
(check (listening-on "127.0.0.1" *home-port*) "it is serving on 127.0.0.1:~d" *home-port*)
(check (not (listening-on "0.0.0.0" *home-port*)) "…and on no other interface")
(let ((g (rfb-greeting *home-port*)))
  (check (and g (eql 0 (search "RFB " g))) "…and a real RFB client gets a greeting: ~s" g))

;; Nobody in this file has composited anything yet.  If the terminal's pixels are on the
;; home seat's screen, the desktop's own loop put them there.
(check (/= (pixel-at (seat-fb *a*) (+ (seat-window-x *a* *term*) 8)
                     (+ (seat-window-y *a* *term*) 8))
           +wm-teal+)
       "the desktop composited itself: the terminal's pixels are on the screen and this ~
        suite has not drawn anything")
(check (quiescent-p (seat-fb *a*))
       "…and an idle desktop is STILL — the digests below can mean what they say")

;;; ==============================================================================
(banner "2. a second person joins the running desktop")
;;; ==============================================================================
;;; ADD-WM-SEAT against the port that is already running, which is how a second seat is
;;; really added: at a REPL or over the control socket, by somebody who says a port number.
;;;
;;; :AUDIO NIL because sound is seat-audio-gate's subject and this image has no headset
;;; system loaded; everything else is the call's own defaults, INCLUDING WHAT IT SERVES ON,
;;; which is the point of the next three checks.

(defparameter *b* (add-wm-seat *p* :port-num *guest-port* :width 700 :height 520
                                   :name "gate-B" :audio nil))
(sleep 0.5)

(check (= 2 (length (glass-port-seats *p*))) "the session now has two seats")
(check (and (seat-home-p *a*) (not (seat-home-p *b*)))
       "one of them is the home seat, and it is the one that was here first")
(check (not (eq (seat-fb *a*) (seat-fb *b*))) "each composites into its OWN screen")
(check (and (= 700 (glass:fb-width (seat-fb *b*))) (= 520 (glass:fb-height (seat-fb *b*))))
       "…and the screens are different sizes (900x620 and 700x520)")

;;; ---- what a new seat opens onto the world ------------------------------------
;;;
;;; THE OPT-IN-SERVING SPLIT DID NOT REACH THIS CALL.  RUN-WM was divided into a session and
;;; a transport so that a desktop could decline to serve; ADD-WM-SEAT went on opening an RFB
;;; listener on *SEAT-BIND-ADDRESS* — 0.0.0.0, every interface — from a call whose only
;;; required argument is a port number, and with no credential, because a session without a
;;; password has none to inherit.  Adding a person to your desktop is not a decision to
;;; publish it, and SERVE-SEAT-VNC (the only other thing that picks an address on somebody's
;;; behalf) had already been given exactly this rule and this reason.
;;;
;;; So: a seat still gets its port, and it gets it on loopback until there is a credential
;;; to go with it, and a caller who names an address is still obeyed.

(check (seat-serving-p *b*) "the new seat IS reachable — it was asked for by port number")
(check (listening-on "127.0.0.1" *guest-port*) "…on 127.0.0.1:~d" *guest-port*)
(check (not (listening-on "0.0.0.0" *guest-port*))
       "…AND ON NOTHING ELSE: adding a seat does not put the desktop on every interface")
(let ((g (rfb-greeting *guest-port*)))
  (check (and g (eql 0 (search "RFB " g)))
         "…and it is a real listener and not merely an absent one: ~s" g))
(check (null (transport-credential (first (seat-transports *b*))))
       "…which matters because there is no credential on it at all")

(check (equal "0.0.0.0" (wm-seat-serve-address "0.0.0.0"))
       "a caller who NAMES an interface is still obeyed — the rule is about the default")
(check (equal "127.0.0.1" (wm-seat-serve-address :default))
       "…and with no credential the default is loopback")
(check (let ((glass:*vnc-password* "a-real-credential")
             (glass::*vnc-verify-fn* (lambda (&rest r) (declare (ignore r)) t)))
         (equal *seat-bind-address* (wm-seat-serve-address :default)))
       "…while a session that CAN demand a password gets ~a, which is what the credential buys"
       *seat-bind-address*)
(check (let ((glass:*vnc-password* "a-password-nothing-can-check")
             (glass::*vnc-verify-fn* nil))
         (equal "127.0.0.1" (wm-seat-serve-address :default)))
       "…and a password no verifier in this image can check is not a credential, it is a ~
        listener that rejects everybody")

;;; ---- and it sees the desktop as it stands ------------------------------------

(check (zerop (hash-table-count (seat-views *b*)))
       "the new seat holds NO view records — copy-on-write against the windows' own slots")
(check (and (= (seat-window-x *a* *term*) (seat-window-x *b* *term*))
            (= (seat-window-y *a* *term*) (seat-window-y *b* *term*)))
       "…so it reads the window's own position and agrees with the home seat")
(defparameter *tw* (glass:fb-width (wm-surface-fb *term*)))
(defparameter *th* (glass:fb-height (wm-surface-fb *term*)))
(check (= (rect-digest (seat-fb *a*) (seat-window-x *a* *term*) (seat-window-y *a* *term*) *tw* *th*)
          (rect-digest (seat-fb *b*) (seat-window-x *b* *term*) (seat-window-y *b* *term*) *tw* *th*))
       "AND BOTH SCREENS SHOW IT, byte for byte over its whole ~dx~d content rectangle"
       *tw* *th*)
(shoot "joined" *a* *b*)

;;; ==============================================================================
(banner "3. shared content, separate screens")
;;; ==============================================================================
;;; One terminal, one shell, one content framebuffer.  Type into it on seat A and seat B's
;;; screen must change too — content damage is the one kind that is session-wide, because a
;;; window painted itself and everybody watching has to see it.

(setf (seat-focus-surface *a*) *term*)
(defparameter *a-before* (digest (seat-fb *a*)))
(defparameter *b-before* (digest (seat-fb *b*)))

(dolist (c (coerce "echo two-seats" 'list)) (glass-on-key *p* t (char-code c)))
(glass-on-key *p* t 65293)              ; Return
(sleep 1.5)                             ; the shell answers, the tick loop composites

(check (/= *a-before* (digest (seat-fb *a*))) "typing into the terminal changed seat A's screen")
(check (/= *b-before* (digest (seat-fb *b*)))
       "…AND SEAT B'S, which nobody typed into: what runs is shared")
(check (= (rect-digest (seat-fb *a*) (seat-window-x *a* *term*) (seat-window-y *a* *term*) *tw* *th*)
          (rect-digest (seat-fb *b*) (seat-window-x *b* *term*) (seat-window-y *b* *term*) *tw* *th*))
       "…and the two screens hold the SAME pixels of it — one content fb, composited twice")
(check (and (= *tw* (glass:fb-width (wm-surface-fb *term*)))
            (= *th* (glass:fb-height (wm-surface-fb *term*))))
       "…and the window's SIZE is shared: it is one framebuffer, ~dx~d, for both of them"
       *tw* *th*)
(shoot "typed" *a* *b*)

;;; ==============================================================================
(banner "4. where a window sits is one person's opinion")
;;; ==============================================================================

(check (quiescent-p (seat-fb *a*)) "the desktop is idle again")
(defparameter *a-still* (digest (seat-fb *a*)))
(defparameter *b-still* (digest (seat-fb *b*)))

;;; ---- the setter is a setter (and must stay one) -------------------------------
;;; SEAT-MOVE-WINDOW changes where a window is FOR A SEAT and does not damage, composite or
;;; wake anything.  Asserted here, on a running desktop with its tick loop going, so that
;;; the day somebody "fixes" the silence by putting a composite inside the setter, this
;;; fails and says why: the drag path damages the union of the old box and the new one, and
;;; it is the only thing that knows both.

(seat-move-window *b* *term* 330 300)
(sleep 0.3)
(check (= *b-still* (digest (seat-fb *b*)))
       "SEAT-MOVE-WINDOW alone moved nothing on screen — it is a setter, not a paint")
(check (null (seat-pending *b*))
       "…and accumulated no damage for the tick loop to find, so nothing was going to happen")
(composite-seat *b*)
(check (/= *b-still* (digest (seat-fb *b*)))
       "…and one COMPOSITE-SEAT later the pixels are where the slot said they were")
(seat-move-window *b* *term* (seat-window-x *a* *term*) (seat-window-y *a* *term*))
(composite-seat *b*)
(seat-forget-window *b* *term*)          ; back to no divergence at all
(composite-seat *b*)

;;; ---- and now the real thing --------------------------------------------------

(check (quiescent-p (seat-fb *a*)) "still idle")
(setf *a-still* (digest (seat-fb *a*)) *b-still* (digest (seat-fb *b*)))
;; Somewhere the whole window still fits on B's SMALLER screen, so that the rectangle
;; compared below is a whole window on both and not a window against a clipped copy of it.
(defparameter *bx* 200) (defparameter *by* 240)
(gate-move *b* *term* *bx* *by*)         ; seat B drags the terminal across ITS screen
(sleep 0.3)

(check (= *a-still* (digest (seat-fb *a*)))
       "SEAT A'S SCREEN IS BYTE-IDENTICAL after seat B moved a window (~16,'0x)" *a-still*)
(check (/= *b-still* (digest (seat-fb *b*))) "…and seat B's is not")
(check (and (= *bx* (seat-window-x *b* *term*)) (/= *bx* (seat-window-x *a* *term*)))
       "the two seats hold the same window at ~d and ~d"
       (seat-window-x *b* *term*) (seat-window-x *a* *term*))
(check (= 1 (hash-table-count (seat-views *b*)))
       "B materialised exactly ONE view record — one window's worth of divergence")
(check (zerop (hash-table-count (seat-views *a*)))
       "…and the home seat still has none: its moves write the window's own slots, which ~
        is what makes them the session's arrangement")
(check (and (<= (+ *bx* *tw*) (glass:fb-width (seat-fb *b*)))
            (<= (+ *by* *th*) (glass:fb-height (seat-fb *b*)))
            (= (rect-digest (seat-fb *a*) (seat-window-x *a* *term*) (seat-window-y *a* *term*) *tw* *th*)
               (rect-digest (seat-fb *b*) *bx* *by* *tw* *th*)))
       "…and the moved window is the SAME PIXELS in its new place: a move is not a repaint")
(shoot "moved" *a* *b*)

;;; ==============================================================================
(banner "5. what is in front is one person's opinion too")
;;; ==============================================================================
;;; Two overlapping blocks of flat colour, so the pixel at the overlap names the window
;;; that is winning.  Added the way a menu pick adds one, and composited once for everybody.

(defparameter *red*   (add-block *p*  60 320 260 180 +ink-red+   "red"))
(defparameter *green* (add-block *p* 140 380 260 180 +ink-green+ "green"))
(composite-all *p*)
(sleep 0.3)
(defparameter *ox* 160) (defparameter *oy* 400)   ; a point inside BOTH

(gate-raise *a* *red*)
(gate-raise *b* *green*)
(sleep 0.3)
(check (= (pixel-at (seat-fb *a*) *ox* *oy*) +ink-red+) "seat A sees RED in front at the overlap")
(check (= (pixel-at (seat-fb *b*) *ox* *oy*) +ink-green+) "seat B sees GREEN in front at the same point")
(check (and (eq (wm-topmost *p* *a*) *red*) (eq (wm-topmost *p* *b*) *green*))
       "the two stacking orders disagree, and each is internally consistent")
(check (and (eq (wm-hit *p* *ox* *oy* *a*) *red*) (eq (wm-hit *p* *ox* *oy* *b*) *green*))
       "…so a click at that point hits a different window on each seat")

;;; A window the second seat has never restacked keeps its SESSION ticket, and the tickets
;;; all come from one ZCLOCK — which is what makes them comparable.  A new window is the
;;; sharpest case: B has diverged about two windows and has never heard of this one, and it
;;; must still open ON TOP for B, because its ticket is newer than anything B holds.
(defparameter *blue* (add-block *p* 200 200 200 140 +ink-blue+ "blue"))
(composite-all *p*)
(sleep 0.3)
(check (null (seat-view *b* *blue*)) "seat B holds no view of the new window")
(check (and (eq (wm-topmost *p* *a*) *blue*) (eq (wm-topmost *p* *b*) *blue*))
       "…and it is topmost for BOTH: one session zclock, so a ticket B never asked for ~
        still sorts above the ones it did")
(check (and (= (pixel-at (seat-fb *a*) 260 260) +ink-blue+)
            (= (pixel-at (seat-fb *b*) 260 260) +ink-blue+))
       "…in pixels, on both screens")
(check (> (window-own-z *blue*) (view-z (seat-view *b* *green*)))
       "…because the ticket really is the later one (~d > ~d)"
       (window-own-z *blue*) (view-z (seat-view *b* *green*)))
(shoot "stacking" *a* *b*)

;;; ==============================================================================
(banner "6. two keyboards, two focused windows, at once")
;;; ==============================================================================

(multiple-value-bind (ta read-a) (add-typist *p* 480 60 200 120 #x884400 "typist-A")
  (multiple-value-bind (tb read-b) (add-typist *p* 480 200 200 120 #x008888 "typist-B")
    (composite-all *p*)
    (sleep 0.3)
    (gate-click *a* ta 20 20)
    (gate-click *b* tb 20 20)
    (check (and (eq (seat-focus-surface *a*) ta) (eq (seat-focus-surface *b*) tb))
           "A focused typist-A, B focused typist-B")
    (check (not (eq (seat-focus-surface *a*) (seat-focus-surface *b*)))
           "…so the focus is genuinely per-seat and not one focus with two owners")
    ;; interleaved, as two people typing really are
    (loop for ca across "hello" for cb across "world" do
      (glass-on-key *p* t (char-code ca) *a*)
      (glass-on-key *p* t (char-code cb) *b*))
    (check (string= (funcall read-a) "hello") "A typed \"hello\" into its own window")
    (check (string= (funcall read-b) "world") "B typed \"world\" into its own window")
    (check (and (string= (funcall read-a) "hello") (string= (funcall read-b) "world"))
           "…and neither keyboard leaked one character into the other's window")
    (check (eq (seat-focus-surface *a*) ta) "B's click did not steal A's focus")
    ;; and the terminal, which A had focused before, is not typing for anybody now
    (check (not (eq (seat-focus-surface *a*) *term*))
           "…A's focus moved to the window A clicked, and only A's")))

;;; ==============================================================================
(banner "7. a menu belongs to the person who opened it")
;;; ==============================================================================

(setf (glass-port-menu-items *p*) (list (cons "Alpha" (lambda () nil))
                                        (cons "Beta"  (lambda () nil))))
(check (quiescent-p (seat-fb *b*)) "the desktop is idle before the menu opens")
(defparameter *b-nomenu* (digest (seat-fb *b*)))
(defparameter *a-nomenu* (digest (seat-fb *a*)))
(defparameter *a-teal* (count-workspace (seat-fb *a*)))
(defparameter *b-teal* (count-workspace (seat-fb *b*)))

(glass-on-pointer *p* 4 620 420 *a*)     ; right-press bare workspace on seat A
(sleep 0.3)
(check (and (seat-menu *a*) (null (seat-menu *b*))) "seat A has an open root menu; seat B has none")
(check (/= (pixel-at (seat-fb *a*) 626 426) +wm-teal+) "A's screen has menu pixels at the press point")
(check (< (count-workspace (seat-fb *a*)) *a-teal*)
       "…and less bare workspace than before (~d fewer pixels of it)"
       (- *a-teal* (count-workspace (seat-fb *a*))))
(check (= *b-nomenu* (digest (seat-fb *b*)))
       "SEAT B'S SCREEN IS BYTE-IDENTICAL — the menu did not touch one pixel of it")
(check (= *b-teal* (count-workspace (seat-fb *b*))) "…same bare workspace, to the pixel")
(shoot "menu" *a* *b*)

(glass-on-pointer *p* 1 60 560 *a*)      ; dismiss: a left press off every menu
(sleep 0.3)
(check (null (seat-menu *a*)) "A dismissed its own menu")
(check (= *a-nomenu* (digest (seat-fb *a*)))
       "…and A's screen came back to exactly the pixels it had before it opened")
(check (= *b-nomenu* (digest (seat-fb *b*))) "…while B's never moved at all")

;;; ==============================================================================
(banner "8. copy-on-write, and a seat that joins late")
;;; ==============================================================================
;;; A seat that has never moved a particular window holds NO record for it and reads the
;;; window's own slots.  The home seat writes through to those slots, so what they hold IS
;;; the session's arrangement — which makes this claim testable at the strongest setting
;;; there is: a seat that joins now, at the home seat's screen size, must composite to the
;;; SAME PIXELS as the home seat.  Not "the same windows": the same 558,000 pixels.

(defparameter *c* (add-wm-seat *p* :port-num *late-port* :serve nil :audio nil :name "gate-C"
                                   :width 900 :height 620))
(sleep 0.5)
(check (zerop (hash-table-count (seat-views *c*))) "the late seat holds no view records")
(check (every (lambda (w) (and (= (seat-window-x *c* w) (window-own-x w))
                               (= (seat-window-y *c* w) (window-own-y w))
                               (= (seat-window-z *c* w) (window-own-z w))))
              (glass-port-surfaces *p*))
       "…so for every window it reads that window's own position and stacking")
(check (not (seat-serving-p *c*)) "…and :SERVE NIL means it is reachable from nowhere at all")
(composite-seat *c*)
(sleep 0.3)
(check (= (digest (seat-fb *a*)) (digest (seat-fb *c*)))
       "A SEAT THAT JOINED LAST SEES THE DESKTOP AS IT STANDS, byte for byte with the home ~
        seat's screen (~16,'0x)" (digest (seat-fb *c*)))
(check (/= (digest (seat-fb *c*)) (digest (seat-fb *b*)))
       "…and not what seat B sees, because seat B has an arrangement of its own")
(shoot "late-joiner" *a* *b* *c*)

;; …and it is copy-on-write and not a snapshot: the moment C moves something, it diverges,
;; and the window's own slots are untouched.
(let ((own (window-own-x *red*)))
  (gate-move *c* *red* 500 480)
  (check (and (= 500 (seat-window-x *c* *red*)) (= own (window-own-x *red*)))
         "…until it moves one: C holds it at 500, the window's own x is still ~d" own)
  (check (= 1 (hash-table-count (seat-views *c*))) "…one view record, made on the write"))

;;; ==============================================================================
(banner "9. two people copying do not clobber each other")
;;; ==============================================================================

(check (not (eq (seat-clipboard *a*) (seat-clipboard *b*))) "each seat has its own selection")
(check (eq (seat-clipboard *a*) (glass:session-clipboard))
       "…and the HOME seat's IS the session clipboard — so everything that reaches the ~
        clipboard without naming a seat lands where the first person is looking")
(check (not (eq (seat-clipboard *b*) (glass:session-clipboard)))
       "…which a further seat's is not")
(glass:clipboard-own (seat-clipboard *a*) :gate-a :text "A's paragraph")
(glass:clipboard-own (seat-clipboard *b*) :gate-b :text "B's filename")
(check (and (string= (glass:clipboard-text (seat-clipboard *a*)) "A's paragraph")
            (string= (glass:clipboard-text (seat-clipboard *b*)) "B's filename"))
       "…and after both copied, each still holds its own")
(check (string= (glass:clipboard-text (glass:session-clipboard)) "A's paragraph")
       "…with the session's selection being the home seat's, unchanged by B copying")

;;; ==============================================================================
(banner "10. the wire you ask a running desktop with")
;;; ==============================================================================
;;; A live desktop is interrogated over its control socket, and that socket used to swallow
;;; READER errors: only (EVAL FORM) was inside the reporting handler, so a form naming a
;;; symbol in the wrong package closed the connection WITH NO OUTPUT AT ALL.  Silence from a
;;; socket reads like an answer — twice, it was mistaken for "this desktop has no seats".
;;; CLIM-GLASS:START-CONTROL-SOCKET (backend/control.lisp) is the one that answers, and this
;;; suite asks its questions through it so it cannot inherit the failure mode either.

(start-control-socket :port *control-port* :name "two-seat-gate-control")
(sleep 0.3)

(defun ask (text &optional (timeout 5))
  "Send TEXT to the control socket and return what it says — or :SILENCE if it closed
   without a byte, which is the answer this section exists to make impossible."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (handler-case
             (progn
               (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address "127.0.0.1")
                                              *control-port*)
               (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                                :element-type 'character
                                                                :buffering :full :timeout timeout)))
                 (write-string text s) (terpri s) (force-output s)
                 (or (read-line s nil nil) :silence)))
           (error (e) (format nil "!! ~a" e)))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(check (listening-on "127.0.0.1" *control-port*)
       "the control socket is on 127.0.0.1:~d — an unauthenticated EVAL binds nothing else"
       *control-port*)
(let ((answer (ask (format nil "(length (glass-port-seats (find-glass-port :port ~d)))" *home-port*))))
  (check (equal answer "3") "it answers a real question about the running desktop: ~s" answer))
(let ((answer (ask (format nil "(mapcar (function seat-name) (glass-port-seats (find-glass-port :port ~d)))"
                           *home-port*))))
  (check (search "gate-B" (princ-to-string answer))
         "…and the seats it names are the ones this suite made: ~a" answer))
(let ((answer (ask (format nil "(seat-window-x (second (glass-port-seats (find-glass-port :port ~d))) ~
                                 (find \"terminal\" (glass-port-surfaces (find-glass-port :port ~d)) ~
                                       :key (function wm-surface-title) :test (function equal)))"
                           *home-port* *home-port*))))
  (check (equal answer (princ-to-string (seat-window-x *b* *term*)))
         "…including where seat B holds the terminal, which is ~a and not the home seat's ~a"
         answer (seat-window-x *a* *term*)))

(let ((answer (ask "(glass:seat-name nil)")))
  (check (not (eq answer :silence))
         "A FORM THAT WILL NOT READ GETS AN ANSWER, not a closed connection")
  (check (and (stringp answer) (eql 0 (search "ERROR:" answer)))
         "…and the answer says what went wrong: ~a" answer)
  (check (and (stringp answer) (search "unreadable" answer))
         "…and says it was the READ, which is the half most likely to fail"))
(let ((answer (ask "(car 7)")))
  (check (and (stringp answer) (eql 0 (search "ERROR:" answer)))
         "a form that reads and will not eval still answers: ~a" answer))
(let ((answer (ask "(+ 1 2)")))
  (check (equal answer "3") "…and the socket is still serving afterwards: ~s" answer))
;; …and the same semantics with no socket in the way, which is where they are testable.
(let ((unbalanced (control-answer (make-string-input-stream "(unbalanced")))
      (fine       (control-answer (make-string-input-stream "(list 1 2)")))
      (nothing    (control-answer (make-string-input-stream ""))))
  (check (and (stringp unbalanced) (search "unreadable" unbalanced))
         "CONTROL-ANSWER reports an unfinished form off a string stream too: ~a" unbalanced)
  (check (equal fine "(1 2)") "…evaluates a good one: ~s" fine)
  (check (null nothing) "…and an empty request is the one thing that gets silence"))
;; WHAT THE FORM PRINTS COMES BACK, as at a REPL.  Every reporting function in the image
;; writes to *STANDARD-OUTPUT* and returns something small, so without this a control
;; connection gets the small thing and the report goes to the session log:
;; (cl-transport.gate:report) answered "5", which was the number of lines it had just
;; written somewhere the asker could not see.
(let ((printed (control-answer (make-string-input-stream "(progn (princ \"hello\") 5)")))
      (failed  (control-answer (make-string-input-stream "(progn (princ \"partial\") (error \"boom\"))"))))
  (check (and (search "hello" printed) (search "5" printed))
         "CONTROL-ANSWER returns what the form printed AND its value: ~s" printed)
  (check (and (search "partial" failed) (search "ERROR" failed))
         "…and keeps what was printed before a form failed: ~s" failed))

;;; ==============================================================================
(banner "the desktop ran through all of it, and nothing else was touched")
;;; ==============================================================================

(check (sb-thread:thread-alive-p *session*) "the session is still running")
(check (= 3 (length (glass-port-seats *p*))) "…with its three seats")
(check (quiescent-p (seat-fb *a*)) "…and an idle desktop that is still idle")

(check (equal *watched-before* (watched-state))
       "~~/.glass-seats, ~~/.glass-devices and ~~/.glass/site-key are exactly as they were")
(check (notany (lambda (p) (listening-on "0.0.0.0" p))
               (list *home-port* *guest-port* *control-port*))
       "nothing this suite opened was ever bound off-loopback")

;;; ---- clean up ----------------------------------------------------------------
;;; The shells die with their windows, the listeners are closed rather than left to the
;;; process exit, and the runtime directory goes with them.

(dolist (s (copy-list (glass-port-surfaces *p*))) (ignore-errors (wm-close *p* s)))
(dolist (seat (glass-port-seats *p*)) (ignore-errors (close-seat-transports seat)))
(sleep 0.3)
(check (not (or (anything-listening-on *home-port*) (anything-listening-on *guest-port*)))
       "both seats' listeners are closed and their ports are free again")
(sh (format nil "rm -rf ~a" *runtime*))
(check (null (probe-file *runtime*)) "…and the runtime directory is gone")

(format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp *fail*) *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0) :abort t)
