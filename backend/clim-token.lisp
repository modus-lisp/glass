;;;; clim-token.lisp — who is driving McCLIM right now, and how the driving changes hands.
;;;;
;;;; Native glass windows (a terminal, warren, loom, a nested remote desktop) are
;;;; per-seat all the way down: they are handed (down keysym) and (mask x y) by whichever
;;;; seat is addressing them, so two people drive two of them at once and neither knows
;;;; the other exists.  McCLIM is not like that and cannot be made like that from out
;;;; here.  A CLIM port has ONE pointer object (CLIMI::PORT-POINTER), ONE keyboard focus
;;;; (PORT-KEYBOARD-INPUT-FOCUS), and DISTRIBUTE-EVENT routes by that focus while
;;;; ignoring the sheet the event names.  Worse for us, a top-level sheet has ONE screen
;;;; transformation, and it is what CLIM places pull-down menus, dialogs and tooltips
;;;; from.  So McCLIM windows are ONE CONSOLIDATED SEAT however many people are watching,
;;;; and this file is the whole of the arbitration: a TOKEN, held by at most one seat.
;;;;
;;;; ---- three things the word "primary" used to mean --------------------------
;;;;
;;;; The seat abstraction grew a PRIMARY seat and then quietly hung three unrelated jobs
;;;; on it.  They are separated here, because only one of them should move when somebody
;;;; else starts clicking:
;;;;
;;;;   THE HOME SEAT (SEAT-HOME-P, the SEAT's own PRIMARY slot).  Which seat INHERITS THE
;;;;     SESSION'S PRE-EXISTING RESOURCES: the session clipboard, the session mixer's own
;;;;     mix, GLASS:*KEY-INJECTOR*, the RFB desktop name, and PORT-SEAT's default.  This
;;;;     is about a one-seat desktop being byte-for-byte the old one, and it is FIXED at
;;;;     the first seat for the life of the session.  It does not follow anybody's mouse.
;;;;
;;;;   THE SESSION ARRANGEMENT.  Whose window moves write THROUGH to the window's own
;;;;     X/Y/Z — the default position a seat sees until it diverges, and what a seat
;;;;     joining later finds on screen.  Also the home seat's, and also fixed: if it
;;;;     travelled, a third person joining would inherit the arrangement of whoever last
;;;;     clicked in a CLIM window, which is nobody's idea of a default.
;;;;
;;;;   THE CLIM DRIVER (this file).  Which seat's pointer, keys and window POSITIONS
;;;;     McCLIM's single-valued state currently tracks.  THIS is what travels, because it
;;;;     is the only one of the three that is about who has their hand on the thing now.
;;;;
;;;; Before this file the driver token existed but the GEOMETRY half did not travel with
;;;; it: WM-SYNC-SHEET only ever ran for the home seat, so a second seat could drag a CLIM
;;;; window across its own screen and CLIM would go on placing that window's pull-downs
;;;; where the HOME seat holds it.  One seat forever, in the one place it shows.
;;;;
;;;; ---- the token's life ------------------------------------------------------
;;;;
;;;;   FREE   nobody is driving.  The next input of any kind — a press, a key, even bare
;;;;          motion into a CLIM window — takes it SILENTLY.  This is the state a
;;;;          one-seat desktop spends its idle time in, and taking it costs nothing
;;;;          (CLIM-RESYNC-GEOMETRY finds nothing diverged and enqueues nothing), which is
;;;;          why a lone seat cannot tell any of this is here.
;;;;
;;;;   HELD   somebody is driving.  Their pointer, their keys.  Another seat's bare motion
;;;;          is dropped — otherwise their mouse would drag CLIM's one pointer around
;;;;          under the holder's hands — and only a PRESS INSIDE a CLIM window takes it,
;;;;          which is a gesture everybody already understands as "I am driving now".
;;;;
;;;; TAKING A FREE TOKEN AND TAKING A HELD ONE ARE DIFFERENT EVENTS and are kept apart in
;;;; the code (CLIM-TOKEN-TAKE-FREE vs CLIM-TOKEN-CONTEST) even though today they do the
;;;; same thing.  A contest is the only one of the two that anybody could reasonably want
;;;; a policy about — ask first, refuse, flash something — and this is where it attaches.
;;;;
;;;; A held token goes back to FREE when:
;;;;   - the holder's pointer leaves every CLIM window (CLIM-TOKEN-FOLLOW-POINTER),
;;;;   - the holder has been silent for *CLIM-TOKEN-IDLE* (the tick loop's sweep),
;;;;   - the holder's last viewer disconnects (CLIM-TOKEN-SEAT-GONE).
;;;; None of those is a handoff; they only stop the token being held FOREVER by somebody
;;;; who has walked away, so that the next person's click is a silent take and not a
;;;; contest.
;;;;
;;;; ---- never in the middle of a gesture --------------------------------------
;;;;
;;;; While the holder has a CLIM pull-down posted, a WM menu open, a window drag in
;;;; flight, or simply a button down, the token is PINNED.  A press from another seat
;;;; then does not take it — it is remembered as PENDING and applied the moment the
;;;; gesture ends (CLIM-TOKEN-SETTLE).  Without that, the other seat's press lands inside
;;;; a CLIM tracking loop that is following the one pointer, and the pointer teleports out
;;;; from under an in-flight drag: a menu that never closes, a drag that never lands.

(in-package #:clim-glass)

;;; Forward references: this file is loaded before BACKEND and WM so that GLASS-PORT can
;;; hold a CLIM-TOKEN in a slot initform.  Declaimed rather than left dangling so the
;;; build stays quiet about them.
(declaim (ftype function enqueue composite-seat
                glass-port-mirrors glass-mirror-sheet glass-mirror-managed
                glass-mirror-clim-x glass-mirror-clim-y
                glass-port-default-seat wm-surface-p port-damage-popups))

;;; ---- knobs ------------------------------------------------------------------

(defvar *clim-token-idle* 15
  "Seconds of silence after which a HELD McCLIM token goes FREE.

   Chosen to be longer than a person pauses INSIDE a gesture and shorter than a person
   is away from one.  Reading a CLIM window with your hands off the mouse for ten
   seconds is ordinary and should not look like leaving; a shared window nobody has
   touched for a quarter of a minute should not still be spoken for.  Going free is
   cheap and invisible in both directions — the holder's next motion takes it back
   silently, and a resync of an unchanged layout enqueues nothing — so the cost of
   picking this too short is a counter going up, not a jump on anybody's screen.  It is
   a special so a running desktop can be retuned from its control socket.")

(defvar *clim-token-stuck* 60
  "Seconds of silence after which a token pinned ONLY BY A BUTTON BEING DOWN is freed
   anyway, its holder's button mask treated as stale.  A minute of no input at all with
   the mouse held down is a client that died between the press and the release, not a
   long drag.  A pin held by a CLIM GRAB is never broken here: the grab is McCLIM's and
   only McCLIM can end it.")

(defvar *clim-token-indicator* nil
  "Show a seat that is NOT driving McCLIM that somebody else is: CLIM windows' title bars
   are tinted on that seat's screen (+WM-TITLE-OTHER-BG+), and left alone on the driver's.

   DEFAULT NIL — off.  It is built so the question 'do we want to see this?' can be
   answered by setting this to T on a running desktop rather than by writing the feature
   first.  Flipping it recomposites the affected seats, so the change is immediate.")

;;; ---- the token ---------------------------------------------------------------

(defclass clim-token ()
  ((holder  :initform nil    :accessor token-holder)
   (state   :initform :free  :accessor token-state)   ; :FREE or :HELD
   (touched :initform 0      :accessor token-touched) ; internal-real-time of last holder input
   (pending :initform nil    :accessor token-pending) ; seat waiting out a pinned gesture
   ;; Counters.  They are the only way to answer "did this churn?" without a stopwatch,
   ;; and the gates read them; keeping them on the token means a running desktop can be
   ;; asked the same question over its control socket.
   (silent-takes :initform 0 :accessor token-silent-takes)  ; a FREE token taken
   (contests     :initform 0 :accessor token-contests)      ; a HELD token taken from somebody
   (deferrals    :initform 0 :accessor token-deferrals)     ; a press refused mid-gesture
   (releases     :initform 0 :accessor token-releases)      ; went FREE
   (resync-scans :initform 0 :accessor token-resync-scans)  ; CLIM windows examined on acquire
   (resyncs      :initform 0 :accessor token-resyncs))      ; …of those, actually moved
  (:documentation
   "Which seat drives McCLIM, and the bookkeeping that lets it change hands without
    changing hands in the middle of a gesture.  A class for the reason everything
    long-lived here is one: a desktop runs for weeks and grows slots while it runs."))

(defmethod print-object ((tok clim-token) stream)
  (print-unreadable-object (tok stream :type t)
    (format stream "~a~@[ ~a~]~@[ pending ~a~]"
            (slot-value tok 'state)
            (let ((h (slot-value tok 'holder))) (and h (seat-name h)))
            (let ((p (slot-value tok 'pending))) (and p (seat-name p))))))

(defun token-now () (get-internal-real-time))
(defun token-age (tok)
  "Seconds since the holder last did anything."
  (/ (- (token-now) (token-touched tok)) (float internal-time-units-per-second)))

;;; ---- geometry: what McCLIM believes, vs what a seat sees ---------------------
;;;
;;; A managed mirror now carries TWO positions.  GLASS-MIRROR-X/Y is the window's OWN
;;; position — the session default, what a seat sees until it moves the window, and what
;;; the home seat writes.  GLASS-MIRROR-CLIM-X/Y is where MCCLIM'S SHEET TRANSFORMATION
;;; currently puts it, which is the driver's view and nobody else's.
;;;
;;; They were the same slot, which is exactly why the geometry could not travel: writing
;;; the sheet to point at a second seat's arrangement also rewrote the window's own
;;; position, and the FIRST seat's screen jumped.  This special is how the write is told
;;; apart: bound while (and only while) we are aiming McCLIM at the driver's view.

(defvar *clim-geometry-follows-driver* nil
  "True while McCLIM's sheet geometry is being pointed at the CLIM driver's view of a
   window.  SET-MIRROR-GEOMETRY then records only what CLIM believes and leaves the
   window's own position — the session default every other seat is reading — alone.")

(defun clim-sheet-goto (port mirror x y)
  "Put McCLIM's idea of MIRROR's window at (X,Y): set the sheet transformation and
   update the mirror geometry, on the EVENT THREAD (both touch sheet state).  Records
   the new CLIM position immediately, before the event runs, so a second claim in the
   same breath does not enqueue the same move twice."
  (when-let ((sheet (glass-mirror-sheet mirror)))
    (setf (glass-mirror-clim-x mirror) x
          (glass-mirror-clim-y mirror) y)
    (enqueue port (lambda ()
                    (let ((*clim-geometry-follows-driver* t))
                      (setf (sheet-transformation sheet)
                            (make-translation-transformation x y))
                      (climi::update-mirror-geometry sheet))))
    t))

(defun clim-resync-geometry (port seat)
  "Aim McCLIM's single-valued sheet geometry at SEAT's arrangement, and answer how many
   windows had to move.

   Only windows whose view actually DIVERGES cost anything: the comparison is against
   GLASS-MIRROR-CLIM-X/Y, what CLIM believes right now, so two seats holding the desktop
   the same way hand the token back and forth for free.  And copy-on-write means a seat
   that has moved nothing has no SEAT-VIEW records at all, so SEAT-WINDOW-X is a slot
   read of the window itself — the common case is a loop over the CLIM windows doing
   arithmetic and nothing else."
  (let ((tok (glass-port-clim-token port)) (scanned 0) (moved 0))
    ;; A posted pop-up is drawn relative to where each seat holds the window that opened
    ;; it, MEASURED AGAINST WHAT CLIM BELIEVES (GLASS-MIRROR-CLIM-X/Y) — so moving what
    ;; CLIM believes moves the pop-up on every screen, without the pop-up repainting.
    ;; Damage where it is now, then where it ends up.  A handoff cannot normally happen
    ;; with a pull-down posted (the token is PINNED by the grab), so this is for the ways
    ;; a pin ends without the menu doing: CLIM-TOKEN-SEAT-GONE, chiefly.  Both calls are
    ;; a no-op walk of the mirror list when nothing is posted.
    (port-damage-popups port)
    (dolist (m (glass-port-mirrors port))
      (when (and (typep m 'glass-mirror) (glass-mirror-managed m) (glass-mirror-sheet m))
        (incf scanned)
        (let ((x (seat-window-x seat m)) (y (seat-window-y seat m)))
          (unless (and (eql x (glass-mirror-clim-x m)) (eql y (glass-mirror-clim-y m)))
            (when (clim-sheet-goto port m x y) (incf moved))))))
    (when (plusp moved) (port-damage-popups port))
    (incf (token-resync-scans tok) scanned)
    (incf (token-resyncs tok) moved)
    moved))

;;; ---- asking the token ---------------------------------------------------------

(defun glass-port-clim-token (port)
  (or (slot-value port 'clim-token)
      (setf (slot-value port 'clim-token) (make-instance 'clim-token))))

(defun clim-token-holder (port)
  "The seat driving McCLIM, or NIL if the token is free."
  (let ((tok (glass-port-clim-token port)))
    (and (eq (token-state tok) :held) (token-holder tok))))

(defun clim-driver-p (port seat)
  "Is SEAT the seat currently driving McCLIM?  A NIL seat is the session speaking for
   itself — an injected key, a harness, a control socket — and is always allowed: there
   is no second hand for it to collide with."
  (or (null seat) (eq seat (clim-token-holder port))))

;; The name this predicate has had since the token existed.  Kept because it is what the
;; call sites and anything typed at a running desktop say.
(defun mcclim-seat-p (port seat) (clim-driver-p port seat))

(defun clim-token-seat (port)
  "The seat whose single-valued CLIM state (modifiers, grab) McCLIM should read.  The
   driver, or — while the token is free — the home seat, which is where that state lived
   before there were seats."
  (or (clim-token-holder port) (glass-port-default-seat port)))

(defun clim-token-elsewhere-p (port seat)
  "Is somebody OTHER than SEAT driving McCLIM?  False when the token is free: nobody
   driving is not somebody else driving.  This is the question the holder indicator asks."
  (let ((h (clim-token-holder port)))
    (and h seat (not (eq h seat)))))

(defun clim-token-pinned-p (port)
  "Is the token in the middle of a gesture and therefore unhandoffable right now?"
  (let ((h (clim-token-holder port)))
    (and h (seat-mid-gesture-p h))))

;;; ---- taking it ----------------------------------------------------------------

(defun clim-token-grant (port seat how)
  "Give SEAT the token, however it came by it, and point McCLIM at SEAT's arrangement.
   The single place the holder changes, so the resync cannot be forgotten on some path."
  (let* ((tok (glass-port-clim-token port))
         (old (clim-token-holder port))
         (changed (not (eq old seat))))
    (setf (token-holder tok) seat
          (token-state tok) :held
          (token-touched tok) (token-now)
          (token-pending tok) nil)
    (when changed
      (ecase how
        (:initial)                                    ; the session's first seat, at startup
        (:free    (incf (token-silent-takes tok)))
        (:contest (incf (token-contests tok))))
      (clim-resync-geometry port seat)
      (clim-token-repaint-indicator port (list old seat)))
    t))

(defun clim-token-take-free (port seat)
  "SEAT takes a FREE token.  Nobody is being interrupted, so this is silent — no
   announcement, no arbitration, and (for a lone seat re-arriving at its own window) no
   work at all."
  (clim-token-grant port seat :free))

(defun clim-token-contest (port seat)
  "SEAT takes a HELD token from whoever had it: somebody pressed inside a CLIM window
   while somebody else was driving.

   Today this is a grant, exactly as taking a free token is.  It is a separate function
   because it is a different event, and it is the one that could ever want a policy —
   ask the holder, refuse below some interval, flash the title bar of the window being
   taken.  Anything of that kind goes here and nowhere else."
  (clim-token-grant port seat :contest))

(defun clim-token-claim (port seat &key press)
  "May SEAT drive McCLIM with the input now in hand?  Takes the token if that is what
   the input means, and answers T iff the input should reach McCLIM.

   PRESS says this input is a button press inside a CLIM window — the gesture that takes
   a token somebody else is holding.  Anything else (motion, a keystroke) takes only a
   FREE token; it will not pull CLIM's one pointer out from under whoever is using it."
  (let ((tok (glass-port-clim-token port)))
    (cond
      ((null seat) t)                                   ; the session speaking for itself
      ((and (eq seat (token-holder tok)) (eq (token-state tok) :held))
       (setf (token-touched tok) (token-now))           ; still driving
       t)
      ((eq (token-state tok) :free) (clim-token-take-free port seat))
      ((not press) nil)                                 ; somebody else is driving
      ((clim-token-pinned-p port)                       ; …and is mid-gesture: wait for it
       (setf (token-pending tok) seat)
       (incf (token-deferrals tok))
       nil)
      (t (clim-token-contest port seat)))))

;;; ---- letting go ----------------------------------------------------------------

(defun clim-token-release (port reason)
  "The token goes FREE.  REASON is for the counters and for anybody watching: :EXIT (the
   holder's pointer left every CLIM window), :IDLE, :STUCK, :GONE (its viewers went
   away).  Not a handoff — nobody is given anything; the next input of any kind takes it."
  (let ((tok (glass-port-clim-token port))
        (old (clim-token-holder port)))
    (when old
      (setf (token-state tok) :free (token-holder tok) nil)
      (incf (token-releases tok))
      (clim-token-repaint-indicator port (list old))
      ;; A pending seat was waiting on a gesture that has now stopped mattering: it did
      ;; ask, and the token is free, so let it have it rather than making it click again.
      (when-let ((want (token-pending tok)))
        (setf (token-pending tok) nil)
        (clim-token-take-free port want))
      reason)))

(defun clim-token-settle (port seat)
  "A gesture of SEAT's has ended (a button came up, a drag landed, a menu closed, a CLIM
   grab was dropped).  If a press from somebody else was deferred while it was in flight,
   the handoff happens NOW — which is the whole point of deferring it.

   The TOKEN moves; the press itself is not replayed.  Replaying it would deliver a click
   to coordinates the application has had a whole gesture to change under, which is worse
   than the click that did not land: the seat that asked is now driving and its next press
   is an ordinary one."
  (let ((tok (glass-port-clim-token port)))
    (when (and seat (eq seat (clim-token-holder port)))
      (when-let ((want (token-pending tok)))
        (unless (clim-token-pinned-p port)              ; another gesture still going?
          (setf (token-pending tok) nil)
          (clim-token-contest port want))))))

(defun clim-token-follow-pointer (port seat obj)
  "SEAT's pointer is over OBJ — a window, or NIL for the bare workspace.  If SEAT is
   driving McCLIM and has moved off every CLIM window, the token goes free: HELD is for
   somebody with their hand on it, and a pointer parked over a terminal is not that.
   Mid-gesture is exempt, because a CLIM pull-down is routinely tracked outside the
   window that posted it."
  (when (and seat (eq seat (clim-token-holder port))
             (not (and obj (not (wm-surface-p obj))))   ; not over a CLIM window
             (not (clim-token-pinned-p port)))
    (clim-token-release port :exit)))

(defun clim-token-idle-sweep (port)
  "Called once per desktop tick.  Frees a token whose holder has gone quiet, so that
   nobody holds it forever by walking away.  Costs one integer compare when the token is
   free, which is what an idle desktop's every tick is."
  (let ((tok (glass-port-clim-token port)))
    (when (eq (token-state tok) :held)
      (let ((age (token-age tok)))
        (cond
          ((not (clim-token-pinned-p port))
           (when (>= age *clim-token-idle*) (clim-token-release port :idle)))
          ;; Pinned, but pinned only by a button that has been down and silent for a
          ;; minute: the release event is never coming.  Drop the stale mask and free it.
          ((and (>= age *clim-token-stuck*)
                (let ((h (clim-token-holder port)))
                  (and h (logtest (seat-buttons h) 7)
                       (null (seat-grab-sheet h)) (null (seat-drag h)) (null (seat-menu h)))))
           (setf (seat-buttons (clim-token-holder port)) 0)
           (clim-token-release port :stuck)))))))

(defun clim-token-seat-gone (port seat)
  "SEAT's last viewer disconnected.  Anything it was in the middle of is over — there is
   nobody on the other end of the gesture — so the pin goes with it and the token is
   free for whoever is still here."
  (when (eq seat (clim-token-holder port))
    (setf (seat-buttons seat) 0 (seat-drag seat) nil (seat-menu seat) nil)
    (clim-token-release port :gone)))

;;; ---- the indicator --------------------------------------------------------------

(defun clim-token-repaint-indicator (port seats)
  (declare (ignore port))
  "Recomposite SEATS because who is driving changed and they draw CLIM title bars
   differently for it.  Nothing at all when the indicator is off, which is the default —
   the token changing hands must not cost a full-screen repaint to nobody's benefit."
  (when *clim-token-indicator*
    (dolist (s (remove-duplicates (remove nil seats)))
      (ignore-errors (composite-seat s)))))

(defun clim-token-show-indicator (port on)
  "Turn the holder indicator on or off on a RUNNING desktop and repaint every seat, so
   the switch can be flipped from a control socket and looked at."
  (setf *clim-token-indicator* (and on t))
  (dolist (s (glass-port-seats port)) (ignore-errors (composite-seat s)))
  *clim-token-indicator*)

;;; ---- what the token is doing, in one line ---------------------------------------

(defun clim-token-report (port)
  "A human-readable line about the token: for the control socket and the gates."
  (let ((tok (glass-port-clim-token port)))
    (format nil "~a~@[ by ~a~], idle ~,1fs~@[, ~a waiting~]; ~
                 takes ~d silent / ~d contested, ~d deferred, ~d released; ~
                 resync ~d moved of ~d examined"
            (token-state tok)
            (let ((h (clim-token-holder port))) (and h (seat-name h)))
            (token-age tok)
            (let ((p (token-pending tok))) (and p (seat-name p)))
            (token-silent-takes tok) (token-contests tok) (token-deferrals tok)
            (token-releases tok) (token-resyncs tok) (token-resync-scans tok))))
