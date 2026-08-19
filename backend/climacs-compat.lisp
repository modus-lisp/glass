;;;; climacs-compat.lisp — the upstream bugs that stood between the root menu's
;;;; "Editor (Climacs)" and an editor you can type in and open files with.
;;;;
;;;; Neither is a glass bug.  Both are in code that ships with McCLIM/Climacs and
;;;; both would bite a CLX desktop the same way; they have simply never been hit
;;;; because the paths that reach them are the ones a *second* window on a *real*
;;;; window manager takes.  They are corrected here rather than in the quicklisp
;;;; trees so that an image built from source has them, and so that what was
;;;; changed and why is written down next to the thing that needed it.
;;;;
;;;; Loaded by the OPTIONAL system MCCLIM-GLASS/CLIMACS, for the same reason
;;;; /speak and /listen are optional: a desktop without an editor should still
;;;; get a backend.

(in-package :clim-glass)

;;; ---- 1. ESA repaints the minibuffer while holding its own history lock -----
;;;
;;; ESA's minibuffer is :display-time :command-loop with DISPLAY-MINIBUFFER as
;;; its display function, and upstream that function is one line:
;;;
;;;   (defun display-minibuffer (frame pane) (dispatch-repaint pane +everywhere+))
;;;
;;; A display function runs inside DO-REDISPLAY-PANE, which wraps it in
;;; WITH-STREAM-HISTORY-LOCKED.  McCLIM's own DISPATCH-REPAINT :BEFORE method on
;;; OUTPUT-RECORDING-STREAM then asserts that the caller does NOT hold that lock
;;; — "Check an invariant to avoid deadlocks with the mirror" — so the call is a
;;; guaranteed error every time it is reached.
;;;
;;; It is reached from ESA-TOP-LEVEL's (redisplay-frame-panes frame :force-p t),
;;; the one *before* the command loop and therefore outside the RESTART-CASE that
;;; would otherwise have turned it into a minibuffer complaint.  Nothing catches
;;; it, so on a debugger-enabled image the frame's thread parks in the debugger
;;; with its first frame already drawn: a window that is on screen, is not an
;;; error dialog, and will never read a key.  That is what "Climacs opens and
;;; does nothing" looked like from the outside.
;;;
;;; The repaint is redundant, not merely ill-timed: DO-REDISPLAY-PANE ends with
;;; (dispatch-repaint scroller-or-pane +everywhere+) itself, after the lock is
;;; released.  So the fix is to skip it exactly when we are the owner — which
;;; leaves any OTHER caller of DISPLAY-MINIBUFFER doing what it always did.
(defun esa::display-minibuffer (frame pane)
  (declare (ignore frame))
  (unless (eq (climi::output-history-lock-owner (stream-output-history pane))
              (clim-sys:current-process))
    (dispatch-repaint pane +everywhere+)))

;;; ---- 2. Climacs' OVERLAYING-PANE lays out its child and not itself ---------
;;;
;;; OVERLAYING-PANE (climacs/typeout.lisp) is a BBOARD-PANE holding a content
;;; pane and an optional overlay.  Upstream ALLOCATE-SPACE passes the space it
;;; was given straight down:
;;;
;;;   (allocate-space (content-pane pane) width height)
;;;
;;; which tells the content pane how to arrange ITS children but never resizes
;;; the content pane.  A bboard child keeps whatever region it was made with, and
;;; COMPOSE-SPACE for a bboard defaults to 100x100 — so the content pane stays
;;; 100x100 inside a 900x400 window, and since SHEET-NATIVE-REGION intersects
;;; down the hierarchy, that 100x100 clips the entire buffer subtree beneath it.
;;; Everything below the first hundred pixels is drawn and thrown away: a Climacs
;;; that is running fine and shows a blank white box.
;;;
;;; A window manager is what exposes this.  Come up at exactly the requested size
;;; and the region a pane was born with may happen to be right; get resized once
;;; — or laid out by a frame that computes its own geometry — and it is not.
;;;
;;; MOVE-AND-RESIZE-SHEET, then ALLOCATE-SPACE: the child needs to BE the size
;;; before it can arrange anything inside that size.
(defmethod allocate-space ((pane climacs-gui::overlaying-pane) width height)
  (let ((content (climacs-gui::content-pane pane)))
    (climi::move-and-resize-sheet content 0 0 width height)
    (allocate-space content width height))
  (let ((overlay (climacs-gui::overlay-tree pane)))
    (when overlay
      (let ((h (space-requirement-height (compose-space overlay))))
        (climi::move-and-resize-sheet overlay 0 0 width h)
        (allocate-space overlay width h)))))

;;; ---- 3. "Is this a directory?" is asked of the name, not of the disk -------
;;;
;;; DIRECTORY-PATHNAME-P (climacs/core.lisp, adapted from cl-fad) is purely
;;; syntactic: a pathname is a directory iff it has no name and no type.  So
;;; #P"/some/dir/" is a directory and #P"/some/dir" is a file,
;;; regardless of what is actually on disk.
;;;
;;; C-x C-f completes directories WITH the trailing slash, so the common way to
;;; land on the second form is to complete one and then rub the slash out —
;;; which is exactly what you do when you meant to type a file name next and
;;; changed your mind.  FIND-FILE-IMPL then takes the not-a-directory branch,
;;; PROBE-FILE says the path exists so it is not a new file, and it opens it:
;;;
;;;   (with-open-file (stream filepath :direction :input) ...)
;;;
;;; On SBCL that OPEN succeeds — you may open a directory — and the first READ
;;; fails with SB-INT:SIMPLE-STREAM-ERROR "Is a directory".  That is a
;;; STREAM-ERROR, NOT a FILE-ERROR, so COM-FIND-FILE's (handler-case ...
;;; (file-error ...)) does not catch it either.  See fix 4 for where it lands.
;;;
;;; Ask the filesystem when the name alone doesn't settle it.  PROBE-FILE
;;; returns a truename with a null NAME for a directory on every implementation
;;; we care about, so the same syntactic test applied to the TRUENAME is the
;;; answer.  Non-existent paths and unreadable ones fall back to the old
;;; behaviour, which is what the save/write-buffer callers want.
(defun climacs-core:directory-pathname-p (pathspec)
  "Return non-NIL if PATHSPEC designates a directory, by name or on disk."
  (flet ((no-file-part-p (p)
           (let ((name (pathname-name p))
                 (type (pathname-type p)))
             (and (or (null name) (eql name :unspecific))
                  (or (null type) (eql type :unspecific))))))
    (or (no-file-part-p pathspec)
        (let ((truename (ignore-errors (probe-file pathspec))))
          (and truename (no-file-part-p truename))))))

;;; ---- 4. One bad command kills the editor for good --------------------------
;;;
;;; ESA-TOP-LEVEL's command loop handles exactly two conditions,
;;; UNBOUND-GESTURE-SEQUENCE and ABORT-GESTURE.  Anything else — any bug in any
;;; command, in any syntax module, in anything a command calls — unwinds past
;;; the loop, past RUN-FRAME-TOP-LEVEL, and out of the frame's thread.
;;;
;;; Nothing tears the window down when that happens, because as far as the
;;; window manager is concerned the mirror is still realized.  You get a Climacs
;;; that is on screen, redraws when you drag it, and never responds to another
;;; key: "it locked up".  The editor is not locked up; it is gone, and only its
;;; picture is left.  Fix 3 removes the specific bug that got here most often,
;;; but the failure MODE is the more serious problem — every remaining bug in
;;; Climacs is a permanent one.
;;;
;;; Emacs shows the error in the echo area and reads the next key.  Do that.
;;; The loop body is three calls; two of them are generic functions we can wrap
;;; without touching the DEFINE-ESA-TOP-LEVEL macro, and between them they are
;;; where all the work happens.  PROCESS-GESTURES-OR-COMMAND reads the gestures,
;;; parses the arguments and executes the command; REDISPLAY-FRAME-PANE draws.
;;; A failure in either loses that command or that frame's worth of drawing and
;;; nothing else.
;;;
;;; This catches SERIOUS-CONDITION only.  ABORT-GESTURE, UNBOUND-GESTURE-
;;; SEQUENCE, FRAME-EXIT and FRAME-LAYOUT-CHANGED are all plain CONDITIONs in
;;; McCLIM, so C-g, an unbound key, C-x C-c and a relayout still travel to the
;;; handlers that are meant to see them.
(defmethod esa:process-gestures-or-command :around ((frame climacs-gui::climacs))
  (handler-case (call-next-method)
    (serious-condition (c)
      (ignore-errors (esa:display-message "~A" c))
      (ignore-errors (beep))
      (ignore-errors (redisplay-frame-panes frame)))))

;;; Upstream already treats a failed redisplay as recoverable — REDISPLAY-FRAME-
;;; PANE :AROUND offers CLEAR-PANE-TRY-AGAIN / CLEAR-PANE / SKIP-REDISPLAY — but
;;; only to a human at a debugger.  With no debugger attached the restarts are
;;; decoration and the frame dies.  Take SKIP-REDISPLAY automatically.
(defmethod redisplay-frame-pane :around
    ((frame climacs-gui::climacs) pane &key force-p)
  (declare (ignore pane force-p))
  (handler-case (call-next-method)
    (serious-condition (c)
      (ignore-errors (esa:display-message "~A" c)))))

;;; ---- 5. Every minibuffer message is a one-second fuse ----------------------
;;;
;;; This is the one that made Climacs feel haunted: it would work, then stop
;;; dead, then work again the next time you started it, with no pattern that
;;; had anything to do with what you were typing.
;;;
;;; ESA's minibuffer messages expire.  HANDLE-REPAINT on MINIBUFFER-PANE checks
;;; whether the current message is older than *MINIMUM-MESSAGE-TIME* (one
;;; second) and, if so, clears it:
;;;
;;;   (when (and (message pane) (> (get-universal-time) (+ 1 (message-time pane))))
;;;     (window-clear pane)
;;;     (setf (message pane) nil))
;;;
;;; WINDOW-CLEAR on a CLIM-STREAM-PANE ends in WINDOW-REFRESH, and WINDOW-REFRESH
;;; ends in (dispatch-repaint pane +everywhere+).  But HANDLE-REPAINT is only
;;; ever called from REPAINT-SHEET, whose :AROUND method for an
;;; OUTPUT-RECORDING-STREAM is holding that pane's output history lock — and
;;; DISPATCH-REPAINT's :BEFORE method asserts the caller does not hold it.  So
;;; the expiry path is an unconditional error, every time it is taken.
;;;
;;; The fuse is lit by anything that puts a message up: "Quit" after C-g, "~A is
;;; not bound" after a chord you didn't mean, "~A is a directory name.", a saved
;;; file.  A second later the next repaint of the minibuffer detonates it and,
;;; via the bug in fix 4, the editor is gone but still on screen.  Type nothing
;;; unusual and nothing happens; hit one wrong key and the editor dies a second
;;; later, which reads as "it locked up" and looks completely random.
;;;
;;; We are already inside a repaint of this pane and the caller draws the
;;; history when we return, so the re-dispatch is redundant as well as illegal.
;;; Do the other four fifths of WINDOW-CLEAR and let the repaint in progress
;;; paint the now-empty pane.  When the lock is NOT ours — nobody in McCLIM
;;; calls HANDLE-REPAINT that way, but methods are public — defer to upstream.
(defmethod esa::handle-repaint ((pane esa::minibuffer-pane) region)
  (declare (ignore region))
  (when (and (esa::message pane)
             (> (get-universal-time)
                (+ esa::*minimum-message-time* (esa::message-time pane))))
    (if (eq (climi::output-history-lock-owner (stream-output-history pane))
            (clim-sys:current-process))
        (progn (stream-clear-output-history pane)
               (window-erase-viewport pane)
               (scroll-extent pane 0 0)
               (change-space-requirements pane))
        (window-clear pane))
    (setf (esa::message pane) nil))
  (call-next-method))

;;; ---- 6. Clicking in the text does nothing, because "window" is the event ---
;;;
;;; Climacs DOES have click-to-move-point, and it is the only layer that does:
;;; Drei has no pointer hit-testing at all (its one pointer method is shift-
;;; middle paste on the gadget), and ESA only routes.  Climacs defines
;;; COM-SWITCH-TO-THIS-WINDOW (window-commands.lisp) plus a translator from the
;;; BLANK-AREA presentation type, and CLICK-TO-OFFSET to turn (x, y) into a
;;; buffer offset by counting lines from the view's TOP mark.
;;;
;;; The translator is written:
;;;
;;;   (define-presentation-to-command-translator blank-area-to-switch-to-this-window
;;;       (blank-area com-switch-to-this-window window-table :echo nil)
;;;       (window x y)
;;;     (list window x y))
;;;
;;; but a translator arglist is (OBJECT &key presentation context-type frame
;;; event window x y) — only the FIRST name is positional, and it always gets
;;; the presentation's object no matter what it is called.  MAKE-TRANSLATOR-FUN
;;; builds exactly (lambda (window &key x y &allow-other-keys) ...), so X and Y
;;; arrive correctly by keyword and WINDOW is the object.
;;;
;;; And the object of a blank-area presentation is the POINTER EVENT
;;; (MAKE-BLANK-AREA-PRESENTATION, standard-presentations.lisp: :object event).
;;; So COM-SWITCH-TO-THIS-WINDOW is handed a POINTER-BUTTON-PRESS-EVENT where it
;;; wants the pane, (buffer-pane-p window) is NIL, its WHEN never fires, and the
;;; click is silently a no-op.  Nothing errors; point simply never moves — which
;;; is why this one has no symptom other than "clicking doesn't do anything".
;;;
;;; Ask for the window by its keyword.  All three of Climacs' blank-area
;;; translators have the same shape and the same bug, so all three are restated:
;;; left click moves point, right click sets the mark and copies, middle click
;;; yanks where you pointed.
(define-presentation-to-command-translator
    climacs-commands::blank-area-to-switch-to-this-window
    (blank-area climacs-commands::com-switch-to-this-window climacs-gui::window-table
     :echo nil)
    (event window x y)
  (list window x y))

(define-presentation-to-command-translator
    climacs-commands::blank-area-to-mouse-save
    (blank-area climacs-commands::com-mouse-save climacs-gui::window-table
     :echo nil :gesture :select-other)
    (event window x y)
  (list window x y))

(define-presentation-to-command-translator
    climacs-commands::blank-area-to-yank-here
    (blank-area climacs-commands::com-yank-here climacs-gui::window-table
     :echo nil :gesture :middle-button)
    (event window x y)
  (list window x y))
