;;;; climacs-gate.lisp — Climacs opens on the glass desktop.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive \
;;;;        --load backend/inspect/climacs-gate.lisp
;;;;
;;;; The root menu has offered "Editor (Climacs)" all along and it has never once
;;;; opened.  Five reasons, stacked one behind the other, each invisible until the
;;;; one in front of it was gone — and then, once it opened, four more that made
;;;; it die again a minute into being used, or quietly not do what you asked:
;;;;
;;;;   1. Nobody loaded Climacs.  WM-EDIT resolves it by name at click time, so a
;;;;      missing system is not a missing menu item — it is a menu item that does
;;;;      nothing and writes one line to a log nobody is reading.
;;;;
;;;;   2. Once loaded it died inside ADOPT-FRAME, before any window appeared:
;;;;
;;;;        There is no applicable method for TEXT-STYLE-ASCENT
;;;;        when called with (#<STANDARD-TEXT-STYLE :SANS-SERIF ...> #<BASIC-MEDIUM>)
;;;;
;;;;      McCLIM's recording streams hand out a "faux medium" — a bare BASIC-MEDIUM
;;;;      with the right port and no backend class — and every measurement inside
;;;;      McCLIM goes through the STREAM, which has its own methods.  ESA's
;;;;      minibuffer is the one caller that measures the MEDIUM instead
;;;;      (esa.lisp:151), and no backend specializes that class.  See the
;;;;      text-measurement section of backend.lisp.
;;;;
;;;;   3. ESA's DISPLAY-MINIBUFFER repaints while holding the output-history lock
;;;;      its own display loop just took, which McCLIM asserts against.  It fires
;;;;      from ESA-TOP-LEVEL's pre-loop REDISPLAY-FRAME-PANES, outside the command
;;;;      loop's handlers, so the thread dies there with the window already drawn.
;;;;
;;;;   4. Climacs' OVERLAYING-PANE lays out its content pane without resizing it,
;;;;      so the content keeps a bboard's 100x100 default and SHEET-NATIVE-REGION
;;;;      clips the whole buffer subtree: a blank white box.
;;;;
;;;;   5. Nothing ever moved the keyboard.  McCLIM routes keys by
;;;;      PORT-KEYBOARD-INPUT-FOCUS and on CLX the X server is what sets it; glass
;;;;      has no X server, so whichever application grabbed focus first kept it.
;;;;      WM-RAISE now posts a WINDOW-MANAGER-FOCUS-EVENT.
;;;;
;;;; Then, with an editor that opened and typed:
;;;;
;;;;   6. Every minibuffer message is a one-second fuse.  HANDLE-REPAINT on the
;;;;      minibuffer expires an old message with WINDOW-CLEAR, which dispatches a
;;;;      repaint from inside the repaint that is holding the pane's output
;;;;      history lock — the same assertion as 3, reached from the other side.
;;;;      Anything that says anything ("Quit" after C-g, "~A is not bound") kills
;;;;      the editor one second later, which is why it looked haunted.
;;;;
;;;;   7. DIRECTORY-PATHNAME-P judges by the name and not the disk, so C-x C-f on
;;;;      a completed directory with the slash rubbed out opens a directory as a
;;;;      file; SBCL signals a STREAM-ERROR, which is not the FILE-ERROR that
;;;;      COM-FIND-FILE guards against.
;;;;
;;;;   8. ESA's command loop handles two conditions and nothing else, so ANY of
;;;;      the above — or any other bug in any command — unwinds out of the frame's
;;;;      thread for good, leaving the window drawn and unattended.  That is what
;;;;      "it locked up" was: not a hang, a corpse.
;;;;
;;;;   9. Clicking in the text never moved the cursor.  Climacs' blank-area
;;;;      translator names its first parameter WINDOW, but a translator's first
;;;;      parameter is the presentation's OBJECT — and a blank-area presentation's
;;;;      object is the pointer EVENT.  COM-SWITCH-TO-THIS-WINDOW got an event
;;;;      where it wanted a pane, its guard failed, and the click was a silent
;;;;      no-op.  All three of Climacs' mouse translators have it.
;;;;
;;;;   3, 4, 6, 7, 8 and 9 are fixed in backend/climacs-compat.lisp (system
;;;;   MCCLIM-GLASS/CLIMACS), which is what this gate loads and what a desktop
;;;;   should load.
;;;;
;;;; The checks: the faux medium can be measured, it measures the SAME as a real
;;;; one, a medium that is not ours is still refused, and — the load-bearing ones —
;;;; a real Climacs reaches a real window on a real glass screen, is laid out to
;;;; the size of that window, takes the keyboard when raised, puts what is typed
;;;; at it into its buffer, is STILL doing all of that after an abort and after a
;;;; C-x C-f onto a directory, and moves point to the character you click on.
;;;; The frame runs in a thread whose condition is captured rather than left to
;;;; take the image down.
(require :asdf)
(load "~/quicklisp/setup.lisp")
(defparameter *climacs-loaded*
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency))
      (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
      (asdf:load-system :mcclim-glass)
      ;; MCCLIM-GLASS/CLIMACS, not bare :CLIMACS — that system is climacs plus the
      ;; two upstream corrections, and loading it here is the same thing a desktop
      ;; does.  Set CLIMACS_RAW=1 to load climacs WITHOUT them, which is how the
      ;; checks below were shown to fail.
      ;; optional on purpose: the gate reports the skip rather than pretending
      (handler-case
          (progn (asdf:load-system (if (sb-ext:posix-getenv "CLIMACS_RAW")
                                       :climacs :mcclim-glass/climacs))
                 t)
        (serious-condition (e) (format t "~&  (climacs will not load: ~a)~%" e) nil)))))
(in-package :clim-glass)

(defvar *fail* 0)
(defun check (ok fmt &rest args)
  (format t "  [~:[FAIL~;pass~]] ~?~%" ok fmt args) (unless ok (incf *fail*)))

(defun wait-until (pred &optional (secs 20))
  (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second))))
    (loop until (funcall pred) do (sleep 1/50)
          when (> (get-internal-real-time) end) do (return nil)
          finally (return t))))

;;; Reaching into Climacs by name, so that this file compiles in an image where
;;; Climacs would not load and the whole section SKIPs instead of failing to read.
(defun climacs-frame (port)
  "The one live CLIMACS frame on PORT, or NIL."
  (find-if (lambda (f) (and (search "CLIMACS" (string (class-name (class-of f))))
                            (climi::frame-panes f)))
           (climi::frame-manager-frames (find-frame-manager :port port))))

(defun find-overlaying-pane (frame)
  (let ((class (find-class (find-symbol "OVERLAYING-PANE" "CLIMACS-GUI") nil)))
    (labels ((walk (s)
               (or (and class (typep s class) s)
                   (some #'walk (sheet-children s)))))
      (and class (walk (climi::frame-panes frame))))))

(defun climacs-text (port)
  "The text in Climacs' current buffer, or NIL if it cannot be asked."
  (ignore-errors
   (let* ((frame (climacs-frame port))
          (buf (funcall (find-symbol "BUFFER" "DREI-BUFFER")
                        (funcall (find-symbol "VIEW" "CLIM")
                                 (funcall (find-symbol "ESA-CURRENT-WINDOW" "ESA") frame)))))
     (coerce (funcall (find-symbol "BUFFER-SEQUENCE" "DREI-BUFFER") buf 0
                      (funcall (find-symbol "SIZE" "DREI-BUFFER") buf))
             'string))))

(defun climacs-point (port)
  "The offset of point in Climacs' current view, or NIL."
  (ignore-errors
   (funcall (find-symbol "OFFSET" "DREI-BUFFER")
            (funcall (find-symbol "POINT" "DREI")
                     (funcall (find-symbol "VIEW" "CLIM")
                              (funcall (find-symbol "ESA-CURRENT-WINDOW" "ESA")
                                       (climacs-frame port)))))))

(defun click-at (port sheet mirror dx dy)
  "A full left click DX,DY pixels into SHEET's own area, addressed the way the RFB
   client does: screen coordinates, through GLASS-ON-POINTER."
  (let* ((reg (sheet-native-region sheet))
         (sx (round (+ (glass-mirror-x mirror) (bounding-rectangle-min-x reg) dx)))
         (sy (round (+ (glass-mirror-y mirror) (bounding-rectangle-min-y reg) dy))))
    (glass-on-pointer port 0 sx sy) (sleep 1/5)   ; move there
    (glass-on-pointer port 1 sx sy) (sleep 1/5)   ; press
    (glass-on-pointer port 0 sx sy) (sleep 1/2))) ; release

(defun sheet-under-p (sheet ancestor)
  (loop for s = sheet then (sheet-parent s)
        while s when (eq s ancestor) do (return t)))

;;; A second McCLIM window, which is the whole point.  One application on a port
;;; is never a focus test: ESA grabs the keyboard for its own minibuffer the
;;; moment it starts reading, so a lone Climacs ends up focused whether or not
;;; anything moved the focus there.  The bug is what happens to the SECOND window
;;; — and it needs a first one that took the keyboard the ordinary way, which any
;;; frame with an interactor does.
(define-application-frame gate-decoy () ()
  (:menu-bar nil)
  (:panes (io :interactor :height 80 :width 200))
  (:layouts (default io)))

(format t "~&[climacs on glass]~%")

(let ((port (make-instance 'glass-port :port 5962)))
  (setf (glass-port-wm-p port) t (glass-port-screen-w port) 1100 (glass-port-screen-h port) 700
        (glass-port-fb port) (glass:make-framebuffer 1100 700 +wm-teal+))
  (climi::restart-port port)

  ;; --- 1. the faux medium, built the way MAKE-CONTEXT-MEDIUM builds it ---------
  (let ((faux (make-instance 'climi::basic-medium :port port))
        (real (make-medium port nil)))
    (check (eq (class-of faux) (find-class 'climi::basic-medium))
           "a recording stream's medium is a bare BASIC-MEDIUM, not a backend one")
    ;; verbatim the call in ESA's minibuffer COMPOSE-SPACE that used to die
    (let ((h (ignore-errors (text-style-height (medium-merged-text-style faux) faux))))
      (check (and h (plusp h)) "ESA's (text-style-height (medium-merged-text-style m) m) answers: ~a" h))
    ;; --- 2. and answers the SAME as a real medium of the same port ------------
    ;; IGNORE-ERRORS throughout: before the fix these signal, and a gate that dies
    ;; on its second check never gets to say what else is broken.
    (let ((ts (make-text-style :sans-serif :roman :normal)))
      (check (eql (ignore-errors (text-style-ascent ts faux)) (text-style-ascent ts real))
             "ascent through the faux medium equals the real medium's")
      (check (eql (ignore-errors (text-style-descent ts faux)) (text-style-descent ts real))
             "...and descent")
      (check (equal (ignore-errors (multiple-value-list (text-size faux "the quick brown fox" :text-style ts)))
                    (multiple-value-list (text-size real "the quick brown fox" :text-style ts)))
             "...and TEXT-SIZE, to the value")
      ;; the ruler must not become the answer to what font was asked about
      (check (ignore-errors
              (/= (text-style-ascent (make-text-style :sans-serif :roman :huge) faux)
                  (text-style-ascent (make-text-style :sans-serif :roman :tiny) faux)))
             "the text style asked about is the one measured (not the ruler's own)")
      ;; --- 3. a medium that is not ours is refused exactly as before ----------
      (check (null (ignore-errors (text-style-ascent ts (make-instance 'climi::basic-medium))))
             "a BASIC-MEDIUM with no glass port still has no applicable method")
      (check (eq (glass-port-ruler port) (%port-ruler faux))
             "one ruler per port, made once")))

  ;; --- 4/5. the whole point: Climacs opens ------------------------------------
  (if (not cl-user::*climacs-loaded*)
      (format t "  [SKIP] Climacs itself — the system is not loadable here~%")
      (let ((err nil) (fm (find-frame-manager :port port)))
        (sb-thread:make-thread
         (lambda ()
           ;; HANDLER-BIND that DECLINES, not just HANDLER-CASE: an error ESA
           ;; catches and turns into a minibuffer message still means Climacs is
           ;; broken, and an error nobody catches parks the thread in the debugger
           ;; forever — alive, mirror on screen, first frame drawn, taking no
           ;; input.  That is what "it opens and does nothing" looks like, and
           ;; only a handler that sees the condition on the way UP can tell.
           (handler-case
               (handler-bind ((error (lambda (e) (unless err (setf err (princ-to-string e))))))
                 (let ((climi::*default-frame-manager* fm))
                   (funcall (find-symbol "CLIMACS" "CLIMACS") :new-process nil)))
             (serious-condition (e) (unless err (setf err (princ-to-string e))))))
         :name "climacs-gate")
        (let ((up (wait-until (lambda ()
                                (find-if (lambda (m)
                                           (and (glass-mirror-managed m)
                                                (equal "Climacs" (glass-mirror-title m))))
                                         (glass-port-mirrors port))))))
          (check up "a managed \"Climacs\" window is on the screen")
          ;; NB the window appears DURING adopt-frame, before the layout that used to
          ;; fail — so "a mirror exists" is not "Climacs opened", and asking about ERR
          ;; here would be asking before the answer exists.  Both real questions are
          ;; below, after the content wait has given the frame its 20 seconds.
          (when up
            (let* ((m (find-if (lambda (m) (equal "Climacs" (glass-mirror-title m)))
                               (glass-port-mirrors port)))
                   (fb (glass-port-fb port))
                   (px (glass:fb-pixels fb))
                   (inks 0))
              (flet ((count-inks ()
                       (composite-all port)      ; nothing drives the tick loop in here
                       (let ((h (make-hash-table)))
                         (loop for yy from (+ (glass-mirror-y m) 2) below (+ (glass-mirror-y m) 120)
                               do (loop for xx from (+ (glass-mirror-x m) 2)
                                          below (min (glass:fb-width fb) (+ (glass-mirror-x m) 300))
                                        do (incf (gethash (logand (aref px (+ (* yy (glass:fb-width fb)) xx))
                                                                  #xffffff)
                                                          h 0))))
                         (setf inks (hash-table-count h)))))
                ;; a window that exists but has nothing in it is not an editor.  The
                ;; title bar is ours, so this looks only inside the CONTENT, where the
                ;; menu bar alone (File Macros Windows Help, antialiased) is far more
                ;; than the flat fill a bare mirror would show.
                (wait-until (lambda () (> (count-inks) 8)))
                (check (> inks 8)
                       "the window has Climacs' own content drawn in it (~d distinct inks)" inks))

              ;; --- 6. laid out to the size of the window, not to 100x100 --------
              ;; The OVERLAYING-PANE bug is silent: a Climacs clipped to a corner
              ;; still runs, still takes keys, and still answers every other check
              ;; here.  Only its geometry says so.  Asking the CONTENT pane, since
              ;; that is the sheet whose region was never set and whose native
              ;; region therefore clipped everything under it.
              (let* ((frame (climacs-frame port))
                     (overlay (and frame (find-overlaying-pane frame))))
                (if (null overlay)
                    (check nil "found Climacs' OVERLAYING-PANE to measure")
                    (multiple-value-bind (x1 y1 x2 y2)
                        (bounding-rectangle*
                         (sheet-region (funcall (find-symbol "CONTENT-PANE" "CLIMACS-GUI")
                                                overlay)))
                      (declare (ignore x1 y1))
                      (multiple-value-bind (ox1 oy1 ox2 oy2)
                          (bounding-rectangle* (sheet-region overlay))
                        (declare (ignore ox1 oy1))
                        (check (and (= (round x2) (round ox2)) (= (round y2) (round oy2)))
                               "the content pane fills the overlaying pane (~dx~d of ~dx~d)"
                               (round x2) (round y2) (round ox2) (round oy2))))))

              ;; --- 7. raising it TAKES the keyboard off another window ---------
              ;;
              ;; CLIM applications move PORT-KEYBOARD-INPUT-FOCUS for themselves,
              ;; through WITH-INPUT-FOCUS — ESA around its whole top level
              ;; (esa.lisp:1076), DEFAULT-FRAME-TOP-LEVEL around every command read
              ;; (frames.lisp:488).  That is an UNWIND-PROTECT that restores the
              ;; previous focus on the way out, so two running frames trade the
              ;; keyboard back and forth on their own and neither ever asked to.
              ;; Which one has it at any instant is a race, and a gate cannot ask a
              ;; race a question.
              ;;
              ;; So the decoy is brought up, allowed to realize a window, and then
              ;; STOPPED — after which the focus is put on it deliberately and
              ;; nothing in the image is moving it any more.  That is the state a
              ;; window manager exists to get out of: some other window owns the
              ;; keyboard, you click on this one, and this one should now have it.
              ;; On CLX the X server says so.  Here nobody did, and the window you
              ;; clicked stayed a window you could only look at.
              (let ((decoy (make-application-frame 'gate-decoy :frame-manager fm))
                    (thread nil))
                (setf thread (sb-thread:make-thread
                              (lambda () (ignore-errors (run-frame-top-level decoy)))
                              :name "gate-decoy"))
                (let ((io (wait-until (lambda () (find-pane-named decoy 'io)))))
                  (check io "a second McCLIM window came up to hold the keyboard")
                  (when io
                    (let ((pane (find-pane-named decoy 'io)))
                      (ignore-errors (sb-thread:terminate-thread thread))
                      (sleep 1/2)
                      (setf (climi::port-keyboard-input-focus port) pane)
                      (check (eq (climi::port-keyboard-input-focus port) pane)
                             "...and holds it, with nothing else contending")
                      (wm-raise port m)
                      (wait-until (lambda ()
                                    (let ((f (climi::port-keyboard-input-focus port)))
                                      (and f (sheet-under-p f (glass-mirror-sheet m)))))
                                  5)
                      (let ((focus (climi::port-keyboard-input-focus port)))
                        (check (and focus (sheet-under-p focus (glass-mirror-sheet m)))
                               "raising Climacs takes the keyboard back (focus: ~a)"
                               focus))))))

              ;; --- 8. and what is typed at it lands in its buffer --------------
              ;; The one check that exercises the whole path at once: RFB keysym ->
              ;; GLASS-ON-KEY -> port queue -> DISTRIBUTE-EVENT -> the focused sheet
              ;; -> ESA's command loop -> self-insert.  Reading the buffer rather
              ;; than the screen, because pixels cannot tell an editor that took the
              ;; keystroke from one that merely redrew.
              (let ((probe "glass"))
                (loop for c across probe
                      do (glass-on-key port t (char-code c)) (sleep 1/40)
                         (glass-on-key port nil (char-code c)) (sleep 1/40))
                (wait-until (lambda () (search probe (or (climacs-text port) ""))) 10)
                (let ((text (climacs-text port)))
                  (check (search probe (or text "")) "typing ~s reaches the buffer (~s)"
                         probe text)))

              ;; --- 9. a directory is a directory even without the slash --------
              ;; DIRECTORY-PATHNAME-P asks the NAME, not the disk, so completing
              ;; a directory in C-x C-f and rubbing the trailing slash out turns
              ;; it into a file as far as Climacs is concerned — and opening a
              ;; directory as a file on SBCL signals a STREAM-ERROR, which is not
              ;; the FILE-ERROR COM-FIND-FILE is watching for.
              ;; The directory we ask about has to be one that really exists, so
              ;; use this checkout's own root — named with the trailing slash and
              ;; without it — rather than a path from the box this was written on.
              (let* ((dp (find-symbol "DIRECTORY-PATHNAME-P" "CLIMACS-CORE"))
                     (root/ (truename
                             (merge-pathnames
                              "../../" (make-pathname :name nil :type nil
                                                      :defaults *load-truename*))))
                     (dirs (pathname-directory root/))
                     (root (make-pathname :directory (butlast dirs)
                                          :name (car (last dirs)) :type nil
                                          :defaults root/)))
                (check (and dp (funcall dp root))
                       "an existing directory named without a slash is a directory")
                (check (and dp (funcall dp root/))
                       "...and with one")
                (check (and dp (not (funcall dp (or *load-pathname* #p"/etc/hostname"))))
                       "...and an existing FILE is not")
                (check (and dp (not (funcall dp (merge-pathnames "no-such-thing-here" root/))))
                       "...and a name that is not on disk is still judged by its name"))

              ;; --- 10. a minibuffer message is not a one-second fuse -----------
              ;; C-g puts "Quit" up.  A second later HANDLE-REPAINT on the
              ;; minibuffer expires it with WINDOW-CLEAR, which dispatches a
              ;; repaint from inside a repaint that holds the output history
              ;; lock, which McCLIM asserts against.  So: abort, wait past the
              ;; expiry, then check the editor is still an editor.  Waiting well
              ;; past *MINIMUM-MESSAGE-TIME* because the comparison is in whole
              ;; universal-time seconds.
              (let ((probe "quitprobe"))
                (glass-on-key port t 65507)                    ; Control_L
                (glass-on-key port t (char-code #\g)) (sleep 1/40)
                (glass-on-key port nil (char-code #\g))
                (glass-on-key port nil 65507)
                (sleep 3)
                (loop for c across probe
                      do (glass-on-key port t (char-code c)) (sleep 1/40)
                         (glass-on-key port nil (char-code c)) (sleep 1/40))
                (wait-until (lambda () (search probe (or (climacs-text port) ""))) 10)
                (check (search probe (or (climacs-text port) ""))
                       "C-g, then a second of nothing, and the editor still takes keys"))

              ;; --- 11. and C-x C-f on a directory does not end the editor ------
              ;; The whole path at once: the prompt pre-fills with
              ;; (directory-of-current-buffer), so typing "glass" onto it is
              ;; exactly "complete a directory, delete the slash".  What we are
              ;; asking is not that it opens — it must not — but that the frame
              ;; is still there afterwards, which before fix 4 it was not: the
              ;; error unwound past the command loop and left the window on the
              ;; screen with nobody behind it.
              (let ((probe "stillhere"))
                (glass-on-key port t 65507)
                (glass-on-key port t (char-code #\x)) (sleep 1/40)
                (glass-on-key port nil (char-code #\x)) (sleep 1/40)
                (glass-on-key port t (char-code #\f)) (sleep 1/40)
                (glass-on-key port nil (char-code #\f))
                (glass-on-key port nil 65507)
                (sleep 2)
                (loop for c across "glass"
                      do (glass-on-key port t (char-code c)) (sleep 1/40)
                         (glass-on-key port nil (char-code c)) (sleep 1/40))
                (glass-on-key port t 65293) (sleep 1/40)       ; Return
                (glass-on-key port nil 65293)
                (sleep 3)
                (loop for c across probe
                      do (glass-on-key port t (char-code c)) (sleep 1/40)
                         (glass-on-key port nil (char-code c)) (sleep 1/40))
                (wait-until (lambda () (search probe (or (climacs-text port) ""))) 10)
                (check (search probe (or (climacs-text port) ""))
                       "C-x C-f on a directory name leaves an editor that still types"))

              ;; --- 12. clicking in the text moves point ------------------------
              ;; Climacs is the only layer that has click-to-move-point at all:
              ;; Drei has no pointer hit-testing and ESA only routes.  Its
              ;; translator names the first (positional) parameter WINDOW, but a
              ;; translator's first parameter is always the presentation's OBJECT,
              ;; and a blank-area presentation's object is the pointer EVENT.  So
              ;; COM-SWITCH-TO-THIS-WINDOW was handed an event where it wanted the
              ;; pane, BUFFER-PANE-P said no, and the click did nothing at all —
              ;; no error, no message, just a cursor that stays where it was.
              ;;
              ;; Everything typed above went onto one line, so a column is an
              ;; offset and we can ask for a specific one.  Addressed in SCREEN
              ;; coordinates through GLASS-ON-POINTER, which is the same entry
              ;; the RFB client uses.
              (let* ((frame (climacs-frame port))
                     (win (and frame (funcall (find-symbol "ESA-CURRENT-WINDOW" "ESA") frame)))
                     (cw (and win (ignore-errors (stream-character-width win #\m))))
                     (before (climacs-point port)))
                (if (not (and win cw (plusp cw) before (plusp before)))
                    (check nil "a buffer pane with text in it to click into (point ~a, char width ~a)"
                           before cw)
                    (progn
                      (click-at port win m 2 4)
                      (let ((after (climacs-point port)))
                        (check (eql after 0)
                               "clicking the first column moves point there (~a -> ~a)"
                               before after))
                      ;; ...and to WHERE you clicked, not merely somewhere
                      (click-at port win m (+ (* 5 cw) 2) 4)
                      (let ((after (climacs-point port)))
                        (check (eql after 5)
                               "clicking five characters in puts point at offset 5 (got ~a)"
                               after)))))))
          ;; Last, because it is the only check whose answer arrives late: either
          ;; branch above has already spent its 20 seconds, so by here the frame has
          ;; either come up or died trying.
          (check (null err) "Climacs opened without signalling~@[ — ~a~]" err))))

  (sb-concurrency:send-message (glass-port-mailbox port) (lambda () nil)))

(format t "~%=> ~:[PASS~;FAIL (~d)~]~%" (plusp *fail*) *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
