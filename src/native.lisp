;;;; src/native.lisp — a framebuffer in a native window: SERVE-NATIVE.
;;;;
;;;; SERVE-MULTI's place for a desktop that is not served over a wire: the same framebuffer,
;;;; the same ON-KEY / ON-POINTER callbacks with RFB's arguments, the same damage
;;;; (FB-TAKE-FRAME), shown in a window of the platform's own.  SERVE-DESK and SERVE-WM pick it
;;;; with :DISPLAY :NATIVE.
;;;;
;;;; The platform side is the host Lisp's, found by name so glass needs nothing to load it:
;;;; modus on Windows provides W32-WINDOW-OPEN / -PRESENT / -EVENTS / -CLOSE in MODUS-INT
;;;; (modus net/win32-window.lisp): a window the calling thread owns, a blit of a rectangle of an
;;;; X8R8G8B8 pixel vector, and the window's input as (:key DOWN KEYSYM) (:pointer MASK X Y)
;;;; (:paint) (:close).  Nothing else provides one yet; NATIVE-DISPLAY-P says whether this Lisp
;;;; can.
;;;;
;;;; THE FRAME IS READ UNDER THE FRAMEBUFFER'S LOCK, which the compositor holds while it
;;;; composites (WITH-FB-LOCKED): blitting without it showed half-drawn frames -- the old
;;;; picture above a line, the new one below -- whenever a drag went faster than a frame.
;;;;
;;;; RESIZING: the window's new client size goes to ON-RESIZE (W H); the host resizes FB
;;;; (SERVE-WM's compositor does) and the next frame is presented at FB's new size.
;;;;
;;;; GLASS_NATIVE_STATS=1: once a second, what the window was shown -- presents, pixels, and
;;;; the time spent presenting (the platform's W32-WINDOW-STATS) -- on *ERROR-OUTPUT*.

(in-package #:glass)

(export '(serve-native native-display-p native-display-dpi))

(defun %native-fn (name)
  ;; INTERN, not FIND-SYMBOL: modus interns its own functions' names lazily (the reader makes
  ;; the symbol when it first sees the name), so FIND-SYMBOL answers NIL for a name no source
  ;; has mentioned yet while INTERN returns the very symbol the function is defined under.
  ;; Only modus has a MODUS-INT package, so nothing is interned anywhere else.
  (let* ((p (find-package "MODUS-INT"))
         (s (and p (intern name p))))
    (and s (fboundp s) (symbol-function s))))

(defun native-display-p ()
  "True when this Lisp can show a framebuffer in a native window (SERVE-NATIVE)."
  (and (%native-fn "W32-WINDOW-OPEN") t))

(defun native-display-dpi ()
  "The display's DPI, 96 being 100% (the platform's W32-DISPLAY-DPI); 96 when there is none."
  (let ((f (%native-fn "W32-DISPLAY-DPI")))
    (or (and f (funcall f)) 96)))

(defun %damage-with-copy (damage copy)
  "DAMAGE ((x0 y0 x1 y1), :FULL or NIL) grown to cover a move COPY (sx sy dx dy w h): a
   window has no CopyRect, so the moved pixels are presented like any others."
  (if (null copy)
      damage
      (destructuring-bind (sx sy dx dy w h) copy
        (%fb-damage-union (%fb-damage-union damage (list sx sy (+ sx w) (+ sy h)))
                          (list dx dy (+ dx w) (+ dy h))))))

(defun serve-native (fb &key on-key on-pointer on-resize tick stop (title *desktop-name*)
                          (interval 1/60) device)
  "Show FB in a native window on this thread until it is closed or STOP (a predicate polled
   each turn) says so: the window's input goes to ON-KEY (DOWN KEYSYM) and ON-POINTER (MASK X
   Y) as SERVE-MULTI's does, a resize of the window to ON-RESIZE (W H), TICK (no arguments) runs
   each turn, and what the compositor marks (FB-MARK-FRAME) is presented.  DEVICE: FB is in the
   display's own pixels (the caller handles density, as SERVE-WM does); otherwise the platform
   magnifies it by the display's scale.  The window must be pumped by the thread that opened it, so
   this thread is the display.  Signals if there is no native display (NATIVE-DISPLAY-P)."
  (unless (eq (fb-format fb) :xrgb)
    (error "glass: a native window shows an :XRGB framebuffer, not ~s" (fb-format fb)))
  (let* ((open (or (%native-fn "W32-WINDOW-OPEN") (error "glass: no native display here")))
         (present (%native-fn "W32-WINDOW-PRESENT"))
         (events (%native-fn "W32-WINDOW-EVENTS"))
         (close (%native-fn "W32-WINDOW-CLOSE"))
         (stats (let ((v (sb-ext:posix-getenv "GLASS_NATIVE_STATS")))
                  (and v (plusp (length v)) (string/= v "0") (%native-fn "W32-WINDOW-STATS"))))
         (stats-at (get-internal-real-time))
         (w (fb-width fb)) (h (fb-height fb))
         (wait (max 1 (round (* 1000 interval))))
         (win (or (funcall open w h title device) (error "glass: the native window did not open"))))
    (declare (ignorable w h))
    ;; Under the lock, and at FB's size now: a resize replaces the pixels.
    (flet ((show (x0 y0 x1 y1)
             (with-fb-locked (fb)
               (funcall present win (fb-pixels fb) (fb-width fb) x0 y0 x1 y1)))
           (show-all ()
             (with-fb-locked (fb)
               (funcall present win (fb-pixels fb) (fb-width fb) 0 0 (fb-width fb) (fb-height fb)))))
      (format *error-output* "~&glass: native window (~dx~d)~%" w h)
      (force-output *error-output*)
      (unwind-protect
           (progn
             (show-all)
             (loop
               (when (and stop (funcall stop)) (return nil))
               (let ((closed nil))
                 (dolist (e (funcall events win wait))
                   (case (first e)
                     (:key (when on-key (funcall on-key (second e) (third e))))
                     (:pointer (when on-pointer (funcall on-pointer (second e) (third e) (fourth e))))
                     (:resize (when on-resize (funcall on-resize (second e) (third e))))
                     (:paint (show-all))
                     (:close (setf closed t))))
                 (when closed (return nil)))
               (when tick (funcall tick))
               (when (and stats (>= (- (get-internal-real-time) stats-at) internal-time-units-per-second))
                 (setf stats-at (get-internal-real-time))
                 (destructuring-bind (n px us) (funcall stats win)
                   (format *error-output* "~&glass: native: ~d presents, ~d Kpx, ~,1f ms presenting~%"
                           n (floor px 1000) (/ us 1000.0))
                   (force-output *error-output*)))
               ;; the damage taken and the pixels read under one hold of the lock, so the
               ;; rectangle shown is the frame the compositor marked
               (with-fb-locked (fb)
                 (multiple-value-bind (frame damage copy) (fb-take-frame fb)
                   (declare (ignore frame))
                   (let ((d (%damage-with-copy damage copy)))
                     (cond ((eq d :full) (show-all))
                           (d (destructuring-bind (x0 y0 x1 y1) d (show x0 y0 x1 y1)))))))))
        (funcall close win)))))
