;;;; src/wm/package.lisp — glass.wm: the McCLIM desktop's window manager, without McCLIM.
;;;;
;;;; backend/wm.lisp is a window manager written inside a McCLIM port.  Most of it never needed
;;;; McCLIM: the chrome is drawn with glass primitives and scribe, the compositing is blits and
;;;; clips, a drag is a CopyRect hint on FB-MARK-FRAME.  This is that window manager lifted out
;;;; of the port -- one seat, surface windows only -- so it runs where McCLIM does not (modus).
;;;;
;;;; The pieces, in load order:
;;;;   chrome.lisp  density, the OPEN LOOK title bar, blits, the menus' look
;;;;   wm.lisp      the window manager: windows, z-order, focus, compositing (damage, the move
;;;;                and scroll CopyRects, the wireframe), the pointer, menus, the tick
;;;;   serve.lisp   SERVE-WM: the network thread and one compositor thread, as SERVE-DESK

(defpackage #:glass.wm
  (:use #:cl)
  (:export
   ;; serving it
   #:serve-wm
   ;; embedding it (the same shape as glass.desk's MAKE-DESK / DESK-POINTER / DESK-TICK)
   #:make-wm #:wm-register-app #:wm-open #:wm-pointer #:wm-key #:wm-tick #:wm-redraw
   #:wm-fb #:wm-surfaces #:wm-apps #:wm-focus #:wm-keyboard-fn
   ;; tunables
   #:*wm-scale* #:*wm-bg* #:*drag-adaptive* #:*wireframe-queue-kb* #:*wireframe-lag-ms*
   #:*wm-scroll-copyrect* #:*wm-log*))
