;;;; src/wm/package.lisp — glass.wm: the McCLIM desktop's window manager, without McCLIM.
;;;;
;;;; backend/wm.lisp is a window manager written inside a McCLIM port.  Most of it never needed
;;;; McCLIM: the chrome is drawn with glass primitives and scribe, the compositing is blits and
;;;; clips, a drag is a CopyRect hint on FB-MARK-FRAME.  This is that window manager lifted out
;;;; of the port -- surface windows only -- so it runs where McCLIM does not (modus).  It has the
;;;; McCLIM desktop's cooperative SEATS (backend/seat.lisp): one session of windows, a screen,
;;;; hands and arrangement per person watching (wm.lisp; WM-SEAT-PORT in serve.lisp).
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
   #:serve-wm #:start-wm #:stop-wm #:*wm-apps* #:*wm-optional-apps* #:wm-available-apps
   ;; embedding it (the same shape as glass.desk's MAKE-DESK / DESK-POINTER / DESK-TICK)
   #:make-wm #:wm-register-app #:wm-register-action #:wm-actions #:wm-open #:wm-pointer #:wm-key #:wm-tick #:wm-redraw #:wm-resize-screen
   ;; the rack (rack.lisp): windows as rack units, for a phone
   #:wm-set-layout #:wm-layout #:wm-set-obscured #:wm-obscured #:wm-set-density #:wm-density #:*density* #:wm-wants-fullscreen #:wm-set-fullscreen #:wm-fullscreen #:*rack-units-landscape* #:*rack-min-unit* #:*rack-max-units* #:*rack-title-scale* #:*wm-title-scale*
   #:wm-fb #:wm-surfaces #:wm-apps #:wm-focus #:wm-keyboard-fn
   ;; seats: one person's screen, hands and arrangement of the session's windows
   #:wm-seat-port #:wm-session #:wm-session-port #:wm-session-seat-keys #:wm-session-seat-clipboard
   #:wm-seats #:wm-home #:wm-add-seat #:wm-find-seat #:*seat*
   #:seat #:seat-id #:seat-key #:seat-home-p #:seat-fb #:seat-focus #:seat-menu #:seat-drag
   #:seat-px #:seat-py #:seat-last-mask #:seat-clipboard #:seat-viewers #:seat-view-count
   ;; tunables
   #:*wm-scale* #:*wm-bg* #:*drag-adaptive* #:*wireframe-queue-kb* #:*wireframe-lag-ms*
   #:*wm-scroll-copyrect* #:*wm-log*))
