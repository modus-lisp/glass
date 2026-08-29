;;;; package.lisp — the glass McCLIM backend (clim-glass).

(defpackage #:clim-glass
  (:use #:clim #:clim-lisp #:clim-backend)
  (:import-from #:climi #:maybe-funcall)
  (:import-from #:alexandria #:when-let #:when-let*)
  (:export #:glass-port
           #:find-glass-port
           #:run-frame
           #:run-wm                      ; a session, AND its home seat exposed on a wire
           #:run-session                 ; …just the session: nothing listens
           #:make-wm-session #:start-wm-session #:run-wm-loop   ; the three RUN-WM is made of
           #:seat-focus #:seat-focus-back #:seat-prev-focus
           #:seat-scale #:seat-ppem #:seat-metric  ; pixel density: see docs/density-and-colour.md
           #:*wm-scale* #:wm-titleh #:wm-border #:wm-size #:with-seat-scale
           #:register-app                ; register an external app in the root menu
           #:add-surface                 ; launch any external glass-surface app as a window
           ;; a seat is what you connect to; a transport is what carries it
           #:add-wm-seat #:port-seat #:seat-name #:seat-identity #:seat-npub
           ;; a viewer in THIS image: the seat's pixels and hands, with no wire
           #:attach-seat-local #:resize-seat-screen #:seat-wallpaper
           #:open-seat-transport #:close-seat-transport #:close-seat-transports
           #:seat-serving-p #:seat-transports #:transport-open-p
           #:transport-kind #:transport-address #:transport-port-num
           #:transport-path #:transport-endpoint #:seat-socket-path
           #:*seat-bind-address* #:*seat-transport-kind*
           ;; a credential belongs to ONE wire, not to the session
           #:transport-password #:transport-credential #:transport-authenticated-p
           ;; plain VNC on demand: the root-menu item's two verbs, and the policy
           #:serve-seat-vnc #:stop-seat-vnc #:seat-vnc-transport
           #:*seat-vnc-address* #:*vnc-password-file* #:vnc-password-file-credential
           #:local-address #:loopback-address-p
           ;; the wire you poke a running desktop with (backend/control.lisp)
           #:start-control-socket #:control-answer))
