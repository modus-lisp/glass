;;;; package.lisp — the glass McCLIM backend (clim-glass).

(defpackage #:clim-glass
  (:use #:clim #:clim-lisp #:clim-backend)
  (:import-from #:climi #:maybe-funcall)
  (:import-from #:alexandria #:when-let #:when-let*)
  (:export #:glass-port
           #:find-glass-port
           #:run-frame
           #:run-wm
           #:register-app                ; register an external app in the root menu
           #:add-surface))               ; launch any external glass-surface app as a window
