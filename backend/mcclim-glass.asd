;;;; mcclim-glass.asd — a McCLIM backend that renders into a glass framebuffer
;;;; and serves it over VNC.  Reuses mcclim-render for all drawing; the only
;;;; display-specific work is copying the rendered image into a glass fb and
;;;; translating RFB input into CLIM events.  Pure CL — no X, no FFI.

(asdf:defsystem "mcclim-glass"
  :description "McCLIM backend on the glass VNC server: run CLIM apps over VNC, no X."
  :author "ynniv"
  :license "MIT"
  :depends-on ("mcclim" "mcclim-render" "glass" "glass/text" "glass/term" "sb-concurrency")
  :serial t
  :components ((:file "package")
               (:file "render-fix")
               (:file "backend")
               (:file "message-port")
               (:file "wm")
               (:file "compositor")))

(asdf:defsystem "mcclim-glass/speak"
  :description "A window to type into: the desktop's voice (glass/speech) behind a text box and
a Speak button, as a McCLIM frame that registers itself in the workspace root menu.  OPTIONAL and
separate from the backend — it is an application, and it is the only thing here that drags in a
speech engine, so a desktop without a voice still loads the backend it draws with."
  :author "ynniv"
  :license "MIT"
  :depends-on ("mcclim" "glass/speech")
  :serial t
  :components ((:file "speak-app")))

(asdf:defsystem "mcclim-glass/listen"
  :description "The window speak is the mirror of: press Listen and the box fills up with what
the session is saying.  The desktop's ear (glass/hearing) behind a text box and three buttons, as
a McCLIM frame that registers itself in the workspace root menu.  OPTIONAL and separate from the
backend for the same reason speak is — it is an application, and the only thing here that drags
in a recognizer."
  :author "ynniv"
  :license "MIT"
  :depends-on ("mcclim" "glass/hearing")
  :serial t
  :components ((:file "listen-app")))
