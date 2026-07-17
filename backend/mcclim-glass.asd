;;;; mcclim-glass.asd — a McCLIM backend that renders into a glass framebuffer
;;;; and serves it over VNC.  Reuses mcclim-render for all drawing; the only
;;;; display-specific work is copying the rendered image into a glass fb and
;;;; translating RFB input into CLIM events.  Pure CL — no X, no FFI.

(asdf:defsystem "mcclim-glass"
  :description "McCLIM backend on the glass VNC server: run CLIM apps over VNC, no X."
  :author "ynniv"
  :license "MIT"
  :depends-on ("mcclim" "mcclim-render" "mcclim-raster-image" "glass" "sb-concurrency")
  :serial t
  :components ((:file "package")
               (:file "backend")
               (:file "wm")))
