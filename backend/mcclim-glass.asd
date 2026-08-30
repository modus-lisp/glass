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
               (:file "seat")
               (:file "clim-token")
               (:file "backend")
               (:file "message-port")
               (:file "wm")
               (:file "control")
               (:file "introspect")     ; source-of / where-is, over the control wire
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

(asdf:defsystem "mcclim-glass/remote"
  :description "Another glass desktop as a window on this one: glass/client's RFB
client wrapped as a wm-surface, plus its root-menu entry (\"Remote desktop\").  The
window manager cannot tell the result from a terminal — a remote desktop is a
framebuffer, an on-key, an on-pointer, a dirty-p and a copy-p, which is precisely
what a surface is.  The copy-p is the interesting one: a scroll inside the remote
arrives as an RFB CopyRect and leaves again as one, so the strip-blit optimisation
composes across the hop instead of stopping at it.

OPTIONAL and separate from the backend, like the speak and listen windows: it is an
application, and the only thing here that opens a socket to another desktop.
Deliberately carries neither clipboard nor audio across the boundary."
  :author "ynniv"
  :license "MIT"
  :depends-on ("mcclim-glass" "glass/client")
  :serial t
  :components ((:file "remote-app")))

(asdf:defsystem "mcclim-glass/climacs"
  :description "Climacs, with the upstream bugs corrected that kept the root menu's
\"Editor (Climacs)\" from ever becoming an editor you could type in and open files with.  None of
them is a glass bug and a CLX desktop would meet all of them: ESA repaints its minibuffer while
holding the output-history lock its own display loop took, and expires an old message the same
illegal way a second after putting it up; Climacs' OVERLAYING-PANE lays out its content pane
without resizing it; DIRECTORY-PATHNAME-P judges a path by its name and never by the disk; ESA's
command loop handles two conditions and lets every other one unwind the frame out of existence
with its window still on the screen; and every one of Climacs' three mouse translators asks for
the window in the parameter that a translator always fills with the presentation's object — which
for a blank area is the pointer event — so clicking in the text moved nothing at all.  Loading
THIS is how a desktop offers the menu
entry: it brings in climacs and the corrections together, so there is no way to get the editor
without them."
  :author "ynniv"
  :license "MIT"
  :depends-on ("mcclim-glass" "climacs")
  :serial t
  :components ((:file "climacs-compat")))

(asdf:defsystem "mcclim-glass/music"
  :description "A playlist, a transport and a clock — Winamp's arrangement, because it was the
right one.  It decodes nothing and plays nothing: reed decodes, spool's player turns that into
a source with a position, and the session mixer plays it to whoever is listening.  All three
existed already; what was missing was a way to point them at a file and a window to do it in.

OPTIONAL, and spool is found by name rather than depended on, so a desktop without it still
builds this window and the window says what is missing."
  :depends-on ("mcclim-glass")
  :serial t
  :components ((:file "music-app")))

(asdf:defsystem "mcclim-glass/mixer"
  :description "The session's audio as a window: every source by name, its level, its gain and
whether the session can hear it.  The WebRTC client's two RMS meters generalised to the machine
the mixer is on -- and diagnostic before it is a control, because a desktop that is silent
because nothing is playing looks exactly like one that is silent because something is broken.

OPTIONAL and separate from the backend, like its Speak and Listen siblings: a desktop without
audio should still build, and this window should then say so rather than not existing."
  :depends-on ("mcclim-glass")
  :serial t
  :components ((:file "mixer-app")))

(asdf:defsystem "mcclim-glass/listen"
  :description "The window speak is the mirror of: press Listen and the box fills up with what
the session is saying.  The desktop's ear (glass/hearing) behind a text box and three buttons, as
a McCLIM frame that registers itself in the workspace root menu.  OPTIONAL and separate from the
backend for the same reason speak is — it is an application, and the only thing here that drags
in a recognizer.

Carries the Dictate toggle, which is why it also depends on glass/dictation: the same ear, with
its words going to the FOCUSED WINDOW as keystrokes instead of into this window's box."
  :author "ynniv"
  :license "MIT"
  :depends-on ("mcclim" "glass/hearing" "glass/dictation")
  :serial t
  :components ((:file "listen-app")))
