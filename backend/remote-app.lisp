;;;; remote-app.lisp — another glass desktop, as a window on this one.
;;;;
;;;; glass-client (src/rfb-client.lisp) turns a remote RFB server into a local
;;;; framebuffer plus an on-key and an on-pointer.  A wm-surface is a framebuffer
;;;; plus an on-key, an on-pointer, a dirty-p and a copy-p.  So this file is mostly
;;;; the observation that those are the same thing: it is the five thunks and the
;;;; menu entry, and the window manager cannot tell the resulting window from a
;;;; terminal.
;;;;
;;;; Two things are worth more than the wiring.
;;;;
;;;; NESTED COPYRECT.  A scroll inside the remote desktop arrives here as an RFB
;;;; CopyRect; the client applies it to our framebuffer and leaves the translation
;;;; on it as a hint; this surface hands the hint to the compositor as COPY-P; the
;;;; compositor turns it into a screen-space CopyRect, moves the strip on the screen
;;;; instead of re-blitting the window, and puts a CopyRect on ITS OWN RFB update.
;;;; The optimisation therefore composes: a scroll two desktops away costs one strip
;;;; per hop rather than a frame per hop, and would keep composing down a chain of
;;;; them.  Nothing here had to be taught about scrolling — COPY-P was already the
;;;; word for "my content translated", and a remote desktop simply has an answer.
;;;;
;;;; INPUT IS THE IDENTITY.  A wm-surface's on-key gets an RFB keysym and its
;;;; on-pointer gets an RFB button mask in coordinates the WM has already made
;;;; surface-local.  The remote's KeyEvent wants an RFB keysym and its PointerEvent
;;;; wants an RFB button mask in screen coordinates — and the surface IS the remote's
;;;; screen.  There is no keymap and no scaling anywhere in this path; the only thing
;;;; done to a coordinate is a clamp, so the remote is never told about a pixel it
;;;; does not have.

(defpackage #:glass-remote
  (:use #:cl)
  (:local-nicknames (#:gc #:glass-client))
  (:documentation
   "A remote glass desktop as a window on the local one: the RFB client wrapped as a
    wm-surface, plus its root-menu entry.  The primitive the isolated-desktop work
    is built on — several remote desktops composited into one view, each in its own
    process behind its own port.")
  (:export #:remote-surface #:remote-surface-fn #:register
           #:*remote-host* #:*remote-port* #:*remote-title*
           #:*remotes* #:remotes-report))

(in-package #:glass-remote)

(defparameter *remote-host* "127.0.0.1"
  "Host the root menu's \"Remote desktop\" connects to.")
(defparameter *remote-port* 5901
  "Port it connects to.  5901 is the neighbouring glass desktop on this box; set
   both of these before REGISTER (or call REGISTER with its own :host/:port) to
   point the menu entry somewhere else.")
(defparameter *remote-title* nil
  "Window title, or NIL to use \"host:port\" until the remote says its own name.")

(defvar *remotes* '()
  "Every live REMOTE this desktop has opened, newest first — so a control socket can
   ask what the windows are doing (REMOTES-REPORT).")

(defun remotes-report ()
  (format nil "~{~a~^~%~}" (mapcar #'gc:remote-report *remotes*)))

;;; ---- finding our own window ------------------------------------------------
;;; ADD-SURFACE makes the framebuffer and the window; the app only ever sees the
;;; framebuffer.  That is the right boundary for an app, and it costs exactly one
;;; thing: when the REMOTE resizes (the remote desktop changed size), this side has
;;; to tell the window manager that the window is now a different shape, and it has
;;; no handle on the window.  So it finds it — by the one thing it does hold, its own
;;; framebuffer, which is the surface's.  Read-only, best-effort, and never on a hot
;;; path: a resize happens when a remote desktop resizes, which is approximately
;;; never.

(defun %owning (fb)
  "(values PORT SURFACE) for the WM window whose content framebuffer is FB, or NIL."
  (ignore-errors
   (let ((all (symbol-value (find-symbol "*ALL-PORTS*" '#:climi))))
     (dolist (p all)
       (when (and (typep p (find-symbol "GLASS-PORT" '#:clim-glass))
                  (clim-glass::glass-port-wm-p p))
         (let ((s (find fb (clim-glass::glass-port-surfaces p)
                        :key #'clim-glass::wm-surface-fb)))
           (when s (return-from %owning (values p s)))))))))

(defun %remote-resized (fb w h name)
  "The remote is now W x H — which the client has ALREADY applied to FB, this being
   the notification.  What is left is the part only the window manager can do:
   retitle the window with whatever the remote calls itself, and force one
   whole-screen composite, because the window's decorated box just changed shape and
   the desktop still holds the old one's pixels outside the new one."
  (declare (ignore w h))
  (multiple-value-bind (port surf) (%owning fb)
    (when surf
      (setf (clim-glass::wm-surface-title surf)
            (or *remote-title*
                (if (plusp (length name))
                    (format nil "~a  [~a:~d]" name *remote-host* *remote-port*)
                    (format nil "~a:~d" *remote-host* *remote-port*)))
            (clim-glass::wm-surface-deco-w surf) -1))         ; force the title bar to re-render
    (when port (ignore-errors (clim-glass::composite-all port)))))

;;; ---- the surface -----------------------------------------------------------

(defun remote-surface-fn (&key (host *remote-host*) (port *remote-port*))
  "A MAKE-FN for ADD-SURFACE / the (:surface ...) window spec, connected to
   HOST:PORT.  Called with a fresh framebuffer, it returns the surface contract:
   (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN)."
  (lambda (fb)
    (let* ((*remote-host* host) (*remote-port* port)
           (r (gc:connect-remote host port :fb fb)))
      (push r *remotes*)
      (setf (gc:remote-on-resize r)
            (lambda (w h)
              ;; off the reader thread: %OWNING walks the WM's structures and
              ;; COMPOSITE-ALL paints, and neither belongs inside a decode.
              (let ((name (gc:remote-name r)) (h2 host) (p2 port))
                (sb-thread:make-thread
                 (lambda () (let ((*remote-host* h2) (*remote-port* p2))
                              (ignore-errors (%remote-resized fb w h name))))
                 :name "glass-remote-resize"))))
      (values
       ;; on-key / on-pointer: straight through, and NON-BLOCKING — these run on the
       ;; local RFB server's reader thread, so a remote that has stopped reading its
       ;; socket must cost a queued (or dropped) event, never a stalled local desktop.
       (lambda (down keysym) (gc:remote-key r down keysym))
       (lambda (mask lx ly)  (gc:remote-pointer r mask lx ly))
       ;; dirty-p: did an update land since the compositor last asked?
       (lambda () (gc:remote-take-dirty r))
       ;; copy-p: how the remote's screen translated since then — a CopyRect that
       ;; arrived over there, on its way to becoming one over here.
       (lambda () (gc:remote-take-copy r))
       ;; close-fn: the window closed, so stop the connection AND stop reconnecting.
       (lambda ()
         (setf *remotes* (remove r *remotes*))
         (gc:remote-stop r))))))

(defun remote-surface (fb)
  "The default MAKE-FN: *REMOTE-HOST*:*REMOTE-PORT*."
  (funcall (remote-surface-fn) fb))

;;; ---- the menu entry ---------------------------------------------------------

(defun register (&key (label "Remote desktop") (host *remote-host*) (port *remote-port*)
                      (width 1280) (height 800))
  "Put LABEL in the workspace root menu, opening a window onto the glass desktop at
   HOST:PORT.  WIDTH x HEIGHT is only the size the window opens at — the handshake
   replaces it with the remote's real size within a frame.  Found by name, the way
   warren and the podcast client register, so this system also loads in an image
   with no desktop in it."
  (let ((reg (find-symbol "REGISTER-APP" '#:clim-glass)))
    (when (and reg (fboundp reg))
      (funcall reg label
               (list :surface (remote-surface-fn :host host :port port)
                     :title (or *remote-title* (format nil "~a:~d" host port))
                     :width width :height height))
      label)))
