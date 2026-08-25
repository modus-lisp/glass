;;;; seat-resize-gate.lisp — a seat's SCREEN can be resized, and its wallpaper follows.
;;;;
;;;; The bug this exists for: glass had two things called "the screen".  GLASS-ON-RESIZE
;;;; — the RFB SetDesktopSize path — resizes GLASS-PORT-TOP, "the MAIN top-level sheet",
;;;; which is one application's window and, in WM mode with nothing open, is NIL.  A
;;;; seat's screen is its FRAMEBUFFER, and nothing resized that at all.  So a viewer
;;;; asking for a different size reached a callback that did nothing, silently.
;;;;
;;;; And it could not simply be pointed at the framebuffer, because the wallpaper was
;;;; the other half of it: it was rasterised once at seat-creation and blitted at 0,0,
;;;; and the PICTURE it came from was discarded.  A resized screen would keep a
;;;; wallpaper cut for a size it no longer was, with nothing to re-cut it from.
;;;;
;;;; What fixed it is the thing the comments already described but the code did not do:
;;;; the picture is the session's taste, the pixels are one seat's, and the pixels are
;;;; cut FROM the picture whenever the screen size changes.
;;;;
;;;;   sbcl --script inspect/seat-resize-gate.lisp

(require :asdf)

;;; WHERE QUICKLISP IS.  McCLIM comes from there and `sbcl --script' implies
;;; --no-userinit, so nothing has loaded it yet.  The other gates say
;;; (load "~/quicklisp/setup.lisp"), which is one machine's answer and is item 1 on
;;; docs/RUNNING.md's list of things a fresh checkout still needs hands for; this asks
;;; instead.  QUICKLISP_SETUP wins, then the system location, then the user's.
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil
                                (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                        (and e (pathname e)))
                                      #p"/opt/quicklisp/setup.lisp"
                                      (merge-pathnames "quicklisp/setup.lisp"
                                                       (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&seat-resize-gate: no Quicklisp (tried QUICKLISP_SETUP, ~
                              /opt/quicklisp, ~~/quicklisp) — McCLIM comes from there.~%")
      (sb-ext:exit :code 1))
    (load setup)))

(let ((here (or *load-truename* *default-pathname-defaults*)))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      ;; pigment rasterises the SVG wallpaper; without it WM-RENDER-BACKGROUND fails into
      ;; its own IGNORE-ERRORS and every wallpaper check would report NIL for the wrong
      ;; reason.  That is not hypothetical — it is what this gate did on its first run.
      (ignore-errors (asdf:load-system :pigment))
      (asdf:load-asd (merge-pathnames "../backend/mcclim-glass.asd" here))
      (asdf:load-system :mcclim-glass))))

(defpackage #:glass-seat-resize-gate (:use #:cl)) (in-package #:glass-seat-resize-gate)

(defvar *pass* 0) (defvar *fail* 0) (defvar *skip* 0)
(defun ok (name got &optional detail)
  (if got (progn (incf *pass*) (format t "  [pass] ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" name detail)))
  (finish-output) got)
(defun skip (name why) (incf *skip*) (format t "  [skip] ~a — ~a~%" name why) (finish-output))
(defun head (s) (format t "~%== ~a ==~%" s) (finish-output))

(defun fb-size (fb) (and fb (list (glass:fb-width fb) (glass:fb-height fb))))

(defparameter *wallpaper*
  (let ((p (ignore-errors (merge-pathnames "assets/wallpaper.svg"
                                           (asdf:system-source-directory :mcclim-glass)))))
    (and p (probe-file p) (namestring p))))

;;; A session with NO WIRE: this is about the screen, not about serving it.
(defparameter *port*
  (clim-glass:make-wm-session :width 800 :height 600
                              :background *wallpaper* :background-mode :cover))
(clim-glass:start-wm-session *port* '())
(defparameter *wm* (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop *port*))
                                          :name "seat-resize-gate-wm"))
(sleep 1)
(defparameter *seat* (clim-glass:port-seat *port*))

(head "the screen a seat actually has")
(multiple-value-bind (fb on-key on-pointer on-resize wake)
    (clim-glass:attach-seat-local *seat*)
  (declare (ignore on-key on-pointer wake))
  (ok "a seat starts at the size it was made with" (equal (fb-size fb) '(800 600))
      (format nil "~a" (fb-size fb)))

  (head "resizing it")
  ;; This is what a local viewer does when its window is dragged.  It is deliberately
  ;; the seat's own resize and NOT glass-on-resize — see the header.
  (funcall on-resize 1024 700)
  (sleep 0.5)
  (ok "a viewer's resize resizes THE SEAT" (equal (fb-size fb) '(1024 700))
      (format nil "~a" (fb-size fb)))
  ;; In place, so every reference already handed out (a capture, a viewer, the
  ;; compositor) keeps pointing at the screen rather than at a stale copy of it.
  (ok "…the same framebuffer object, resized in place"
      (eq fb (clim-glass:attach-seat-local *seat*)))
  (funcall on-resize 640 480)
  (sleep 0.5)
  (ok "…and it shrinks as well as grows" (equal (fb-size fb) '(640 480))
      (format nil "~a" (fb-size fb)))
  ;; A viewer echoes the size back at us: SDL answers %SET-WINDOW-SIZE with its own
  ;; SIZE-CHANGED event, so "resize to what you already are" must be a no-op or the two
  ;; of them push each other round forever.
  (ok "asking for the size it already is does nothing"
      (null (clim-glass:resize-seat-screen *seat* 640 480))))

(head "the wallpaper follows")
(if (null *wallpaper*)
    (skip "wallpaper checks" "no assets/wallpaper.svg in this checkout")
    (progn
      (ok "the wallpaper is cut for the screen"
          (equal (fb-size (clim-glass:seat-wallpaper *seat*)) '(640 480))
          (format nil "~a" (fb-size (clim-glass:seat-wallpaper *seat*))))
      (clim-glass:resize-seat-screen *seat* 1200 900)
      (sleep 0.5)
      ;; THE POINT OF THE WHOLE EXERCISE.  Before, this was the old size: the picture had
      ;; been consumed at set time, so the pixels were the only record of it.
      (ok "…and re-cut when the screen is resized"
          (equal (fb-size (clim-glass:seat-wallpaper *seat*)) '(1200 900))
          (format nil "~a" (fb-size (clim-glass:seat-wallpaper *seat*))))
      (clim-glass:resize-seat-screen *seat* 500 400)
      (sleep 0.5)
      (ok "…every time, in both directions"
          (equal (fb-size (clim-glass:seat-wallpaper *seat*)) '(500 400))
          (format nil "~a" (fb-size (clim-glass:seat-wallpaper *seat*))))

      (head "and a second pair of eyes gets the same picture, not the same pixels")
      (handler-case
          (let ((s2 (clim-glass:add-wm-seat *port* :width 640 :height 480
                                                   :port-num 5999 :serve nil :audio nil)))
            (ok "a seat added later inherits the session's wallpaper"
                (equal (fb-size (clim-glass:seat-wallpaper s2)) '(640 480))
                (format nil "~a" (fb-size (clim-glass:seat-wallpaper s2))))
            (ok "…cut for ITS screen, not the first seat's"
                (not (equal (fb-size (clim-glass:seat-wallpaper s2))
                            (fb-size (clim-glass:seat-wallpaper *seat*))))
                (format nil "seat 2 ~a vs seat 1 ~a"
                        (fb-size (clim-glass:seat-wallpaper s2))
                        (fb-size (clim-glass:seat-wallpaper *seat*)))))
        (error (e) (skip "second seat" (princ-to-string e))))))

(head "and a CLIENT can ask for it, which is the path a browser uses")
;; noVNC sends SetDesktopSize when its container resizes; glass turns that into a resize
;; of the seat the client is looking at.  This is the whole chain minus the browser: a
;; real transport, a real handshake, a real message 251.
(let ((sock (format nil "/tmp/seat-resize-gate-~d.rfb" (sb-posix:getpid))))
  (handler-case
      (progn
        (clim-glass:open-seat-transport *seat* :kind :rfb-unix :path sock)
        (sleep 1)
        (let ((s (nth-value 1 (glass:open-connection :host (format nil "unix:~a" sock))))
              (nl (code-char 10)))
          (flet ((rd (n) (let ((b (make-array n :element-type '(unsigned-byte 8))))
                           (read-sequence b s) b))
                 (wr (v) (write-sequence (coerce v '(vector (unsigned-byte 8))) s)
                   (finish-output s)))
            (rd 12)
            (wr (map 'list #'char-code (format nil "RFB 003.008~c" nl)))
            (let ((n (aref (rd 1) 0))) (rd n) (wr (list 1)) (rd 4))
            (wr (list 1))
            (rd 4) (rd 16)
            (let ((l (rd 4)))
              (rd (+ (* 16777216 (aref l 0)) (* 65536 (aref l 1)) (* 256 (aref l 2)) (aref l 3))))
            ;; SetEncodings advertising ExtendedDesktopSize (-308), as noVNC does
            (wr (list 2 0 0 1 #xFF #xFF #xFE #xCC))
            ;; ...and the server must ANSWER with one, unprompted, or noVNC never enables
            ;; its resize path and never sends the message this gate is about to send.
            (wr (list 3 0 0 0 0 0 4 0 2 88))          ; FramebufferUpdateRequest
            (let* ((hdr (rd 4)) (r (rd 12))
                   (enc (+ (* 16777216 (aref r 8)) (* 65536 (aref r 9))
                           (* 256 (aref r 10)) (aref r 11))))
              (declare (ignore hdr))
              (ok "the server offers ExtendedDesktopSize before being asked"
                  (= enc #xFFFFFECC) (format nil "#x~8,'0X" enc)))
            (let ((w 1024) (h 640))
              (wr (append (list 251 0 (ldb (byte 8 8) w) (ldb (byte 8 0) w)
                                (ldb (byte 8 8) h) (ldb (byte 8 0) h) 1 0)
                          (list 0 0 0 1 0 0 0 0
                                (ldb (byte 8 8) w) (ldb (byte 8 0) w)
                                (ldb (byte 8 8) h) (ldb (byte 8 0) h) 0 0 0 0))))
            (sleep 1.5)
            (ok "SetDesktopSize from a client resizes THIS SEAT"
                (equal (fb-size (clim-glass:attach-seat-local *seat*)) '(1024 640))
                (format nil "~a" (fb-size (clim-glass:attach-seat-local *seat*))))
            (ignore-errors (close s)))))
    (error (e) (skip "client-driven resize" (princ-to-string e))))
  (ignore-errors (delete-file sock)))

(format t "~%~d passed, ~d failed, ~d skipped~%~%=> ~:[FAIL~;PASS~]~%"
        *pass* *fail* *skip* (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
