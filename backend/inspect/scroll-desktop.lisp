;;;; scroll-desktop.lisp — a glass desktop with ONE thing in it that scrolls.
;;;;
;;;; The inner desktop of the nested-CopyRect measurement: a loom browser window on
;;;; a generated tall page, which is the only window on a glass desktop that reports
;;;; a content translation (COPY-P) and therefore the only one whose scroll can be
;;;; sent as a CopyRect.  Deterministic and local — no network, the same bytes every
;;;; run — because the number being measured is a ratio between two encodings of the
;;;; same pixels, and a page that changed between the arms would decide it.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 \
;;;;        --load backend/inspect/scroll-desktop.lisp -- 5931 4031

(require :asdf)
(load "~/quicklisp/setup.lisp")

(defparameter *vnc* 5931)
(defparameter *control* 4031)
;; The browser window's size.  1100x700 fills a 1280x800 desktop, which is what a
;; scroll measurement wants (the biggest possible moved block); a DRAG measurement
;; needs the window to have somewhere to go, so it asks for a smaller one.
(defparameter *win-w* 1100)
(defparameter *win-h* 700)
(let ((tail (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
  (when (first tail) (setf *vnc* (parse-integer (first tail))))
  (when (second tail) (setf *control* (parse-integer (second tail))))
  (when (third tail) (setf *win-w* (parse-integer (third tail))))
  (when (fourth tail) (setf *win-h* (parse-integer (fourth tail)))))

(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-asd (merge-pathnames "../../glass.asd" *load-truename*))
    (ql:quickload '(:glass :mcclim :mcclim-render :sb-concurrency :pigment))
    (asdf:load-system :loom/glass)
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)))

;;; A tall, busy, entirely local page.  Text and rules rather than photographs: the
;;; point is a page whose every scroll step exposes a strip that costs real bytes to
;;; encode, not one that compresses to nothing and flatters whichever arm runs second.
(defparameter *page*
  (let ((path "/tmp/glass-scroll-page.html"))
    (with-open-file (o path :direction :output :if-exists :supersede)
      (format o "<!doctype html><meta charset=utf-8><title>scroll</title><style>~%")
      (format o "body{margin:0;font:15px/1.55 sans-serif;color:#161616;background:#fff}~%")
      (format o "section{padding:18px 32px;border-bottom:1px solid #d0d4d8}~%")
      (format o "section:nth-child(even){background:#eef1f4}~%")
      (format o "h2{font-size:22px;margin:0 0 8px}p{margin:0 0 10px;max-width:62em}~%")
      (format o "code{background:#dde3e8;padding:1px 4px}</style>~%")
      (dotimes (i 60)
        (format o "<section><h2>Section ~d — a heading with some width to it</h2>~%" i)
        (dotimes (k 3)
          (format o "<p>~d.~d The frame arrives in two halves and only one of them is visible from ~
                     either side. A page painted once into a tall canvas costs nothing to scroll ~
                     in principle; what it costs in practice is the copy of the visible slice and ~
                     the bytes that copy forces onto the wire. Between those two the milliseconds ~
                     hide, and a scroll that feels slow feels slow for exactly one of them. ~
                     <code>section ~d row ~d</code></p>~%" i k i k))
        (format o "<ul>~{<li>~a</li>~}</ul></section>~%"
                (loop for k below 6 collect (format nil "item ~d.~d — a short list row with text" i k))))
      (namestring path))))

(in-package :clim-glass)

(setf glass:*desktop-name*
      (format nil "glass scroll fixture (:~d)" (- (symbol-value 'cl-user::*vnc*) 5900)))

;; The shipped one (backend/control.lisp), not a copy: the copy that used to live here
;; closed the connection without a byte when a form would not READ.
(start-control-socket :port (symbol-value 'cl-user::*control*)
                      :name (format nil "glass-control-~d" (symbol-value 'cl-user::*control*)))

(format *error-output* "~&@@ scroll fixture on :~d (control ~d) — ~a~%"
        (symbol-value 'cl-user::*vnc*) (symbol-value 'cl-user::*control*)
        (symbol-value 'cl-user::*page*))
(finish-output *error-output*)

(run-wm (list (list :browse (format nil "file://~a" (symbol-value 'cl-user::*page*))
                    :width (symbol-value 'cl-user::*win-w*)
                    :height (symbol-value 'cl-user::*win-h*)))
        :port (symbol-value 'cl-user::*vnc*) :width 1280 :height 800)
