;;;; inspect/remote-gate.lisp — is the RFB client actually decoding the remote?
;;;;
;;;; Headless, no window manager, no McCLIM: connect to a running glass desktop,
;;;; let a few frames land, write what we decoded to a PNG, and print the report.
;;;; The PNG is the whole point — a client that mis-decodes ZRLE still counts
;;;; rectangles happily, and only the pixels say whether the palette bit-packing
;;;; and the CPIXEL layout were read the way the encoder wrote them.
;;;;
;;;;   sbcl --non-interactive --load inspect/remote-gate.lisp -- 5901 /tmp/remote.png

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-asd "/home/claude/glass/glass.asd")
    (ql:quickload '(:glass/client :glass/text :zpng))))

(defpackage #:glass-remote-gate (:use #:cl) (:local-nicknames (#:gc #:glass-client)))
(in-package #:glass-remote-gate)

(defun save-png (fb path)
  (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb))
         (px (glass:fb-pixels fb))
         (png (make-instance 'zpng:png :width w :height h :color-type :truecolor))
         (d (zpng:data-array png)))
    (dotimes (y h)
      (dotimes (x w)
        (let ((p (aref px (+ (* y w) x))))
          (setf (aref d y x 0) (ldb (byte 8 16) p)
                (aref d y x 1) (ldb (byte 8 8) p)
                (aref d y x 2) (ldb (byte 8 0) p)))))
    (zpng:write-png png path)
    path))

(defun run (&key (port 5901) (host "127.0.0.1") (path "/tmp/remote.png") (settle 3.0)
                 (poke t))
  (let ((r (gc:connect-remote host port)))
    (sleep settle)
    ;; Move the remote pointer so an update is provoked even on an idle desktop —
    ;; and so the input half is exercised, not just the decode half.
    (when poke
      (dotimes (i 20)
        (gc:remote-pointer r 0 (+ 200 (* i 8)) (+ 200 (* i 4)))
        (sleep 0.03))
      (sleep 1.0))
    (format t "~&~a~%" (gc:remote-report r))
    (format t "~&wrote ~a~%" (save-png (gc:remote-fb r) path))
    (gc:remote-stop r)
    (gc:remote-stats r)))

(let* ((args sb-ext:*posix-argv*)
       (tail (cdr (member "--" args :test #'string=)))
       (port (if (first tail) (parse-integer (first tail)) 5901))
       (path (or (second tail) "/tmp/remote.png")))
  (run :port port :path path))
