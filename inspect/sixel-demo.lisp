;;;; sixel-demo.lisp — prove the glass terminal's SIXEL decoder by piping a real
;;;; image (a JPEG, converted to sixel with ImageMagick) into it, then dumping the
;;;; rendered terminal to a PNG.  Sixel is a <=256-colour palette format, so a photo
;;;; comes through quantised — that's the format, not the terminal.
;;;;   convert some.jpg -resize 240x sixel:/tmp/demo.six
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load inspect/sixel-demo.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload '(:glass/term :glass/text :zpng))))
(in-package :glass-term)

(defun feed-string (tm s) (loop for b across (sb-ext:string-to-octets s :external-format :utf-8) do (feed-byte tm b)))
(defun feed-file (tm path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence b s)
      (loop for byte across b do (feed-byte tm byte)))))
(defun save-fb-png (fb path)
  (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (px (glass:fb-pixels fb))
         (png (make-instance 'zpng:png :width w :height h :color-type :truecolor)) (d (zpng:data-array png)))
    (dotimes (y h) (dotimes (x w) (let ((v (aref px (+ (* y w) x))))
      (setf (aref d y x 0) (ldb (byte 8 16) v) (aref d y x 1) (ldb (byte 8 8) v) (aref d y x 2) (ldb (byte 8 0) v)))))
    (zpng:write-png png path)))

(let* ((six (or (second sb-ext:*posix-argv*) "/tmp/demo.six"))
       (tm (make-terminal :cols 56 :rows 16 :ppem 16)) (e (string (code-char 27))))
  (feed-string tm (format nil "~a[1;38;5;81m  JPEG -> sixel -> glass terminal ~a[0;38;5;245m(<=256-colour palette)~a[0m~C~C~C~C" e e e #\Return #\Linefeed #\Return #\Linefeed))
  (feed-file tm six)                                  ; the sixel stream (DCS ... q ... ST)
  (feed-string tm (format nil "~C~C~a[38;5;250m  sixel bands composited over the text grid.~a[0m" #\Return #\Linefeed e e))
  (render tm)
  (save-fb-png (terminal-fb tm) "/tmp/sixel-demo.png")
  (format t "~&wrote /tmp/sixel-demo.png (~dx~d, ~d sixel image(s))~%"
          (glass:fb-width (terminal-fb tm)) (glass:fb-height (terminal-fb tm)) (length (terminal-graphics tm)))
  (kill-terminal tm))
(finish-output) (sb-ext:exit)
