;;;; copyrect-unit.lisp — pure in-process correctness check for CopyRect (no
;;;; sockets/threads/WM).  Simulate a window MOVE and confirm that the server's
;;;; snapshot-move + dirty-rects, applied on the client as CopyRect + exposed
;;;; rects, reconstruct the new framebuffer EXACTLY.  0 differing pixels == correct.
;;;;   sbcl --non-interactive --load backend/inspect/copyrect-unit.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (ql:quickload :glass)))
(in-package :glass)

(defun run-case (w h ww wh ax ay bx by)
  (let* ((fb (make-framebuffer w h)) (px (fb-pixels fb))
         (client (make-array (* w h) :element-type '(unsigned-byte 32)))
         (snap   (make-array (* w h) :element-type '(unsigned-byte 32))))
    (flet ((bg   (x y) (logior (ash (mod (* x 3) 256) 16) (ash (mod (* y 5) 256) 8) (mod (* x y) 251)))
           (win  (x y) (logior (ash (mod (* x 7) 256) 16) (ash (mod (* y 11) 256) 8) 200)))
      ;; fb-old = background everywhere + a "window" at A; client & snap start there.
      (dotimes (y h) (dotimes (x w) (setf (aref px (+ (* y w) x)) (bg x y))))
      (dotimes (yy wh) (dotimes (xx ww) (setf (aref px (+ (* (+ ay yy) w) (+ ax xx))) (win xx yy))))
      (replace client px) (replace snap px)
      ;; fb-new = background + the same window at B (erase A back to bg).
      (dotimes (yy wh) (dotimes (xx ww) (setf (aref px (+ (* (+ ay yy) w) (+ ax xx))) (bg (+ ax xx) (+ ay yy)))))
      (dotimes (yy wh) (dotimes (xx ww) (setf (aref px (+ (* (+ by yy) w) (+ bx xx))) (win xx yy))))
      ;; SERVER: move the block in the snapshot, then diff over the damage box.
      (snapshot-move snap w ax ay bx by ww wh)
      (let* ((dmg (list (min ax bx) (min ay by) (+ (max ax bx) ww) (+ (max ay by) wh)))
             (rects (dirty-rects fb snap dmg)))
        ;; CLIENT: CopyRect(A->B) on its own fb, then apply the exposed rects from fb-new.
        (let ((tmp (make-array (* ww wh) :element-type '(unsigned-byte 32))))
          (dotimes (yy wh) (dotimes (xx ww) (setf (aref tmp (+ (* yy ww) xx)) (aref client (+ (* (+ ay yy) w) ax xx)))))
          (dotimes (yy wh) (dotimes (xx ww) (setf (aref client (+ (* (+ by yy) w) bx xx)) (aref tmp (+ (* yy ww) xx))))))
        (dolist (r rects) (destructuring-bind (rx ry rw rh) r
          (dotimes (yy rh) (dotimes (xx rw)
            (setf (aref client (+ (* (+ ry yy) w) (+ rx xx))) (aref px (+ (* (+ ry yy) w) (+ rx xx))))))))
        (let ((diff 0)) (dotimes (i (* w h)) (unless (= (aref client i) (aref px i)) (incf diff)))
          (list (length rects) diff))))))

(let ((total 0))
  (dolist (c '((200 120 60 40 20 15 110 60)     ; non-overlapping move
               (200 120 60 40 20 15 40 30)      ; heavily-overlapping move
               (200 120 60 40 20 15 20 60)      ; straight-down move
               (200 120 80 50 100 40 10 10)))   ; move up-left
    (destructuring-bind (nr diff) (apply #'run-case c)
      (incf total diff)
      (format t "~&case ~a -> ~d exposed rects, ~d diff pixels~%" c nr diff)))
  (format t "~&COPYRECT-UNIT TOTAL DIFF: ~d (0 == correct)~%" total))
(finish-output) (sb-ext:exit)
