;;;; test/i420-test.lisp — :I420 framebuffers: the same drawing as :XRGB, read back through FB-GET.
;;;;   sbcl --non-interactive --load test/i420-test.lisp     (exits 0 iff every check passes)
;;;;
;;;; The :XRGB framebuffer is the reference.  Luma must agree EXACTLY wherever the drawing is
;;;; solid (rectangles, fills, moves, blits): 4:2:0 keeps full-resolution brightness, and that
;;;; is the claim.  Colour is compared at block resolution, away from edges.  Text is held to a
;;;; looser bound, measured, because :I420 blends in Y'CbCr where :XRGB blends in linear light.

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (make-pathname :name nil :type nil
                     :defaults (merge-pathnames "../" (make-pathname :name nil :type nil
                                                                     :defaults *load-truename*)))
      asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/desk)))

(defpackage #:glass-i420-test (:use #:cl))
(in-package #:glass-i420-test)

(defvar *fails* 0)
(defun check (name ok) (format t "~&I420 ~:[FAIL~;ok  ~] ~a~%" ok name) (unless ok (incf *fails*)))

(defun luma (c) (glass::%rgb-y c))
(defun both (w h fill) (list (glass:make-framebuffer w h fill :xrgb) (glass:make-framebuffer w h fill :i420)))
(defun max-luma-diff (a b)
  (let ((m 0))
    (dotimes (y (glass:fb-height a) m)
      (dotimes (x (glass:fb-width a))
        (setf m (max m (abs (- (luma (glass:fb-get a x y))
                               (glass::%rgb-y (glass:fb-get b x y))))))))))
(defun luma-of-i420 (fb x y) (aref (glass:fb-y fb) (+ x (* y (glass:fb-width fb)))))
(defun max-y-diff (a b)
  "Reference luma vs the :I420 plane's stored luma."
  (let ((m 0))
    (dotimes (y (glass:fb-height a) m)
      (dotimes (x (glass:fb-width a))
        (setf m (max m (abs (- (luma (glass:fb-get a x y)) (luma-of-i420 b x y)))))))))
(defun chan-diff (c d) (max (abs (- (ldb (byte 8 16) c) (ldb (byte 8 16) d)))
                            (abs (- (ldb (byte 8 8) c) (ldb (byte 8 8) d)))
                            (abs (- (ldb (byte 8 0) c) (ldb (byte 8 0) d)))))

(format t "~&== format and planes~%")
(destructuring-bind (x i) (both 64 48 #x1E2530)
  (check "default format is :XRGB" (eq (glass:fb-format (glass:make-framebuffer 4 4)) :xrgb))
  (check ":I420 planes are WxH and ceil(W/2) x ceil(H/2)"
         (and (= (length (glass:fb-y i)) (* 64 48)) (= (length (glass:fb-u i)) (* 32 24))
              (= (length (glass:fb-u (glass:make-framebuffer 5 3 0 :i420))) (* 3 2))))
  (check "a fill reads back within 2 per channel" (<= (chan-diff (glass:fb-get i 10 10) #x1E2530) 2))
  (check "the fill's luma is exact" (zerop (max-y-diff x i)))
  (let ((glass:*default-fb-format* :i420))
    (check "*DEFAULT-FB-FORMAT* picks the format" (eq (glass:fb-format (glass:make-framebuffer 8 8)) :i420))))

(format t "~&== rectangles, odd edges, clip~%")
(destructuring-bind (x i) (both 101 77 0)
  (dolist (fb (list x i))
    (glass:fb-rect fb 3 5 40 21 #xFF3030)
    (glass:fb-rect fb 50 7 1 60 #x30FF30)                  ; a 1-pixel line at an even x
    (glass:fb-rect fb 61 7 1 60 #x4060FF)                  ; and at an odd x
    (glass:fb-frame fb 70 20 25 25 #xFFFFFF 3)
    (glass:with-fb-clip (fb 10 30 20 20) (glass:fb-rect fb 0 0 101 77 #xFFE040)))
  (check "luma is exact everywhere" (zerop (max-y-diff x i)))
  (check "colour inside a solid rectangle is right"
         (<= (chan-diff (glass:fb-get x 20 15) (glass:fb-get i 20 15)) 3))
  (check "the clip box confined the fill"
         (and (= (glass:fb-get x 5 30) 0) (<= (chan-diff (glass:fb-get i 5 30) 0) 2)
              (<= (chan-diff (glass:fb-get i 15 35) #xFFE040) 3))))

(format t "~&== move and blits~%")
(destructuring-bind (x i) (both 120 90 #x102030)
  (dolist (fb (list x i))
    (glass:fb-rect fb 10 10 30 20 #xFF0000) (glass:fb-rect fb 15 40 9 9 #x00FF00)
    (glass:fb-move-rect fb 10 10 50 30 30 40)              ; even move
    (glass:fb-move-rect fb 15 40 71 51 9 9))               ; odd move
  (check "FB-MOVE-RECT: luma exact" (zerop (max-y-diff x i)))
  (check "moved colour is right inside the block"
         (<= (chan-diff (glass:fb-get x 65 40) (glass:fb-get i 65 40)) 3)))
(destructuring-bind (x i) (both 200 150 #x202020)
  (destructuring-bind (wx wi) (both 61 41 #x3366CC)
    (dolist (w (list wx wi)) (glass:fb-rect w 0 0 61 8 #xEEEEEE))
    (glass:fb-blit-clipped x wx 13 17) (glass:fb-blit-clipped i wi 13 17)
    (glass:fb-blit-clipped x wx 150 120) (glass:fb-blit-clipped i wi 150 120)   ; off the edge
    (glass:with-fb-clip (x 0 0 100 100) (glass:fb-blit-clipped x wx 90 90))
    (glass:with-fb-clip (i 0 0 100 100) (glass:fb-blit-clipped i wi 90 90))
    (check "FB-BLIT-CLIPPED (odd position, off-edge, clipped): luma exact" (zerop (max-y-diff x i)))
    (check "blitted colour is right" (<= (chan-diff (glass:fb-get x 40 40) (glass:fb-get i 40 40)) 3))
    (glass:fb-blit x wx 30 100) (glass:fb-blit i wi 30 100)
    (check "FB-BLIT: luma exact" (zerop (max-y-diff x i)))))

(format t "~&== video: FB-BLIT-YUV~%")
(let* ((sw 64) (sh 36) (ys (make-array (* sw sh) :element-type '(unsigned-byte 8)))
       (us (make-array (* 32 18) :element-type '(unsigned-byte 8) :initial-element 90))
       (vs (make-array (* 32 18) :element-type '(unsigned-byte 8) :initial-element 200))
       (fb (glass:make-framebuffer 100 80 0 :i420)))
  (dotimes (k (* sw sh)) (setf (aref ys k) (+ 16 (mod k 200))))
  (glass:fb-blit-yuv fb 10 20 sw sh ys us vs sw sh sw 32)
  (check "1:1 copies luma exactly"
         (loop for yy below sh always
               (loop for xx below sw always (= (luma-of-i420 fb (+ 10 xx) (+ 20 yy)) (aref ys (+ xx (* yy sw)))))))
  (check "and chroma" (and (= (aref (glass:fb-u fb) (+ 5 (* 10 50))) 90) (= (aref (glass:fb-v fb) (+ 20 (* 15 50))) 200)))
  (glass:fb-blit-yuv fb 0 0 100 80 ys us vs sw sh sw 32)
  (check "scaled to fill: corners are the source corners"
         (and (= (luma-of-i420 fb 0 0) (aref ys 0)) (= (luma-of-i420 fb 99 79) (aref ys (1- (* sw sh)))))))

(format t "~&== text~%")
(destructuring-bind (x i) (both 300 60 #x1E2530)
  (dolist (fb (list x i)) (glass:fb-text fb 4 8 "The quick brown fox 0123" :size 16 :color #xFFFFFF)
                          (glass:fb-text fb 4 34 "coloured" :size 13 :color #xFF3030))
  (let ((d (max-y-diff x i)) (n 0) (sum 0))
    (dotimes (yy 60) (dotimes (xx 300)
      (let ((e (abs (- (luma (glass:fb-get x xx yy)) (luma-of-i420 i xx yy))))) (incf sum e) (incf n))))
    (format t "~&    text luma: max diff ~d, mean ~,2f~%" d (/ sum n))
    (check "text luma tracks the reference (mean diff < 3)" (< (/ sum n) 3))
    (check "text drew something" (> (count-if (lambda (v) (> v 150)) (glass:fb-y i)) 100))))

(format t "~&== a desk in :I420~%")
(let* ((glass:*default-fb-format* :i420)
       (screen (glass:make-framebuffer 400 300 #x1E2530))
       (d (glass.desk:make-desk screen)))
  (glass.desk:desk-register-app d "Red" (lambda (fb) (glass:fb-fill fb #xff0000)
                                          (values nil (lambda (m x y) (declare (ignore m x y))) (lambda () nil)))
                                :width 120 :height 80)
  (glass.desk:desk-open d "Red")
  (glass.desk:desk-redraw d) (glass.desk:desk-tick d)
  (let ((w (first (glass.desk::desk-windows d))))
    (check "the window's surface is :I420" (eq (glass:fb-format (glass.desk::win-fb w)) :i420))
    (check "its content reached the screen red"
           (<= (chan-diff (glass:fb-get screen (+ (glass.desk::win-x w) 40) (+ (glass.desk::win-y w) 40)) #xff0000) 4))))

(format t "~&I420: ~:[~d FAILED~;all passed~]~%" (zerop *fails*) *fails*)
(uiop:quit (if (zerop *fails*) 0 1))
