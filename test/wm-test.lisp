;;;; test/wm-test.lisp — glass/wm in process: chrome hit test, drag (opaque and wireframe), the
;;;; clamp, resize, close, the root and window menus, focus and the keyboard hint, and that every
;;;; damage-limited composite leaves the screen exactly as a full repaint would.
;;;;   sbcl --script test/wm-test.lisp   (with glass/wm findable by ASDF; exits 0 iff all pass)

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass/wm)))

(defpackage #:glass-wm-test (:use #:cl))
(in-package #:glass-wm-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&WM ~:[FAIL~;ok  ~] ~a~%" ok name)
  (unless ok (incf *fails*)))

(defun same-as-full-p (wm)
  "Does the screen equal what a full repaint draws?"
  (let* ((fb (glass.wm:wm-fb wm)) (before (copy-seq (glass:fb-pixels fb))))
    (glass.wm::wm-composite wm nil)
    (equalp before (glass:fb-pixels fb))))

(defun ptr (wm mask x y) (glass.wm:wm-pointer wm mask x y) (glass.wm:wm-tick wm))

(let* ((screen (glass:make-framebuffer 800 600))
       (wm (glass.wm:make-wm screen))
       (calls '()))
  (setf (glass.wm:wm-keyboard-fn wm) (lambda (on) (push on calls)))
  (dolist (a glass.desk:*standard-apps*)
    (destructuring-bind (label fn &key width height) a
      (glass.wm:wm-register-app wm label fn :width width :height height)))
  (let* ((paint (glass.wm:wm-open wm "Paint"))
         (notes (glass.wm:wm-open wm "Notes")))
    (glass.wm:wm-tick wm)
    (check "two windows, Notes on top" (equal (glass.wm:wm-surfaces wm) (list notes paint)))
    (check "cascade: Paint at 40,62, Notes at 68,90"
           (and (= 40 (glass.wm::surf-x paint)) (= 62 (glass.wm::surf-y paint))
                (= 68 (glass.wm::surf-x notes)) (= 90 (glass.wm::surf-y notes))))
    (check "opening Notes (text) raised the keyboard, Paint did not" (equal calls '(t)))
    (check "screen = full repaint after opening" (same-as-full-p wm))
    ;; resize Notes by its corner
    (ptr wm 1 483 385) (ptr wm 1 433 355) (ptr wm 0 433 355)
    (check "resize corner: Notes is 370x270" (and (= 370 (glass.wm::surf-cw notes)) (= 270 (glass.wm::surf-ch notes))))
    (check "screen = full repaint after resize" (same-as-full-p wm))
    ;; drag Paint by its title (opaque)
    (setf calls '())
    (ptr wm 1 150 50)
    (check "title press raises Paint and puts the keyboard away"
           (and (eq paint (first (glass.wm:wm-surfaces wm))) (equal calls '(nil))
                (eq paint (glass.wm:wm-focus wm))))
    (let ((glass.wm:*wireframe-lag-ms* 1000000) (glass.wm:*wireframe-queue-kb* 1d9))
      (ptr wm 1 200 100) (ptr wm 1 250 150) (ptr wm 0 250 150))
    (check "opaque drag moved Paint by the pointer" (and (= 140 (glass.wm::surf-x paint)) (= 162 (glass.wm::surf-y paint))))
    (check "opaque drag never went wireframe" (zerop (glass.wm::wm-wire-drags wm)))
    (check "screen = full repaint after opaque drag" (same-as-full-p wm))
    ;; the same drag with the link lagging: wireframe, the window lands on release
    (let ((glass.wm:*wireframe-lag-ms* -1) (glass.wm:*wm-log* nil))
      (ptr wm 1 250 150) (ptr wm 1 280 180)
      (check "lagging: wireframe engaged, window held still"
             (and (glass.wm::wm-drag-wire wm) (= 140 (glass.wm::surf-x paint))))
      (check "screen = full repaint with the outline up" (same-as-full-p wm))
      (ptr wm 1 300 200) (ptr wm 0 300 200))
    (check "wireframe drop lands the window" (and (= 190 (glass.wm::surf-x paint)) (= 212 (glass.wm::surf-y paint))
                                                 (not (glass.wm::wm-drag-wire wm))))
    (check "screen = full repaint after the wireframe drop" (same-as-full-p wm))
    ;; the clamp: 40 px of title bar stay reachable
    (ptr wm 1 300 200) (ptr wm 1 3000 3000) (ptr wm 0 3000 3000)
    (check "clamp keeps the title bar on screen"
           (and (<= (glass.wm::surf-x paint) 760) (<= (glass.wm::surf-y paint) 596)))
    (ptr wm 1 (+ (glass.wm::surf-x paint) 20) (- (glass.wm::surf-y paint) 10))
    (ptr wm 1 200 100) (ptr wm 0 200 100)
    (check "and it can be dragged back" (< (glass.wm::surf-x paint) 400))
    (check "screen = full repaint after the clamp" (same-as-full-p wm))
    ;; content press: focus with the keyboard, keys go there
    (setf calls '())
    (let ((nx (+ (glass.wm::surf-x notes) 50)) (ny (+ (glass.wm::surf-y notes) 150)))
      (ptr wm 1 nx ny) (ptr wm 0 nx ny))
    (check "tap into Notes: raised, focused, keyboard up"
           (and (eq notes (first (glass.wm:wm-surfaces wm))) (eq notes (glass.wm:wm-focus wm)) (equal calls '(t))))
    ;; the window menu: Back
    (multiple-value-bind (bx by) (glass.wm::wm-menu-button-box)
      (let ((x (+ (glass.wm::surf-x notes) bx 3)) (y (+ (- (glass.wm::surf-y notes) (glass.wm::wm-titleh)) by 3)))
        (ptr wm 1 x y)
        (check "wedge opens the Window menu" (equal "Window" (glass.wm::wm-menu-title (glass.wm::wm-menu wm))))
        (ptr wm 0 x y)
        (let* ((m (glass.wm::wm-menu wm))
               (i (position "Back" (glass.wm::wm-menu-items m) :key #'car :test #'string=))
               (iy (+ (glass.wm::wm-menu-y m) (glass.wm::menu-titleh) (* i (glass.wm::menu-itemh)) 5))
               (ix (+ (glass.wm::wm-menu-x m) 20)))
          (ptr wm 1 ix iy) (ptr wm 0 ix iy))))
    (check "Back sent Notes to the bottom" (and (null (glass.wm::wm-menu wm)) (eq notes (car (last (glass.wm:wm-surfaces wm))))))
    (check "screen = full repaint after the menu" (same-as-full-p wm))
    ;; the close box
    (glass.wm::wm-raise wm notes)
    (setf calls '())
    (multiple-value-bind (kx ky ks) (glass.wm::wm-close-button-box (glass.wm::surf-cw notes))
      (let ((x (+ (glass.wm::surf-x notes) kx (floor ks 2)))
            (y (+ (- (glass.wm::surf-y notes) (glass.wm::wm-titleh)) ky (floor ks 2))))
        (ptr wm 1 x y) (ptr wm 0 x y)))
    (check "close box closes Notes and puts the keyboard away"
           (and (equal (glass.wm:wm-surfaces wm) (list paint)) (null (glass.wm:wm-focus wm)) (equal calls '(nil))))
    (check "screen = full repaint after close" (same-as-full-p wm))
    ;; the root menu: press the workspace, release on Clock
    (ptr wm 4 700 20) (ptr wm 0 700 20)
    (let ((m (glass.wm::wm-menu wm)))
      (check "right press on the workspace opens the root menu"
             (and m (equal (mapcar #'car (glass.wm::wm-menu-items m)) (mapcar #'first glass.desk:*standard-apps*))))
      (let ((ix (+ (glass.wm::wm-menu-x m) 20)) (iy (+ (glass.wm::wm-menu-y m) (glass.wm::menu-titleh) 5)))
        (ptr wm 1 ix iy) (ptr wm 0 ix iy)))
    (check "choosing Clock opens it on top"
           (and (null (glass.wm::wm-menu wm)) (equal "Clock" (glass.wm::surf-title (first (glass.wm:wm-surfaces wm))))))
    (check "screen = full repaint after the root menu" (same-as-full-p wm))
    ;; a left tap on the workspace opens it too (a touch screen has no right button)
    (ptr wm 1 790 590) (ptr wm 0 790 590)
    (check "left tap on the workspace opens the root menu" (glass.wm::wm-menu wm))
    (ptr wm 1 5 590) (ptr wm 0 5 590)
    (check "a press off the menu dismisses it" (null (glass.wm::wm-menu wm)))
    (check "screen = full repaint at the end" (same-as-full-p wm))))

(format t "~&WM ~d failure~:p~%" *fails*)
(finish-output)
#+sbcl (sb-ext:exit :code (if (zerop *fails*) 0 1))
#-sbcl (if (zerop *fails*) (uiop:quit 0) (uiop:quit 1))
