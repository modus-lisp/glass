;;;; menu-gesture-gate.lisp — a menu chooses what you RELEASE on, and nothing else.
;;;;
;;;; THE BUG THIS EXISTS FOR, reported as: "if i click to pop the root menu, then drag
;;;; without releasing, we're activating every menu item that the cursor moves across."
;;;;
;;;; A pointer event carries the CURRENT button mask, and the handler asked whether the
;;;; left button was down.  During a press-and-drag that is true of every motion event, so
;;;; each item the cursor crossed was a leaf with the button down, and each one ran.  With
;;;; a menu of five items, dragging down it launched five things.
;;;;
;;;; What chooses an item is letting go on it, so the tree remembers the mask it last saw
;;;; and acts on the release EDGE.  One rule covers both idioms people expect:
;;;;
;;;;   press, drag, release   -> runs what you released on
;;;;   click, move, click     -> the second press only hovers; its release runs the item
;;;;   a single opening click -> chooses nothing, because the menu is placed with its
;;;;                             title strip under the pointer
;;;;   drag off and let go    -> runs nothing, and dismisses
;;;;
;;;; The gate drives WM-ON-POINTER with real masks rather than calling the menu handler
;;;; directly, because the bug was in which events reach the handler and what it concludes
;;;; from them — a test that called the handler with a tidy sequence would have passed
;;;; against the broken code.
;;;;
;;;;   sbcl --script inspect/menu-gesture-gate.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&menu-gesture-gate: no Quicklisp — McCLIM comes from there.~%")
      (sb-ext:exit :code 1))
    (load setup)))
(let ((here (or *load-truename* *default-pathname-defaults*)))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (ignore-errors (asdf:load-system :pigment))
      (asdf:load-asd (merge-pathnames "../backend/mcclim-glass.asd" here))
      (asdf:load-system :mcclim-glass))))

(defvar *pass* 0) (defvar *fail* 0)
(defun ok (n g &optional d)
  (if g (progn (incf *pass*) (format t "  [pass] ~a~@[ — ~a~]~%" n d))
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" n d))))

(defparameter *ran* '())
(let ((p (clim-glass:make-wm-session :width 1200 :height 800)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "d")
  (sleep 1.5)
  ;; a menu of leaves that only record being run
  (let* ((seat (clim-glass:port-seat p))
         (items (loop for i from 0 below 5
                      collect (cons (format nil "item ~d" i)
                                    (let ((i i)) (lambda () (push i *ran*)))))))
    (setf (clim-glass::glass-port-menu-items p) items)
    (flet ((row-y (menu i)
             (+ (clim-glass::wm-menu-y menu)
                (clim-glass::with-seat-scale (seat) (clim-glass::menu-titleh))
                (* i (clim-glass::with-seat-scale (seat) (clim-glass::menu-itemh)))
                (floor (clim-glass::with-seat-scale (seat) (clim-glass::menu-itemh)) 2))))

      ;; ---- press, drag across every item, release on the last -------------------
      (setf *ran* '())
      (clim-glass::wm-open-menu p 100 100 seat)
      (let* ((menu (clim-glass::seat-menu seat))
             (mx (+ (clim-glass::wm-menu-x menu) 30)))
        (dotimes (i 5)                                    ; drag with the button HELD
          (clim-glass::wm-on-pointer p 1 mx (row-y menu i) seat) (sleep 0.05))
        (ok "dragging across 5 items with the button down runs NONE of them"
            (null *ran*) (format nil "ran ~s" (reverse *ran*)))
        (clim-glass::wm-on-pointer p 0 mx (row-y menu 4) seat)   ; release on the last
        (sleep 0.3)
        (ok "...and releasing on the last runs exactly that one"
            (equal *ran* '(4)) (format nil "ran ~s" (reverse *ran*))))

      ;; ---- click to open, move, click to choose ---------------------------------
      (setf *ran* '())
      (clim-glass::wm-open-menu p 100 100 seat)
      (let* ((menu (clim-glass::seat-menu seat))
             (mx (+ (clim-glass::wm-menu-x menu) 30)))
        (clim-glass::wm-on-pointer p 0 mx (row-y menu 0) seat)   ; the opening click's release
        (sleep 0.1)
        (ok "a click that opens the menu chooses nothing"
            (null *ran*) (format nil "ran ~s" (reverse *ran*)))
        (clim-glass::wm-on-pointer p 0 mx (row-y menu 2) seat)   ; move, no button
        (clim-glass::wm-on-pointer p 1 mx (row-y menu 2) seat)   ; press on item 2
        (sleep 0.1)
        (ok "...pressing on an item does not run it yet"
            (null *ran*) (format nil "ran ~s" (reverse *ran*)))
        (clim-glass::wm-on-pointer p 0 mx (row-y menu 2) seat)   ; release on it
        (sleep 0.3)
        (ok "...releasing on it runs exactly it"
            (equal *ran* '(2)) (format nil "ran ~s" (reverse *ran*))))

      ;; ---- drag off the menu and let go: nothing runs, menu goes away ------------
      (setf *ran* '())
      (clim-glass::wm-open-menu p 100 100 seat)
      (let* ((menu (clim-glass::seat-menu seat))
             (mx (+ (clim-glass::wm-menu-x menu) 30)))
        (clim-glass::wm-on-pointer p 1 mx (row-y menu 1) seat)
        (clim-glass::wm-on-pointer p 1 900 700 seat)             ; drag well off it
        (clim-glass::wm-on-pointer p 0 900 700 seat)             ; let go out there
        (sleep 0.3)
        (ok "letting go off the menu runs nothing" (null *ran*)
            (format nil "ran ~s" (reverse *ran*)))
        (ok "...and dismisses it" (null (clim-glass::seat-menu seat)))))))

(format t "~&== held and dragged outside, a menu sits still ==~%")
;; THE SECOND EDGE BUG, and the same shape as the first.  Dismissing asked whether a button
;; was down rather than whether one had just gone down — so every motion event outside the
;; menu, mid-drag, dismissed it; and the still-held button then reached the ordinary pointer
;; path, which opens the root menu.  Dismiss, re-open at the new point, once per motion
;; event: the menu flickered and slid along under the cursor.  Two correct-looking
;; behaviours fighting each other.
;;
;; Held and moved outside is a gesture in progress that has not said anything yet, so the
;; right answer is to do nothing at all except stop highlighting.
(setf *ran* '())
(let ((p (clim-glass:make-wm-session :width 1200 :height 800)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "s")
  (sleep 1.5)
  (let* ((seat (clim-glass:port-seat p))
         (items (loop for i from 0 below 5
                      collect (cons (format nil "item ~d" i)
                                    (let ((i i)) (lambda () (push i *ran*)))))))
    (setf (clim-glass::glass-port-menu-items p) items)
    (clim-glass::wm-open-menu p 100 100 seat)
    (let* ((menu (clim-glass::seat-menu seat))
           (mx0 (clim-glass::wm-menu-x menu)) (my0 (clim-glass::wm-menu-y menu))
           (ih (clim-glass::with-seat-scale (seat) (clim-glass::menu-itemh)))
           (th (clim-glass::with-seat-scale (seat) (clim-glass::menu-titleh))))
      ;; press on item 1, then drag well outside, several events, button HELD
      (clim-glass::wm-on-pointer p 1 (+ mx0 30) (+ my0 th ih (floor ih 2)) seat)
      (dolist (pt '((900 300) (905 320) (910 340) (915 360)))
        (clim-glass::wm-on-pointer p 1 (first pt) (second pt) seat) (sleep 0.05))
      (let ((m (clim-glass::seat-menu seat)))
        (ok "held and dragged outside, the menu is still open" (not (null m)))
        (ok "...and has not moved"
            (and m (= mx0 (clim-glass::wm-menu-x m)) (= my0 (clim-glass::wm-menu-y m)))
            (format nil "was ~a,~a now ~a,~a" mx0 my0
                    (and m (clim-glass::wm-menu-x m)) (and m (clim-glass::wm-menu-y m))))
        (ok "...and nothing is highlighted"
            (and m (= -1 (clim-glass::wm-menu-hover m)))
            (format nil "hover ~a" (and m (clim-glass::wm-menu-hover m))))
        (ok "...and nothing has run" (null *ran*) (format nil "ran ~s" *ran*)))
      ;; come back in over item 3 -- still held -- then release there
      (clim-glass::wm-on-pointer p 1 (+ mx0 30) (+ my0 th (* 3 ih) (floor ih 2)) seat)
      (sleep 0.1)
      (ok "coming back in highlights again"
          (= 3 (clim-glass::wm-menu-hover (clim-glass::seat-menu seat)))
          (format nil "hover ~a" (clim-glass::wm-menu-hover (clim-glass::seat-menu seat))))
      (clim-glass::wm-on-pointer p 0 (+ mx0 30) (+ my0 th (* 3 ih) (floor ih 2)) seat)
      (sleep 0.3)
      (ok "...and releasing there runs exactly that item" (equal *ran* '(3))
          (format nil "ran ~s" *ran*)))))

(format t "~&== a fresh click outside still dismisses ==~%")
;; Kept honest: the fix above makes dismissal edge-triggered, so this is the case that
;; would break if the edge were computed wrongly in the other direction.
(let ((p (clim-glass:make-wm-session :width 1200 :height 800)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "x")
  (sleep 1.5)
  (let ((seat (clim-glass:port-seat p)))
    (clim-glass::wm-open-menu p 100 100 seat)
    (ok "the menu is open" (not (null (clim-glass::seat-menu seat))))
    (clim-glass::wm-on-pointer p 0 900 700 seat)      ; move out, no button
    (clim-glass::wm-on-pointer p 1 900 700 seat)      ; a NEW press out there
    (sleep 0.2)
    (ok "...and a fresh press outside closes it" (null (clim-glass::seat-menu seat)))))


(format t "~&== the gesture works from whichever button opened the menu ==~%")
;; A root menu opens on left OR right — (LOGTEST MASK 5) — and selecting watched only the
;; left.  So a right-press-drag-release highlighted its way down the menu, following the
;; pointer exactly as it should, and then chose nothing: the menu just stayed open.  Every
;; visible part of the interaction worked, which is why it reads as "it broke" rather than
;; as a missing feature, and why it needs a test naming the button.
(let ((p (clim-glass:make-wm-session :width 1200 :height 800)))
  (clim-glass:start-wm-session p '())
  (sb-thread:make-thread (lambda () (clim-glass:run-wm-loop p)) :name "r")
  (sleep 1.5)
  (let* ((seat (clim-glass:port-seat p))
         (items (loop for i from 0 below 5
                      collect (cons (format nil "item ~d" i)
                                    (let ((i i)) (lambda () (push i *ran*)))))))
    (setf (clim-glass::glass-port-menu-items p) items)
    (dolist (btn '(1 4))
      (setf *ran* '())
      (clim-glass::wm-on-pointer p btn 300 200 seat)
      (sleep 0.2)
      (let ((m (clim-glass::seat-menu seat)))
        (ok (format nil "button ~d opens the root menu" btn) (not (null m)))
        (when m
          (let* ((ih (clim-glass::with-seat-scale (seat) (clim-glass::menu-itemh)))
                 (th (clim-glass::with-seat-scale (seat) (clim-glass::menu-titleh)))
                 (mx (+ (clim-glass::wm-menu-x m) 30))
                 (py (+ (clim-glass::wm-menu-y m) th (* 2 ih) (floor ih 2))))
            (clim-glass::wm-on-pointer p btn mx py seat)      ; drag, that button held
            (sleep 0.1)
            (ok (format nil "...hover follows a button-~d drag" btn)
                (= 2 (clim-glass::wm-menu-hover m))
                (format nil "hover ~a" (clim-glass::wm-menu-hover m)))
            (ok (format nil "...nothing ran while button ~d was held" btn) (null *ran*)
                (format nil "ran ~s" *ran*))
            (clim-glass::wm-on-pointer p 0 mx py seat)        ; release it
            (sleep 0.3)
            (ok (format nil "...and releasing button ~d chooses that item" btn)
                (equal *ran* '(2)) (format nil "ran ~s" *ran*))
            (ok (format nil "...and dismisses" btn) (null (clim-glass::seat-menu seat)))))))))

(format t "~&~d passed, ~d failed~%=> ~:[FAIL~;PASS~]~%" *pass* *fail* (zerop *fail*))

(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
