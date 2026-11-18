;;;; test/media-pictures-test.lisp -- Media's Pictures tab (glass/media-pictures): the tab only when the
;;;; system is loaded, the folder (GLASS_PICTURES, ~/Pictures, Media's folder) and never above it, the
;;;; listing, thumbnails made off the caller's thread, paging, the viewer and its Prev / Next / Back,
;;;; folders and the up row, the cooperative fallback, and closing.
;;;;   sbcl --script test/media-pictures-test.lisp      (exits 0 iff every check passes)
;;;;
;;;; The fixture is a folder of PNGs drawn here (png-pure), an SVG, a file that is not a picture, a broken
;;;; one, a big one, a subfolder with a subfolder, and two links that lead OUT of the folder (a directory
;;;; and a file).  Set PICTURES_PNG_DIR to also write the window as PNGs (480x520 and 400x700, the list
;;;; and a picture open).

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (make-pathname :name nil :type nil
                     :defaults (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))
      asdf:*central-registry*)
(require :sb-posix)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
    (asdf:load-system :glass/desk-apps)
    (asdf:load-system :warp-media/glass)
    (asdf:load-system :png-pure)))

(defpackage #:glass-media-pictures-test (:use #:cl))
(in-package #:glass-media-pictures-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&MEDIA-PICTURES ~:[FAIL~;ok  ~] ~a~%" ok name)
  (unless ok (incf *fails*))
  ok)

(defun sym (name) (find-symbol name "GLASS.DESK"))
(defun call (name &rest args) (apply (sym name) args))

;;; ---- before glass/media-pictures: Media is Music and Stations -----------------------------------

(check "without glass/media-pictures: Music and Stations, no Pictures tab"
       (equal (glass.desk::%media-tab-keys) '(:music :stations)))
(let ((fb (glass:make-framebuffer 480 520)))
  (multiple-value-bind (k p dirty-p c close-fn) (glass.desk:media-app fb)
    (declare (ignore k p c))
    (funcall dirty-p)
    ;; the second tab starts at half the width: the strip is two shares, as before
    (check "and the strip is two halves, as before"
           (and (/= (glass:fb-get fb 242 18) (glass:fb-get fb 238 18))
                (= (glass:fb-get fb 245 18) (glass:fb-get fb 470 18))))
    (funcall close-fn)))

(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
    (asdf:load-system :glass/media-pictures)))

(check "with it: a third tab, Pictures" (equal (glass.desk::%media-tab-keys) '(:music :stations :pictures)))
(check "Pictures needs pigment (it is loaded with it)" (and (find-package "PIGMENT") t))

;;; ---- the fixture ----------------------------------------------------------------------------------

(defvar *base* (format nil "~a/glass-media-pictures-~d/" (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                       (sb-posix:getpid)))
(defvar *root* (format nil "~aPictures/" *base*))
(defvar *outside* (format nil "~aoutside/" *base*))
(ensure-directories-exist (format nil "~atrip/deeper/" *root*))
(ensure-directories-exist *outside*)

(defun write-picture (path w h fn)
  "A W x H PNG at PATH, FN (x y) -> (values r g b) in 0..1."
  (let ((px (make-array (* w h 3) :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (dotimes (x w)
        (multiple-value-bind (r g b) (funcall fn (/ x (max 1 (1- w))) (/ y (max 1 (1- h))))
          (let ((i (* 3 (+ x (* y w)))))
            (setf (aref px i) (max 0 (min 255 (round (* 255 r))))
                  (aref px (+ i 1)) (max 0 (min 255 (round (* 255 g))))
                  (aref px (+ i 2)) (max 0 (min 255 (round (* 255 b)))))))))
    (png-pure:write-png-file px w h path)))

(defun mix (a b f) (+ a (* (- b a) f)))
(defun in (name) (format nil "~a~a" *root* name))

(write-picture (in "a-sunset.png") 240 160
               (lambda (x y)
                 (cond ((< (+ (expt (- x 0.5) 2) (expt (* 0.66 (- y 0.55)) 2)) 0.012) (values 1 0.9 0.45))
                       ((> y (+ 0.72 (* 0.04 (sin (* 9 x))))) (values 0.12 0.08 0.16))
                       (t (values (mix 0.98 0.42 y) (mix 0.62 0.2 y) (mix 0.3 0.52 y))))))
(write-picture (in "b-forest.png") 150 220
               (lambda (x y)
                 (let ((tree (< (abs (- (mod (* x 4) 1) 0.5)) (* 0.5 (- y 0.3) 1.3))))
                   (cond ((> y 0.9) (values 0.25 0.18 0.1))
                         ((and tree (> y 0.3)) (values 0.1 (mix 0.45 0.25 y) 0.15))
                         (t (values (mix 0.55 0.8 y) (mix 0.75 0.9 y) 0.98))))))
(write-picture (in "c-red.png") 64 48 (lambda (x y) (declare (ignore x y)) (values 1 0 0)))
(write-picture (in "d-green.png") 48 64 (lambda (x y) (declare (ignore x y)) (values 0 1 0)))
(write-picture (in "e-ocean.png") 260 160
               (lambda (x y)
                 (let ((w (* 0.08 (sin (* 18 (+ x (* 0.3 y)))))))
                   (if (< y 0.35) (values 0.7 0.85 0.98)
                       (values 0.05 (mix 0.45 0.2 (+ y w)) (mix 0.8 0.45 (+ y w)))))))
(write-picture (in "f-dunes.png") 220 140
               (lambda (x y)
                 (if (< y (+ 0.4 (* 0.1 (sin (* 7 x)))))
                     (values 0.55 0.75 0.95)
                     (let ((s (* 0.12 (sin (* 30 (+ y (* 0.4 x)))))))
                       (values (+ 0.85 s) (+ 0.65 s) (+ 0.35 s))))))
(write-picture (in "g-aurora.png") 220 150
               (lambda (x y)
                 (let ((g (exp (* -30 (expt (- y 0.4 (* 0.15 (sin (* 8 x)))) 2)))))
                   (values (* 0.1 g) (+ 0.05 (* 0.85 g)) (+ 0.15 (* 0.5 g))))))
(write-picture (in "h-mountains.png") 240 150
               (lambda (x y)
                 (let ((ridge (- 0.85 (* 0.6 (abs (- (mod (* x 2.2) 1) 0.5))))))
                   (cond ((> y (+ 0.25 (* 0.6 (abs (- (mod (* x 2.2) 1) 0.5)))))
                          (if (< y (+ 0.3 (* 0.6 (abs (- (mod (* x 2.2) 1) 0.5))) 0.08)) (values 0.95 0.95 1)
                              (values 0.4 0.42 0.48)))
                         (t (values (mix 0.4 0.75 y) (mix 0.6 0.85 y) ridge))))))
(write-picture (in "i-rings.png") 160 160
               (lambda (x y)
                 (let ((d (sqrt (+ (expt (- x 0.5) 2) (expt (- y 0.5) 2)))))
                   (if (evenp (floor (* d 14))) (values 0.95 0.75 0.2) (values 0.2 0.25 0.5)))))
(write-picture (in "j-night.png") 240 150
               (lambda (x y)
                 (if (< (mod (* 7919 (+ (floor (* x 240)) (* 251 (floor (* y 150))))) 97) 1)
                     (values 1 1 0.9)
                     (values 0.04 0.05 (mix 0.25 0.08 y)))))
(write-picture (in "l-meadow.png") 220 150
               (lambda (x y)
                 (if (< y 0.45) (values (mix 0.5 0.8 y) (mix 0.7 0.9 y) 1)
                     (values (+ 0.25 (* 0.1 (sin (* 40 x)))) (mix 0.7 0.45 y) 0.2))))
(write-picture (in "m-stripes.png") 200 140
               (lambda (x y) (declare (ignore y))
                 (case (mod (floor (* x 6)) 3) (0 (values 0.9 0.3 0.3)) (1 (values 0.95 0.95 0.9)) (t (values 0.2 0.35 0.7)))))
;; the big one: noise, so it is several MB and seconds of pure-CL decode
(let* ((w 2400) (h 1600) (px (make-array (* w h 3) :element-type '(unsigned-byte 8)))
       (state 12345))
  (dotimes (i (* w h))
    (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
    (let ((y (floor i w)))
      (setf (aref px (* 3 i)) (logand (ash state -16) #x3f)
            (aref px (+ (* 3 i) 1)) (+ 40 (floor (* 120 y) h))
            (aref px (+ (* 3 i) 2)) (+ 120 (logand (ash state -8) #x3f)))))
  (png-pure:write-png-file px w h (in "big.png")))
(with-open-file (o (in "broken.png") :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
  (write-sequence (make-array 300 :element-type '(unsigned-byte 8) :initial-element 7) o))
(with-open-file (o (in "k-badge.svg") :direction :output :if-exists :supersede)
  (write-string "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"120\" height=\"120\" viewBox=\"0 0 120 120\"><rect width=\"120\" height=\"120\" rx=\"20\" fill=\"#30406a\"/><circle cx=\"60\" cy=\"60\" r=\"38\" fill=\"#e0a030\"/></svg>" o))
(loop for (name r g b) in '(("n-sky.png" 0.45 0.65 0.95) ("o-sand.png" 0.9 0.8 0.55)
                             ("p-plum.png" 0.45 0.2 0.45) ("q-teal.png" 0.1 0.5 0.5))
      do (let ((r r) (g g) (b b))
           (write-picture (in name) 180 120 (lambda (x y) (values (mix r 1 (* 0.4 x)) (mix g 0 (* 0.3 y)) b)))))
(with-open-file (o (in "notes.txt") :direction :output :if-exists :supersede) (write-line "not a picture" o))
(write-picture (in ".hidden.png") 8 8 (lambda (x y) (declare (ignore x y)) (values 0 0 0)))
(write-picture (in "trip/beach.png") 200 130 (lambda (x y) (if (< y 0.5) (values 0.5 0.8 1) (values 0.95 0.85 0.6))))
(write-picture (in "trip/cliff.png") 130 200 (lambda (x y) (values (mix 0.6 0.3 x) (mix 0.5 0.3 y) 0.3)))
(write-picture (in "trip/deeper/cave.png") 100 80 (lambda (x y) (values (* 0.3 x) (* 0.3 y) 0.2)))
(write-picture (format nil "~asecret.png" *outside*) 50 50 (lambda (x y) (declare (ignore x y)) (values 1 0 1)))
(sb-posix:symlink *outside* (string-right-trim "/" (in "outside")))
(sb-posix:symlink (format nil "~asecret.png" *outside*) (in "escape.png"))

(defvar *names* '("a-sunset.png" "b-forest.png" "big.png" "broken.png" "c-red.png" "d-green.png" "e-ocean.png"
                  "f-dunes.png" "g-aurora.png" "h-mountains.png" "i-rings.png" "j-night.png" "k-badge.svg"
                  "l-meadow.png" "m-stripes.png" "n-sky.png" "o-sand.png" "p-plum.png" "q-teal.png"))

;;; ---- the folder -----------------------------------------------------------------------------------

(defvar *home* (format nil "~ahome/" *base*))
(ensure-directories-exist (format nil "~aPictures/" *home*))
(defvar *bare-home* (format nil "~abare/" *base*))
(ensure-directories-exist *bare-home*)
(defun same-dir (a b) (equal (namestring (truename a)) (namestring (truename b))))

(sb-posix:setenv "GLASS_PICTURES" *root* 1)
(check "GLASS_PICTURES names the folder" (same-dir (glass.desk:pictures-root :home *home*) *root*))
(sb-posix:unsetenv "GLASS_PICTURES")
(check "else ~/Pictures" (same-dir (glass.desk:pictures-root :home *home*) (format nil "~aPictures/" *home*)))
(let ((glass.desk:*media-root* *outside*))
  (check "else Media's folder" (same-dir (glass.desk:pictures-root :home *bare-home*) *outside*)))
(sb-posix:setenv "GLASS_PICTURES" *root* 1)

(multiple-value-bind (dirs images) (glass.desk::pictures-listing (truename *root*) (truename *root*))
  (let ((dn (mapcar #'glass.desk:pic-entry-name dirs)) (in (mapcar #'glass.desk:pic-entry-name images)))
    (check "listing: the pictures pigment reads, sorted; not the text file, not the hidden one"
           (equal in *names*))
    (check "listing: the subfolder" (equal dn '("trip")))
    (check "a link that leads out of the folder (a directory) is not listed" (not (member "outside" dn :test #'string=)))
    (check "nor one to a file outside" (not (member "escape.png" in :test #'string=)))))

;;; ---- the window ---------------------------------------------------------------------------------------

(defun save-png (fb path)
  (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb))
         (px (make-array (* w h 3) :element-type '(unsigned-byte 8))))
    (dotimes (y h) (dotimes (x w)
      (let ((c (glass:fb-get fb x y)) (i (* 3 (+ x (* y w)))))
        (setf (aref px i) (ldb (byte 8 16) c) (aref px (+ i 1)) (ldb (byte 8 8) c) (aref px (+ i 2)) (ldb (byte 8 0) c)))))
    (png-pure:write-png-file px w h path)))

(defvar *png-dir* (let ((d (sb-ext:posix-getenv "PICTURES_PNG_DIR"))) (and d (plusp (length d)) (string-right-trim "/" d))))
(defun shot (fb name) (when *png-dir* (ensure-directories-exist (format nil "~a/" *png-dir*))
                        (save-png fb (format nil "~a/~a" *png-dir* name))))

(defvar *panels* '())
(defmethod initialize-instance :after ((p glass.desk:pictures-panel) &key) (push p *panels*))

(defun targets (pn) (glass.desk::panel-targets pn))
(defun target (pn label) (find label (targets pn) :key #'glass.desk::rt-label :test #'equal))
(defun tap (on-pointer x y) (funcall on-pointer 1 x y) (funcall on-pointer 0 x y))
(defun tap-label (on-pointer pn label)
  "Tap LABEL's target, the panel sitting under the tab strip."
  (let ((tg (target pn label)))
    (when tg
      (tap on-pointer (+ (glass.desk::rt-x tg) (floor (glass.desk::rt-w tg) 2))
           (+ glass.desk::*media-tab-h* (glass.desk::rt-y tg) (floor (glass.desk::rt-h tg) 2)))
      t)))
(defun now () (/ (get-internal-real-time) internal-time-units-per-second))

(defun settle (dirty-p pn seconds)
  "Call DIRTY-P until every picture on PN's page has its thumbnail (or failed).  (values done
   slowest-call-seconds calls-while-waiting)."
  (let ((end (+ (now) seconds)) (slowest 0) (calls 0))
    (loop
      (let ((t0 (now)))
        (funcall dirty-p)
        (setf slowest (max slowest (- (now) t0))))
      (incf calls)
      (let ((page (glass.desk::%pic-page-images pn (glass.desk:pictures-page pn))))
        (when (every (lambda (e) (gethash (glass.desk::%pic-key e) (glass.desk::pictures-thumbs pn))) page)
          (return (values t slowest calls))))
      (when (> (now) end) (return (values nil slowest calls)))
      (sleep 0.01))))

(defun wait-view (dirty-p pn seconds)
  (let ((end (+ (now) seconds)))
    (loop (funcall dirty-p)
          (let ((e (nth (glass.desk:pictures-index pn) (glass.desk:pictures-images pn))))
            (when (and e (gethash (glass.desk::%pic-key e) (glass.desk::pictures-views pn))) (return t)))
          (when (> (now) end) (return nil))
          (sleep 0.01))))

(defun quiet-p (dirty-p seconds)
  (loop with end = (+ (now) seconds) while (< (now) end) never (progn (sleep 0.02) (funcall dirty-p))))

(defun open-media (w h)
  (let ((fb (glass:make-framebuffer w h)))
    (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn resize-fn text-p selection-fn)
        (glass.desk:media-app fb)
      (declare (ignore copy-p))
      (check (format nil "~dx~d: no RESIZE-FN, TEXT-P or SELECTION-FN, as before" w h)
             (and (null resize-fn) (null text-p) (null selection-fn)))
      (values fb on-key on-pointer dirty-p close-fn))))

(let ((glass.desk:*media-root* *root*) (glass.desk::*media-library* nil))
  (multiple-value-bind (fb on-key on-pointer dirty-p close-fn) (open-media 480 520)
    (check "it paints when opened" (funcall dirty-p))
    (check "and Pictures is not made until it is shown (no folder read, nothing decoded)" (null *panels*))
    (check "three tabs, each a fingertip target: a third of the width, the strip's height"
           (and (>= glass.desk::*media-tab-h* 36) (>= (floor 480 3) 48)
                (eq (glass.desk::%media-tab-at '(:music :stations :pictures) 480 100) :music)
                (eq (glass.desk::%media-tab-at '(:music :stations :pictures) 480 240) :stations)
                (eq (glass.desk::%media-tab-at '(:music :stations :pictures) 480 479) :pictures)))
    (tap on-pointer 400 18)
    (let ((t0 (now)))
      (funcall dirty-p)
      (check "a tap on Pictures shows it at once (the first paint does not wait for a decode)"
             (< (- (now) t0) 0.5)))
    (let ((pn (first *panels*)))
      (check "the Pictures tab is the one marked" (= (glass:fb-get fb 400 34) glass.desk::+ra-on+))
      (check "it opens on the folder GLASS_PICTURES names"
             (same-dir (glass.desk:pictures-folder pn) *root*))
      (check "the first page: the subfolder, then pictures, whole cells at least 36 px, labelled by name"
             (let ((cells (remove-if (lambda (tg) (member (glass.desk::rt-label tg) '("Previous page" "Next page")
                                                          :test #'equal))
                                     (targets pn))))
               (and (= 16 (length cells)) (target pn "trip") (target pn "a-sunset.png")
                    (every (lambda (tg) (and (>= (glass.desk::rt-w tg) 36) (>= (glass.desk::rt-h tg) 36))) cells))))
      (check "20 items at 16 a page (4 x 4 at 480x520): a pager" (and (target pn "Next page") (target pn "Previous page")))
      (check "thumbnails are not there yet: they are being made elsewhere"
             (notevery (lambda (e) (gethash (glass.desk::%pic-key e) (glass.desk::pictures-thumbs pn)))
                       (glass.desk::%pic-page-images pn 0)))
      (check "on a decoder thread of the tab's own" (and (glass.desk::pictures-thread pn)
                                                        (sb-thread:thread-alive-p (glass.desk::pictures-thread pn))))
      (multiple-value-bind (done slowest calls) (settle dirty-p pn 90)
        (check "every thumbnail on the page arrives" done)
        (check (format nil "and DIRTY-P never waited on a decode (slowest call ~,3f s over ~d calls, big.png among them)"
                       slowest calls)
               (and (< slowest 0.25) (> calls 5))))
      (let ((th (glass.desk::pictures-thumbs pn)))
        (flet ((thumb (name) (gethash (namestring (truename (in name))) th)))
          (check "big.png (2400x1600) has a thumbnail no bigger than its cell"
                 (let ((v (thumb "big.png"))) (and (consp v) (<= (first v) 120) (= 2400 (fourth v)) (= 1600 (fifth v)))))
          (check "broken.png failed, quietly" (eq (thumb "broken.png") :fail))
          (check "a thumbnail keeps the picture's shape (b-forest is portrait)"
                 (let ((v (thumb "b-forest.png"))) (and (consp v) (< (first v) (second v)))))
          (check "a thumbnail keeps the picture's shape (d-green is 48x64: as tall as the box, 3/4 as wide)"
                 (let ((v (thumb "d-green.png")))
                   (multiple-value-bind (bw bh) (glass.desk::%pic-thumb-box pn)
                     (and (consp v) (< (first v) bw) (= bh (second v)) (= (round (* 48 bh) 64) (first v))))))))
      (check "and then it is quiet: no repaint for a second with nothing new" (quiet-p dirty-p 1))
      (shot fb "pictures-list-480x520.png")
      ;; paging
      (tap-label on-pointer pn "Next page") (funcall dirty-p)
      (check "Next page: the rest" (and (= 1 (glass.desk:pictures-page pn)) (target pn "q-teal.png")
                                        (not (target pn "a-sunset.png"))))
      (check "the next page's thumbnails were made ahead (no wait on turning)"
             (every (lambda (e) (gethash (glass.desk::%pic-key e) (glass.desk::pictures-thumbs pn)))
                    (glass.desk::%pic-page-images pn 1)))
      (settle dirty-p pn 20)
      (check "the SVG is drawn (stencil, through pigment)"
             (consp (gethash (namestring (truename (in "k-badge.svg"))) (glass.desk::pictures-thumbs pn))))
      (tap-label on-pointer pn "Previous page") (funcall dirty-p)
      (check "Previous page: back" (= 0 (glass.desk:pictures-page pn)))
      ;; the viewer
      (tap-label on-pointer pn "c-red.png")
      (funcall dirty-p)
      (let ((e (nth (glass.desk:pictures-index pn) (glass.desk:pictures-images pn))))
        (check "a tap on a picture opens it" (and (eq (glass.desk:pictures-mode pn) :view)
                                                  (equal (glass.desk:pic-entry-name e) "c-red.png")))
        (check "with the big controls: Prev, Back, Next, a third of the width each, 56 px tall"
               (every (lambda (l) (let ((tg (target pn l)))
                                    (and tg (>= (glass.desk::rt-w tg) 160) (>= (glass.desk::rt-h tg) 56))))
                      '("Prev" "Back" "Next"))))
      (check "it is decoded off-thread, and arrives" (wait-view dirty-p pn 20))
      (funcall dirty-p)
      (check "it fills the tab, fitted: red in the middle of the picture area"
             (= #xff0000 (glass:fb-get fb 240 (+ glass.desk::*media-tab-h* 36 (floor (- 484 36 56) 2)))))
      (let ((v (gethash (namestring (truename (in "c-red.png"))) (glass.desk::pictures-views pn))))
        (check "aspect kept (64x48 shown 4:3, upscaled at most 4x)"
               (and (consp v) (= (* 3 (first v)) (* 4 (second v))) (<= (first v) 256))))
      (tap-label on-pointer pn "Next") (funcall dirty-p)
      (check "Next: the next picture" (equal "d-green.png" (glass.desk:pic-entry-name
                                                            (nth (glass.desk:pictures-index pn) (glass.desk:pictures-images pn)))))
      (check "already made (the neighbours are decoded ahead)" (wait-view dirty-p pn 0.2))
      (funcall dirty-p)
      (check "and shown" (= #x00ff00 (glass:fb-get fb 240 (+ glass.desk::*media-tab-h* 36 (floor (- 484 36 56) 2)))))
      (funcall on-key t #xff51) (funcall on-key nil #xff51) (funcall dirty-p)
      (check "the Left key: Prev" (= 4 (glass.desk:pictures-index pn)))
      ;; wrap: from the first, Prev is the last; from the last, Next is the first
      (setf (glass.desk:pictures-index pn) 0 (glass.desk::panel-dirty pn) t) (funcall dirty-p)
      (wait-view dirty-p pn 20)
      (shot fb "pictures-view-480x520.png")
      (tap-label on-pointer pn "Prev") (funcall dirty-p)
      (check "Prev from the first WRAPS to the last" (= 18 (glass.desk:pictures-index pn)))
      (tap-label on-pointer pn "Next") (funcall dirty-p)
      (check "and Next from the last wraps to the first" (= 0 (glass.desk:pictures-index pn)))
      (setf (glass.desk:pictures-index pn) 17 (glass.desk::panel-dirty pn) t) (funcall dirty-p)
      (tap-label on-pointer pn "Back") (funcall dirty-p)
      (check "Back: the list, on the page holding the picture that was showing"
             (and (eq (glass.desk:pictures-mode pn) :list) (= 1 (glass.desk:pictures-page pn))
                  (target pn "p-plum.png")))
      (check "a viewed picture keeps only itself and its neighbours"
             (<= (hash-table-count (glass.desk::pictures-views pn)) 3))
      (tap-label on-pointer pn "Previous page") (funcall dirty-p)
      ;; folders
      (check "at the root there is no up row" (not (target pn "Up")))
      (tap-label on-pointer pn "trip") (funcall dirty-p)
      (check "a tap on a folder opens it" (same-dir (glass.desk:pictures-folder pn) (in "trip/")))
      (check "its pictures and its folder, and an up row"
             (and (target pn "beach.png") (target pn "cliff.png") (target pn "deeper") (target pn "Up")))
      (settle dirty-p pn 20)
      (shot fb "pictures-folder-480x520.png")
      (tap-label on-pointer pn "deeper") (funcall dirty-p)
      (check "and one inside that" (same-dir (glass.desk:pictures-folder pn) (in "trip/deeper/")))
      (tap-label on-pointer pn "Up") (funcall dirty-p)
      (check "Up: the parent" (same-dir (glass.desk:pictures-folder pn) (in "trip/")))
      (funcall on-key t #xff08) (funcall dirty-p)
      (check "BackSpace: up again, to the root" (same-dir (glass.desk:pictures-folder pn) *root*))
      (glass.desk:pictures-up pn)
      (check "up STOPS at the root" (same-dir (glass.desk:pictures-folder pn) *root*))
      (glass.desk:pictures-open pn (in "outside/"))
      (check "the link that leads out is not entered, even asked by name"
             (same-dir (glass.desk:pictures-folder pn) *root*))
      (glass.desk:pictures-open pn *outside*)
      (check "nor the folder it leads to" (same-dir (glass.desk:pictures-folder pn) *root*))
      (glass.desk:pictures-open pn (format nil "~atrip/../../" *root*))
      (check "nor a path that climbs out" (same-dir (glass.desk:pictures-folder pn) *root*))
      (funcall dirty-p)
      ;; a new picture appears without reopening
      (write-picture (in "a-added.png") 40 30 (lambda (x y) (declare (ignore x y)) (values 0 0 1)))
      (let ((glass.desk::*pictures-relist-seconds* 0)) (funcall dirty-p))
      (check "a picture added to the folder appears (the folder is looked at again)" (target pn "a-added.png"))
      (delete-file (in "a-added.png"))
      (let ((glass.desk::*pictures-relist-seconds* 0)) (funcall dirty-p))
      ;; the other tabs still work, and Pictures stays painted while hidden
      (tap on-pointer 240 18) (funcall dirty-p)
      (check "Stations is the middle tab" (= (glass:fb-get fb 240 34) glass.desk::+ra-on+))
      (tap on-pointer 400 18)
      (check "and back on Pictures it shows at once" (funcall dirty-p))
      (let ((th (glass.desk::pictures-thread pn)))
        (funcall close-fn)
        (check "closing the window stops its decoder thread"
               (loop repeat 200 thereis (not (sb-thread:thread-alive-p th)) do (sleep 0.02)))))))

;;; ---- the narrow window ------------------------------------------------------------------------------
(let ((glass.desk:*media-root* *root*) (glass.desk::*media-library* nil))
  (multiple-value-bind (fb on-key on-pointer dirty-p close-fn) (open-media 400 700)
    (declare (ignore on-key))
    (funcall dirty-p)
    (tap on-pointer 350 18) (funcall dirty-p)
    (let ((pn (first *panels*)))
      (check "400x700: three columns of cells, at least 36 px, the whole cell the target"
             (let ((tg (target pn "a-sunset.png"))) (and tg (= 133 (glass.desk::rt-w tg)) (>= (glass.desk::rt-h tg) 36))))
      (check "400x700: a fresh window has its own thumbnail cache"
             (zerop (count-if #'consp (loop for v being the hash-values of (glass.desk::pictures-thumbs pn) collect v))))
      (settle dirty-p pn 90)
      (funcall dirty-p)
      (shot fb "pictures-list-400x700.png")
      (tap-label on-pointer pn "a-sunset.png")
      (wait-view dirty-p pn 20) (funcall dirty-p)
      (check "400x700: the picture fills the width, aspect kept"
             (let ((v (gethash (namestring (truename (in "a-sunset.png"))) (glass.desk::pictures-views pn))))
               (and (consp v) (= 400 (first v)) (<= (abs (- (second v) (round (* 400 160) 240))) 1))))
      (shot fb "pictures-view-400x700.png")
      (funcall close-fn))))

;;; ---- without a thread: cooperative, one picture a turn ---------------------------------------------------
(let* ((fb (glass:make-framebuffer 480 484))
       (pn (let ((glass.desk:*pictures-threads* nil)) (glass.desk:make-pictures-panel fb :root (truename *root*)))))
  (multiple-value-bind (done) (settle (lambda () (glass.desk:pictures-panel-dirty-p pn)) pn 60)
    (check "cooperatively (no thread): the thumbnails still arrive, one a turn" done))
  (check "with no thread made" (null (glass.desk::pictures-thread pn)))
  (check "and a file too big to decode in one turn is shown by name, not decoded"
         (eq :too-big (gethash (namestring (truename (in "big.png"))) (glass.desk::pictures-thumbs pn))))
  (glass.desk:pictures-panel-close pn))

;;; ---- the app on its own -----------------------------------------------------------------------------------
(let ((fb (glass:make-framebuffer 320 240)))
  (multiple-value-bind (k p d c x r) (glass.desk:pictures-app fb)
    (check "PICTURES-APP is a desk app too: five values, no RESIZE-FN" (and (functionp k) (functionp p) (functionp d)
                                                                           (null c) (functionp x) (null r)))
    (funcall d) (funcall x)))

(ignore-errors (sb-ext:run-program "/bin/rm" (list "-rf" *base*)))
(format t "~&MEDIA-PICTURES ~:[FAIL (~d)~;PASS~]~%" (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
