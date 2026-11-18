;;;; media-pictures.lisp -- the Pictures tab of the Media window: a folder of images, a page of
;;;; thumbnails at a time, and one picture filling the tab with Prev / Next / Back.
;;;;
;;;; Its own system (glass/media-pictures), because the decoding is pigment's (pure-CL PNG, JPEG,
;;;; GIF, WebP, SVG) and glass/desk-apps does not depend on pigment.  MEDIA-APP finds PICTURES-APP
;;;; BY NAME, so a desk built without this file has Media exactly as before: Music and Stations.
;;;;
;;;; What every folder tab does -- the folder and never above its root, the paged grid, the worker
;;;; thread fed a replacing wish list through mailboxes, the cooperative fallback, the viewer's bar,
;;;; DIRTY-P -- is media-folder.lisp's (FOLDER-PANEL), and its header says how.  This file is what is
;;;; Pictures' own:
;;;;
;;;; THE FOLDER.  GLASS_PICTURES, else ~/Pictures, else Media's folder (*MEDIA-ROOT*).
;;;;
;;;; THE JOBS.  Reading and decoding a picture and scaling it (%PIC-RUN-JOB, on the worker).  The wish
;;;; list: the view's picture first, then its neighbours, then the page's thumbnails and the next
;;;; page's -- turn a page and the old page's thumbnails stop being made.
;;;;
;;;; COOPERATIVELY (*PICTURES-THREADS* NIL), one picture per DIRTY-P turn: pigment's decoders are not
;;;; resumable, so a turn that decodes one stalls for that one, and a file over
;;;; *PICTURES-COOP-MAX-BYTES* is then shown by name only rather than stall the desktop.
;;;;
;;;; THE CACHE IS THE WINDOW'S.  Thumbnails (scaled to the cell, composited onto its colour) are
;;;; kept per window for as long as it is open, so paging back and returning from a folder is
;;;; instant; a viewed picture keeps itself and its two neighbours.  Prev / Next WRAP: a folder of
;;;; pictures has no first page to stop at.

(in-package #:glass.desk)

(export '(pictures-app pictures-root pictures-panel make-pictures-panel pictures-panel-dirty-p
          pictures-panel-press pictures-panel-key pictures-panel-close pictures-open pictures-up
          picture-types *picture-types* *pictures-threads* *pictures-coop-max-bytes*
          pictures-folder pictures-mode pictures-index pictures-page pictures-dirs pictures-images
          pic-entry-name pic-entry-path))

;;; ---- formats and limits ----------------------------------------------------------------------------

(defparameter *picture-types* '("png" "jpg" "jpeg" "gif" "webp" "svg")
  "What pigment decodes, by extension: PNG (all colour types, Adam7), JPEG (baseline and
   progressive, turned upright by its EXIF orientation), GIF (the first frame), WebP (lossy and
   lossless, with alpha), SVG (rendered through stencil).  A file listed here that pigment cannot
   read shows as a cell with a question mark, never as an error.")

(defparameter *picture-max-bytes* (* 64 1024 1024) "A bigger file is not decoded.")
(defparameter *picture-max-megapixels* 80 "Nor one whose header says it is bigger than this.")

(defparameter *pictures-threads* t
  "Decode on a thread of the tab's own (the default).  NIL: decode cooperatively, one picture per
   DIRTY-P turn on the caller's thread -- see the header.")

(defparameter *pictures-coop-max-bytes* (* 512 1024)
  "Cooperatively, a file bigger than this is not decoded: a turn must not stall on it.")

(defparameter *pictures-relist-seconds* 5 "How often the open folder is looked at again.")

(defun picture-types () *picture-types*)

(defun pictures-root (&key (home (user-homedir-pathname)))
  "GLASS_PICTURES, else HOME's Pictures, else *MEDIA-ROOT*, else the current directory: the first
   of them that exists, RESOLVED.  Read at use, not at load."
  (%folder-root "GLASS_PICTURES" "Pictures/" home))

(defun pictures-listing (root folder)
  "(values DIRS IMAGES) in FOLDER: FOLDER-LISTING of the picture types."
  (folder-listing root folder *picture-types*))

;;; ---- decoding (the worker's side: no window, no panel, only these arguments) -----------------------

(defun %pic-probe-size (bytes)
  "A PNG's or JPEG's (values W H) from its header, without decoding: the megapixel cap's look."
  (cond ((and (>= (length bytes) 24) (= (aref bytes 0) 137) (= (aref bytes 1) 80)
              (= (aref bytes 12) 73) (= (aref bytes 13) 72) (= (aref bytes 14) 68) (= (aref bytes 15) 82))
         (flet ((u32 (o) (logior (ash (aref bytes o) 24) (ash (aref bytes (+ o 1)) 16)
                                 (ash (aref bytes (+ o 2)) 8) (aref bytes (+ o 3)))))
           (values (u32 16) (u32 20))))
        ((and (>= (length bytes) 3) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))
         (ignore-errors (pigment:jpeg-size bytes)))
        (t (values nil nil))))

(defun %pic-run-job (job &optional cache)
  "JOB is (KIND KEY PATH BOX-W BOX-H BG CAP MAX-BYTES).  The reply: (KIND KEY W H PIXELS OW OH),
   or (KIND KEY NIL REASON) -- :too-big or :fail.  Never signals.  Pictures keeps no CACHE."
  (declare (ignore cache))
  (destructuring-bind (kind key path box-w box-h bg cap max-bytes) job
    (handler-case
        (let ((size (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))))
          (if (> size max-bytes)
              (list kind key nil :too-big)
              (let ((bytes (%pic-read path)))
                (multiple-value-bind (pw ph) (%pic-probe-size bytes)
                  (if (and pw ph (> (* pw ph) (* *picture-max-megapixels* 1000000)))
                      (list kind key nil :too-big)
                      (let ((img (pigment:decode-image-bytes
                                  bytes (and (string-equal (pathname-type path) "svg") "image/svg+xml")
                                  :width box-w :height box-h)))
                        (if (and img (plusp (pigment:img-w img)) (plusp (pigment:img-h img)))
                            (multiple-value-bind (w h px)
                                (%pic-scale (pigment:img-rgba img) (pigment:img-w img) (pigment:img-h img)
                                            box-w box-h bg cap)
                              (list kind key w h px (pigment:img-w img) (pigment:img-h img)))
                            (list kind key nil :fail))))))))
      (serious-condition () (list kind key nil :fail)))))

;;; ---- the panel ---------------------------------------------------------------------------------------

(defclass pictures-panel (folder-panel) ()
  (:documentation "The Pictures tab of one Media window."))

(defun make-pictures-panel (fb &key (root (pictures-root)) (threads *pictures-threads*))
  (let ((pn (make-instance 'pictures-panel :fb fb :root root :folder root :threads threads)))
    (folder-relist pn)
    pn))

(defmethod folder-label ((pn pictures-panel)) "Pictures")
(defmethod folder-types ((pn pictures-panel)) *picture-types*)
(defmethod folder-runner ((pn pictures-panel)) '%pic-run-job)
(defmethod folder-thread-name ((pn pictures-panel)) "glass-pictures")
(defmethod folder-relist-seconds ((pn pictures-panel)) *pictures-relist-seconds*)
(defmethod folder-empty-hint ((pn pictures-panel)) "PNG, JPEG, GIF, WebP or SVG files")
(defmethod folder-list-info ((pn pictures-panel) nd ni)
  (format nil "~[~:;~:*~d folder~:p  ~]~d picture~:p" nd ni))

(defun pictures-open (pn dir) (folder-open pn dir))
(defun pictures-up (pn) (folder-up pn))
(defun %pic-relist (pn) (folder-relist pn))

(defmethod folder-wish ((pn pictures-panel))
  "In the viewer its picture and the two either side; then this page's thumbnails, then the next
   page's."
  (let* ((jobs '()) (images (folder-files pn)) (n (length images))
         (coop (not (folder-threads-p pn)))
         (max-bytes (if coop *pictures-coop-max-bytes* *picture-max-bytes*)))
    (when (and (eq (folder-mode pn) :view) (plusp n))
      (multiple-value-bind (x y w h) (%pic-view-box pn)
        (declare (ignore x y))
        (dolist (d '(0 1 -1))
          (let* ((e (nth (mod (+ (folder-index pn) d) n) images)) (k (%pic-key e)))
            (unless (or (gethash k (folder-views pn)) (find k jobs :key #'second :test #'equal))
              (push (list :view k (pic-entry-path e) w h +ra-bg+ 4 max-bytes) jobs))))))
    (multiple-value-bind (bw bh) (folder-thumb-box pn)
      (dolist (page (list (folder-page pn) (1+ (folder-page pn))))
        (dolist (e (%pic-page-images pn page))
          (let ((k (%pic-key e)))
            (unless (or (gethash k (folder-thumbs pn)) (find k jobs :key #'second :test #'equal))
              (push (list :thumb k (pic-entry-path e) bw bh +ra-row+ 4 max-bytes) jobs))))))
    (nreverse jobs)))

(defmethod folder-store ((pn pictures-panel) reply)
  (destructuring-bind (kind key w &rest more) reply
    (let ((value (if w (list* w more) (first more))))          ; (W H PIXELS OW OH) or a reason
      (if (eq kind :thumb)
          (progn (setf (gethash key (folder-thumbs pn)) value)
                 (and (eq (folder-mode pn) :list)
                      (find key (%pic-page-images pn (folder-page pn)) :key #'%pic-key :test #'equal)
                      t))
          (let ((cur (nth (folder-index pn) (folder-files pn))))
            (setf (gethash key (folder-views pn)) value)
            (and (eq (folder-mode pn) :view) cur (equal key (%pic-key cur))))))))

(defmethod folder-trim ((pn pictures-panel))
  "A viewed picture keeps itself and its neighbours; the rest go (they are window-sized)."
  (let* ((images (folder-files pn)) (n (length images))
         (keep (and (plusp n) (mapcar (lambda (d) (%pic-key (nth (mod (+ (folder-index pn) d) n) images)))
                                      '(0 1 -1)))))
    (loop for k being the hash-keys of (folder-views pn)
          unless (member k keep :test #'equal) collect k into gone
          finally (dolist (k gone) (remhash k (folder-views pn))))))

;;; drawing

(defmethod folder-draw-cell ((pn pictures-panel) e x y cwi ch)
  (let ((fb (panel-fb pn)) (bx (+ x 6)) (by (+ y 6)))
    (multiple-value-bind (bw bh) (folder-thumb-box pn)
      (glass:fb-rect fb (+ x 2) (+ y 2) (- cwi 4) (- ch 4) +ra-row+)
      (let ((th (gethash (%pic-key e) (folder-thumbs pn)))
            (idx (position e (folder-files pn))))
        (cond ((consp th) (%pic-blit-centred fb bx by bw bh th))
              ((null th) (%pic-centred-text fb bx bw (+ by (floor bh 2) 4) "..." 12 +ra-dim+))
              (t (%pic-centred-text fb bx bw (+ by (floor bh 2) 6)
                                    (if (eq th :too-big) "too big" "?") (if (eq th :too-big) 10 20)
                                    +ra-dim+)))
        (%pic-centred-text fb x cwi (- (+ y ch) 10) (pic-entry-name e) 10 +ra-dim+)
        (%pic-target pn x y cwi ch
                     (lambda () (setf (folder-mode pn) :view (folder-index pn) idx))
                     (pic-entry-name e))))))

(defmethod folder-step ((pn pictures-panel) d)
  "The next (D 1) or previous (D -1) picture -- WRAPPING: past the last is the first."
  (let ((n (length (folder-files pn))))
    (when (plusp n) (setf (folder-index pn) (mod (+ (folder-index pn) d) n)))))

(defun %pic-step (pn d) (folder-step pn d))

(defmethod folder-draw-view ((pn pictures-panel))
  (let* ((fb (panel-fb pn))
         (images (folder-files pn)) (n (length images))
         (e (nth (folder-index pn) images))
         (v (and e (gethash (%pic-key e) (folder-views pn)))))
    (%pic-head pn (if e (pic-entry-name e) "")
               (format nil "~@[~a  ~]~d/~d" (and (consp v) (format nil "~dx~d" (fourth v) (fifth v)))
                       (1+ (folder-index pn)) n))
    (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
      (glass:fb-rect fb bx by bw bh +ra-bg+)
      (cond ((consp v) (%pic-blit-centred fb bx by bw bh v))
            ((null v)
             (let ((th (and e (gethash (%pic-key e) (folder-thumbs pn)))))
               (when (consp th) (%pic-blit-centred fb bx by bw (- bh 40) th)))
             (%pic-centred-text fb bx bw (- (+ by bh) 16) "Loading..." 12 +ra-dim+))
            (t (%pic-centred-text fb bx bw (+ by (floor bh 2))
                                  (if (eq v :too-big) "Too big to show here" "Can't show this picture")
                                  13 +ra-dim+))))
    (%folder-bar pn)))

;;; the contract

(defun pictures-panel-press (pn x y) (folder-panel-press pn x y))
(defun pictures-panel-key (pn keysym) (folder-panel-key pn keysym))
(defun pictures-panel-dirty-p (pn) (folder-panel-dirty-p pn))
(defun pictures-panel-close (pn) (folder-panel-close pn))

(defun pictures-app (fb)
  "Pictures in FB: (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN) -- no RESIZE-FN, no TEXT-P.
   MEDIA-APP finds this by name for its Pictures tab."
  (%folder-app (make-pictures-panel fb)))
