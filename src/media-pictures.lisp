;;;; media-pictures.lisp -- the Pictures tab of the Media window: a folder of images, a page of
;;;; thumbnails at a time, and one picture filling the tab with Prev / Next / Back.
;;;;
;;;; Its own system (glass/media-pictures), because the decoding is pigment's (pure-CL PNG, JPEG,
;;;; GIF, WebP, SVG) and glass/desk-apps does not depend on pigment.  MEDIA-APP finds PICTURES-APP
;;;; BY NAME, so a desk built without this file has Media exactly as before: Music and Stations.
;;;;
;;;; THE FOLDER.  GLASS_PICTURES, else ~/Pictures, else Media's folder (*MEDIA-ROOT*) -- read when
;;;; the tab is first shown, not at load (a saved core is a snapshot of load time).  NEVER ABOVE THAT
;;;; ROOT, as warp's library: folders and files are compared RESOLVED, so a link inside the root
;;;; that points out of it is outside, and is neither listed nor entered.  The up row stops there.
;;;;
;;;; NOTHING SLOW ON THE WINDOW MANAGER'S THREAD.  Reading and decoding a picture (a big JPEG is
;;;; seconds of pure-CL work) and scaling it happen on a DECODER thread, one per open Pictures tab,
;;;; started the first time something needs decoding and stopped when the window closes.  It is
;;;; told what to decode by message and answers by message, through two mailboxes made here and
;;;; handed over as :ARGUMENTS of a global function -- never a closure -- with every reply %SHAREd
;;;; on the way in and its pixels allocated shared (%SHARED-ARRAY).  That is the shape desk-serve
;;;; and rfb.lisp use, so it holds where threads share nothing (modus) as well as on SBCL.  The
;;;; requests are a WISH LIST, replaced whole: the view's picture first, then its neighbours, then
;;;; the page's thumbnails and the next page's -- turn a page and the old page's thumbnails stop
;;;; being made.
;;;;
;;;; WITHOUT A THREAD (*PICTURES-THREADS* NIL, or a host where MAKE-THREAD fails) the same jobs run
;;;; COOPERATIVELY: one picture per DIRTY-P turn, on the caller's thread.  pigment's decoders are not
;;;; resumable, so a turn that decodes one stalls for that one; that is the degraded mode, and a
;;;; file over *PICTURES-COOP-MAX-BYTES* is then shown by name only rather than stall the desktop.
;;;;
;;;; THE CACHE IS THE WINDOW'S.  Thumbnails (scaled to the cell, composited onto its colour) are
;;;; kept per window for as long as it is open, so paging back and returning from a folder is
;;;; instant; a viewed picture keeps itself and its two neighbours.
;;;;
;;;; THE LOOK IS THE STATIONS PANEL'S (+RA-...+ colours, its head, its 48-px pager chevrons, its
;;;; text helpers), and its conventions: PAGED, not scrolled; every target recorded as it is drawn,
;;;; with a label a test can find it by; the whole cell the target; a repaint only when something
;;;; it shows changed.

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

;;; ---- the folder -------------------------------------------------------------------------------------

(defun %pic-dir (s)
  "S (a string or pathname naming a directory), resolved, as a directory pathname, or NIL."
  (and s (plusp (length (namestring s)))
       (ignore-errors (truename (%ra-dir-pathname s)))))

(defun pictures-root (&key (home (user-homedir-pathname)))
  "GLASS_PICTURES, else HOME's Pictures, else *MEDIA-ROOT*, else the current directory: the first
   of them that exists, RESOLVED.  Read at use, not at load."
  (let ((env (sb-ext:posix-getenv "GLASS_PICTURES")))
    (or (and env (plusp (length env)) (%pic-dir env))
        (and home (%pic-dir (merge-pathnames "Pictures/" (%ra-dir-pathname home))))
        (%pic-dir *media-root*)
        (%pic-dir "./"))))

(defun %pic-within-p (root path)
  "Is PATH (resolved) ROOT or inside it?  Directory namestrings end in /, so /a/b is not inside
   /a/bc."
  (let ((r (namestring root)) (p (namestring path)))
    (and (<= (length r) (length p)) (string= r p :end2 (length r)))))

(defclass pic-entry ()
  ((name :initarg :name :reader pic-entry-name :documentation "As listed: a link's own name.")
   (path :initarg :path :reader pic-entry-path :documentation "Resolved: what is opened or read.")
   (kind :initarg :kind :reader pic-entry-kind :documentation ":dir or :image"))
  (:documentation "A folder or a picture in the open folder."))

(defun picture-types () *picture-types*)

(defun %pic-hidden-p (name) (or (zerop (length name)) (char= (char name 0) #\.)))

(defun pictures-listing (root folder)
  "(values DIRS IMAGES) in FOLDER, each sorted by name, case folded.  Only what resolves inside
   ROOT: a link that leads out is not there at all, rather than a row that refuses."
  (flet ((sorted (l) (sort l #'string< :key (lambda (e) (string-downcase (pic-entry-name e))))))
    (let ((dirs '()) (images '()))
      (dolist (d (or (ignore-errors (uiop:subdirectories folder)) '()))
        (let ((name (car (last (pathname-directory d))))
              (to (ignore-errors (truename d))))
          (when (and (stringp name) (not (%pic-hidden-p name)) to (%pic-within-p root to))
            (push (make-instance 'pic-entry :name name :path to :kind :dir) dirs))))
      (dolist (f (or (ignore-errors (uiop:directory-files folder)) '()))
        (let ((name (file-namestring f)) (type (pathname-type f)))
          (when (and type (member type *picture-types* :test #'string-equal) (not (%pic-hidden-p name)))
            (let ((to (ignore-errors (truename f))))
              (when (and to (%pic-within-p root to))
                (push (make-instance 'pic-entry :name name :path to :kind :image) images))))))
      (values (sorted dirs) (sorted images)))))

;;; ---- decoding (the decoder thread's side: no window, no panel, only these arguments) -------------

(defun %pic-read (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence b s) b)))

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

(defun %pic-scale (rgba iw ih box-w box-h bg cap)
  "(values DW DH PIXELS): straight-alpha RGBA IWxIH fitted into BOX-WxBOX-H, aspect kept, made
   at most CAP times bigger, composited onto BG, as X8R8G8B8 in a shared array.  Smaller is an
   area average (a photo's thumbnail would shimmer by nearest neighbour); bigger is bilinear."
  (declare (type (simple-array (unsigned-byte 8) (*)) rgba) (type fixnum iw ih))
  (let* ((s (min (/ box-w iw) (/ box-h ih) cap))
         (dw (max 1 (min box-w (round (* iw s))))) (dh (max 1 (min box-h (round (* ih s)))))
         (out (glass::%shared-array (* dw dh) :element-type '(unsigned-byte 32)))
         (br (ldb (byte 8 16) bg)) (bgg (ldb (byte 8 8) bg)) (bb (ldb (byte 8 0) bg)))
    (declare (type fixnum dw dh br bgg bb) (type (simple-array (unsigned-byte 32) (*)) out))
    (flet ((over (i)
             ;; source pixel I (a byte offset) onto BG: (values r g b)
             (let* ((al (aref rgba (+ i 3))) (rest (- 255 al)))
               (values (round (+ (* al (aref rgba i)) (* rest br)) 255)
                       (round (+ (* al (aref rgba (+ i 1))) (* rest bgg)) 255)
                       (round (+ (* al (aref rgba (+ i 2))) (* rest bb)) 255)))))
      (if (and (<= dw iw) (<= dh ih))
          (dotimes (y dh)
            (let* ((sy0 (floor (* y ih) dh)) (sy1 (max (1+ sy0) (floor (* (1+ y) ih) dh))))
              (declare (type fixnum sy0 sy1))
              (dotimes (x dw)
                (let* ((sx0 (floor (* x iw) dw)) (sx1 (max (1+ sx0) (floor (* (1+ x) iw) dw)))
                       (r 0) (g 0) (b 0) (a 0) (n 0))
                  (declare (type fixnum sx0 sx1 r g b a n))
                  (loop for sy of-type fixnum from sy0 below (min ih sy1)
                        do (loop for i of-type fixnum from (* 4 (+ (* sy iw) sx0))
                                   below (* 4 (+ (* sy iw) (min iw sx1))) by 4
                                 do (let ((al (aref rgba (+ i 3))))
                                      (incf r (* al (aref rgba i))) (incf g (* al (aref rgba (+ i 1))))
                                      (incf b (* al (aref rgba (+ i 2)))) (incf a al) (incf n))))
                  (let* ((n255 (* (max 1 n) 255)) (rest (- n255 a)))
                    (setf (aref out (+ (* y dw) x))
                          (logior (ash (min 255 (round (+ r (* rest br)) n255)) 16)
                                  (ash (min 255 (round (+ g (* rest bgg)) n255)) 8)
                                  (min 255 (round (+ b (* rest bb)) n255)))))))))
          (dotimes (y dh)
            (let* ((fy (max 0.0 (min (float (1- ih)) (- (/ (* (+ y 0.5) ih) dh) 0.5))))
                   (y0 (floor fy)) (y1 (min (1- ih) (1+ y0))) (ty (- fy y0)))
              (dotimes (x dw)
                (let* ((fx (max 0.0 (min (float (1- iw)) (- (/ (* (+ x 0.5) iw) dw) 0.5))))
                       (x0 (floor fx)) (x1 (min (1- iw) (1+ x0))) (tx (- fx x0)))
                  (multiple-value-bind (r00 g00 b00) (over (* 4 (+ (* y0 iw) x0)))
                    (multiple-value-bind (r01 g01 b01) (over (* 4 (+ (* y0 iw) x1)))
                      (multiple-value-bind (r10 g10 b10) (over (* 4 (+ (* y1 iw) x0)))
                        (multiple-value-bind (r11 g11 b11) (over (* 4 (+ (* y1 iw) x1)))
                          (flet ((lerp2 (c00 c01 c10 c11)
                                   (max 0 (min 255 (round (+ (* (- 1 ty) (+ (* (- 1 tx) c00) (* tx c01)))
                                                             (* ty (+ (* (- 1 tx) c10) (* tx c11)))))))))
                            (setf (aref out (+ (* y dw) x))
                                  (logior (ash (lerp2 r00 r01 r10 r11) 16) (ash (lerp2 g00 g01 g10 g11) 8)
                                          (lerp2 b00 b01 b10 b11))))))))))))))
    (values dw dh out)))

(defun %pic-run-job (job)
  "JOB is (KIND KEY PATH BOX-W BOX-H BG CAP MAX-BYTES).  The reply: (KIND KEY W H PIXELS OW OH),
   or (KIND KEY NIL REASON) -- :too-big or :fail.  Never signals."
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

;;; The mailboxes: a struct, as rfb.lisp's RFB-MAILBOX is, because that is what %SHARE is known to
;;; carry between modus threads; the list it holds is shared a cell at a time on the way in.

(defstruct (pic-box (:constructor %make-pic-box))
  (lock (sb-thread:make-mutex :name "glass-pictures"))
  (cv (sb-thread:make-waitqueue :name "glass-pictures"))
  (items '()))

(defun %pic-post (box msg)
  (sb-thread:with-mutex ((pic-box-lock box))
    (setf (pic-box-items box) (glass::%share (cons msg (pic-box-items box))))
    (sb-thread:condition-broadcast (pic-box-cv box)))
  msg)

(defun %pic-take (box &optional wait)
  "Every message, oldest first; with WAIT (seconds), wait that long for one when there is none."
  (sb-thread:with-mutex ((pic-box-lock box))
    (when (and wait (null (pic-box-items box)))
      (sb-thread:condition-wait (pic-box-cv box) (pic-box-lock box) :timeout wait))
    (prog1 (reverse (pic-box-items box)) (setf (pic-box-items box) nil))))

(defun %pictures-worker (requests replies)
  "The decoder thread: wait for a wish list, decode its first job, answer, look again.  A new
   list replaces the old; (:STOP) ends it."
  (let ((jobs '()) (done '()))
    (handler-case
        (loop
          (let ((batch (%pic-take requests (if jobs nil 1))))
            (when (assoc :stop batch) (return-from %pictures-worker nil))
            (let ((latest (find :jobs batch :key #'car :from-end t)))
              ;; what was just answered may be on a list posted before the answer was seen
              (when latest
                (setf jobs (remove-if (lambda (j) (member (second j) done :test #'equal)) (second latest))
                      done '()))))
          (let ((job (pop jobs)))
            (when job
              (%pic-post replies (%pic-run-job job))
              (push (second job) done))))
      (serious-condition () nil))))

;;; ---- the panel ---------------------------------------------------------------------------------------

(defparameter *pic-cell-w* 120 "The grid's cells are about this wide (never fewer than two across).")
(defparameter *pic-name-h* 20 "The name line under a thumbnail.")
(defparameter *pic-bar-h* 56 "The viewer's Prev / Back / Next bar: bigger than a row, for a thumb.")

(defclass pictures-panel ()
  ((fb :initarg :fb :reader panel-fb)
   (root :initarg :root :reader pictures-root-of)
   (folder :initarg :folder :accessor pictures-folder)
   (dirs :initform '() :accessor pictures-dirs)
   (images :initform '() :accessor pictures-images)
   (listed-at :initform 0 :accessor pictures-listed-at)
   (mode :initform :list :accessor pictures-mode :documentation ":list or :view")
   (index :initform 0 :accessor pictures-index :documentation "The picture shown, in :view.")
   (page :initform 0 :accessor pictures-page)
   (targets :initform '() :accessor panel-targets)
   (dirty :initform t :accessor panel-dirty)
   (thumbs :initform (make-hash-table :test 'equal) :reader pictures-thumbs
           :documentation "Resolved namestring -> (W H PIXELS OW OH), or :fail / :too-big.  The window's.")
   (views :initform (make-hash-table :test 'equal) :reader pictures-views)
   (wanted :initform nil :accessor pictures-wanted :documentation "The wish list last posted.")
   (requests :initform nil :accessor pictures-requests)
   (replies :initform nil :accessor pictures-replies)
   (thread :initform nil :accessor pictures-thread)
   (threads :initarg :threads :initform *pictures-threads* :accessor pictures-threads-p))
  (:documentation "The Pictures tab of one Media window."))

(defun make-pictures-panel (fb &key (root (pictures-root)) (threads *pictures-threads*))
  (let ((pn (make-instance 'pictures-panel :fb fb :root root :folder root :threads threads)))
    (%pic-relist pn)
    pn))

(defun %pic-relist (pn)
  "Read the open folder again.  T if what it holds changed."
  (setf (pictures-listed-at pn) (get-internal-real-time))
  (multiple-value-bind (dirs images) (pictures-listing (pictures-root-of pn) (pictures-folder pn))
    (flet ((names (l) (mapcar #'pic-entry-name l)))
      (unless (and (equal (names dirs) (names (pictures-dirs pn)))
                   (equal (names images) (names (pictures-images pn))))
        (setf (pictures-dirs pn) dirs (pictures-images pn) images
              (pictures-index pn) (max 0 (min (pictures-index pn) (1- (length images)))))
        (when (and (eq (pictures-mode pn) :view) (null images)) (setf (pictures-mode pn) :list))
        t))))

(defun pictures-open (pn dir)
  "Navigate to DIR -- only if it resolves inside the root; anything else leaves the folder where it
   is.  Returns the folder."
  (let ((to (%pic-dir dir)))
    (when (and to (%pic-within-p (pictures-root-of pn) to))
      (setf (pictures-folder pn) to (pictures-page pn) 0 (pictures-mode pn) :list (pictures-index pn) 0
            (pictures-dirs pn) '() (pictures-images pn) '() (panel-dirty pn) t)
      (%pic-relist pn))
    (pictures-folder pn)))

(defun pictures-up (pn)
  "The parent folder -- but not past the root, where up is where you already are."
  (let ((f (pictures-folder pn)))
    (if (equal (namestring f) (namestring (pictures-root-of pn)))
        f
        (pictures-open pn (make-pathname :directory (butlast (pathname-directory f)) :name nil :type nil
                                         :defaults f)))))

(defun %pic-at-root-p (pn) (equal (namestring (pictures-folder pn)) (namestring (pictures-root-of pn))))

;;; layout

(defun %pic-grid (pn)
  "(values COLS ROWS CELL-W CELL-H TOP): the grid under the head (and the up row, if any)."
  (let* ((fb (panel-fb pn)) (w (glass:fb-width fb)) (h (glass:fb-height fb))
         (cols (max 2 (floor w *pic-cell-w*)))
         (cw (floor w cols))
         (top (+ *ra-head-h* (if (%pic-at-root-p pn) 0 *ra-row-h*)))
         ;; about square cells, as many rows as come nearest, stretched or squeezed to fill the
         ;; height -- a page with a band of nothing under it reads as a list that ran out
         (room (max 36 (- h top)))
         (rows (max 1 (min (floor room 36) (round room cw))))
         (ch (floor room rows)))
    (values cols rows cw ch top)))

(defun %pic-items (pn) (append (pictures-dirs pn) (pictures-images pn)))

(defun %pic-per-page (pn) (multiple-value-bind (cols rows) (%pic-grid pn) (* cols rows)))

(defun %pic-pages (pn) (max 1 (ceiling (length (%pic-items pn)) (%pic-per-page pn))))

(defun %pic-view-box (pn)
  "(values X Y W H) the viewer's picture area: between the head and the bar."
  (let ((fb (panel-fb pn)))
    (values 0 *ra-head-h* (glass:fb-width fb) (max 1 (- (glass:fb-height fb) *ra-head-h* *pic-bar-h*)))))

(defun %pic-thumb-box (pn)
  (multiple-value-bind (cols rows cw ch) (%pic-grid pn)
    (declare (ignore cols rows))
    (values (max 1 (- cw 12)) (max 1 (- ch 12 *pic-name-h*)))))

(defun %pic-key (entry) (namestring (pic-entry-path entry)))

;;; the decoder: a wish list, posted when it changes

(defun %pic-page-images (pn page)
  (let* ((items (%pic-items pn)) (per (%pic-per-page pn)) (n (length items)))
    (remove-if-not (lambda (e) (eq (pic-entry-kind e) :image))
                   (subseq items (min n (* page per)) (min n (* (1+ page) per))))))

(defun %pic-wish (pn)
  "The jobs wanted now, most wanted first: in the viewer its picture and the two either side;
   then this page's thumbnails, then the next page's."
  (let* ((jobs '()) (images (pictures-images pn)) (n (length images))
         (coop (not (pictures-threads-p pn)))
         (max-bytes (if coop *pictures-coop-max-bytes* *picture-max-bytes*)))
    (when (and (eq (pictures-mode pn) :view) (plusp n))
      (multiple-value-bind (x y w h) (%pic-view-box pn)
        (declare (ignore x y))
        (dolist (d '(0 1 -1))
          (let* ((e (nth (mod (+ (pictures-index pn) d) n) images)) (k (%pic-key e)))
            (unless (or (gethash k (pictures-views pn)) (find k jobs :key #'second :test #'equal))
              (push (list :view k (pic-entry-path e) w h +ra-bg+ 4 max-bytes) jobs))))))
    (multiple-value-bind (bw bh) (%pic-thumb-box pn)
      (dolist (page (list (pictures-page pn) (1+ (pictures-page pn))))
        (dolist (e (%pic-page-images pn page))
          (let ((k (%pic-key e)))
            (unless (or (gethash k (pictures-thumbs pn)) (find k jobs :key #'second :test #'equal))
              (push (list :thumb k (pic-entry-path e) bw bh +ra-row+ 4 max-bytes) jobs))))))
    (nreverse jobs)))

(defun %pic-start-thread (pn)
  "The decoder thread, made the first time there is something to decode.  NIL (and cooperative
   from now on) if this host cannot make one."
  (or (pictures-thread pn)
      (let ((rq (glass::%share (%make-pic-box))) (rp (glass::%share (%make-pic-box))))
        (let ((th (ignore-errors (sb-thread:make-thread #'%pictures-worker :arguments (list rq rp)
                                                                          :name "glass-pictures"))))
          (if th
              (setf (pictures-requests pn) rq (pictures-replies pn) rp (pictures-thread pn) th)
              (progn (setf (pictures-threads-p pn) nil) nil))))))

(defun %pic-store (pn reply)
  "Keep a decoder's answer; T if it changes what is on show."
  (destructuring-bind (kind key w &rest more) reply
    (let ((value (if w (list* w more) (first more))))          ; (W H PIXELS OW OH) or a reason
      (if (eq kind :thumb)
          (progn (setf (gethash key (pictures-thumbs pn)) value)
                 (and (eq (pictures-mode pn) :list)
                      (find key (%pic-page-images pn (pictures-page pn)) :key #'%pic-key :test #'equal)
                      t))
          (let ((cur (nth (pictures-index pn) (pictures-images pn))))
            (setf (gethash key (pictures-views pn)) value)
            (and (eq (pictures-mode pn) :view) cur (equal key (%pic-key cur))))))))

(defun %pic-trim-views (pn)
  "A viewed picture keeps itself and its neighbours; the rest go (they are window-sized)."
  (let* ((images (pictures-images pn)) (n (length images))
         (keep (and (plusp n) (mapcar (lambda (d) (%pic-key (nth (mod (+ (pictures-index pn) d) n) images)))
                                      '(0 1 -1)))))
    (loop for k being the hash-keys of (pictures-views pn)
          unless (member k keep :test #'equal) collect k into gone
          finally (dolist (k gone) (remhash k (pictures-views pn))))))

(defun %pic-pump (pn)
  "Take the decoder's answers and tell it what is wanted now.  T if something on show changed."
  (let ((changed nil))
    (when (pictures-replies pn)
      (dolist (r (%pic-take (pictures-replies pn)))
        (when (%pic-store pn r) (setf changed t))))
    (%pic-trim-views pn)
    (let ((wish (%pic-wish pn)))
      (if (pictures-threads-p pn)
          (unless (equal (mapcar #'second wish) (pictures-wanted pn))
            (when (or wish (pictures-thread pn))
              (when (%pic-start-thread pn)
                (setf (pictures-wanted pn) (mapcar #'second wish))
                (%pic-post (pictures-requests pn) (list :jobs wish)))))
          ;; cooperative: one decode this turn
          (when wish
            (when (%pic-store pn (%pic-run-job (first wish))) (setf changed t)))))
    changed))

;;; drawing

(defun %pic-blit (fb x y w h pixels)
  "PIXELS (W x H, X8R8G8B8) into FB at X,Y, clipped."
  (declare (type (simple-array (unsigned-byte 32) (*)) pixels) (type fixnum x y w h))
  (let* ((fw (glass:fb-width fb)) (fh (glass:fb-height fb))
         (x0 (max 0 x)) (x1 (min fw (+ x w))) (y0 (max 0 y)) (y1 (min fh (+ y h))))
    (when (and (< x0 x1) (< y0 y1))
      (if (eq (glass:fb-format fb) :i420)
          (loop for ty from y0 below y1
                do (loop for tx from x0 below x1
                         do (glass:fb-put fb tx ty (aref pixels (+ (* (- ty y) w) (- tx x))))))
          (let ((dp (glass:fb-pixels fb)))
            (loop for ty from y0 below y1
                  do (replace dp pixels :start1 (+ (* ty fw) x0) :end1 (+ (* ty fw) x1)
                                        :start2 (+ (* (- ty y) w) (- x0 x)))))))
    (glass:fb-touch fb)))

(defun %pic-blit-centred (fb bx by bw bh value)
  (destructuring-bind (w h px &rest original) value
    (declare (ignore original))
    (%pic-blit fb (+ bx (floor (- bw w) 2)) (+ by (floor (- bh h) 2)) w h px)))

(defun %pic-folder-glyph (fb cx cy size)
  (let* ((w size) (h (round (* size 3) 4)) (x (- cx (floor w 2))) (y (- cy (floor h 2))))
    (glass:fb-rect fb x (- y 5) (floor w 3) 6 +ra-dim+)
    (glass:fb-rect fb x y w h +ra-dim+)
    (glass:fb-rect fb (+ x 2) (+ y 2) (- w 4) 2 +ra-control+)))

(defun %pic-centred-text (fb x w baseline text size color)
  (let ((s (%ra-fit text (- w 8) size)))
    (%ra-text fb (+ x (max 4 (floor (- w (%ra-width s size)) 2))) baseline s :size size :color color)))

(defun %pic-target (pn x y w h action label)
  (push (make-instance 'radio-target :x x :y y :w w :h h :action action :label label) (panel-targets pn)))

(defun %pic-control (pn x y w h icon label action)
  (let ((fb (panel-fb pn)))
    (glass:fb-rect fb x y w h +ra-control+)
    (glass:fb-rect fb x y 1 h +ra-bg+)
    (%ra-icon fb icon (+ x (floor w 2)) (+ y (floor h 2)) +ra-fg+)
    (%pic-target pn x y w h action label)))

(defun %pic-title (pn)
  "The open folder, from the root down: Pictures / Trips / 2024."
  (let ((root (pathname-directory (pictures-root-of pn))) (f (pathname-directory (pictures-folder pn))))
    (format nil "~{~a~^ / ~}" (cons "Pictures" (nthcdr (length root) f)))))

(defun %pic-head (pn title info &optional pager)
  "The head: TITLE at the left, INFO at the right, and the pager chevrons when PAGER."
  (let* ((fb (panel-fb pn)) (w (glass:fb-width fb)) (right (- w 8)))
    (glass:fb-rect fb 0 0 w *ra-head-h* +ra-head+)
    (when pager
      (let ((page (pictures-page pn)) (pages (%pic-pages pn)))
        (%pic-control pn (- w (* 2 *ra-control-w*)) 0 *ra-control-w* *ra-head-h* :left "Previous page"
                      (lambda () (setf (pictures-page pn) (max 0 (1- page)))))
        (%pic-control pn (- w *ra-control-w*) 0 *ra-control-w* *ra-head-h* :right "Next page"
                      (lambda () (setf (pictures-page pn) (min (1- pages) (1+ page)))))
        (decf right (+ 8 (* 2 *ra-control-w*)))))
    (let* ((iw (%ra-width info 10)))
      (%ra-text fb (- right iw) (%ra-baseline 0 *ra-head-h* 10) info :size 10 :color +ra-dim+)
      (%ra-text fb 8 (%ra-baseline 0 *ra-head-h* 12) (%ra-fit title (- right iw 24) 12) :size 12))))

(defun %pic-up-icon (fb cx cy color)
  (loop for i from 0 to 6
        do (glass:fb-rect fb (- cx i) (+ cy -4 i) (1+ (* 2 i)) 2 color)))

(defun %pic-draw-list (pn)
  (let* ((fb (panel-fb pn)) (w (glass:fb-width fb))
         (items (%pic-items pn)) (n (length items))
         (pages (%pic-pages pn))
         (page (setf (pictures-page pn) (max 0 (min (pictures-page pn) (1- pages)))))
         (nd (length (pictures-dirs pn))) (ni (length (pictures-images pn))))
    (%pic-head pn (%pic-title pn)
               (format nil "~[~:;~:*~d folder~:p  ~]~d picture~:p~@[  ~a~]" nd ni
                       (and (> pages 1) (format nil "~d/~d" (1+ page) pages)))
               (> pages 1))
    (unless (%pic-at-root-p pn)
      (let* ((y *ra-head-h*)
             (parent (butlast (pathname-directory (pictures-folder pn))))
             (pname (if (<= (length parent) (length (pathname-directory (pictures-root-of pn))))
                        "Pictures" (car (last parent)))))
        (glass:fb-rect fb 0 y w *ra-row-h* +ra-row+)
        (glass:fb-rect fb 0 (+ y *ra-row-h* -1) w 1 +ra-bg+)
        (%pic-up-icon fb 20 (+ y (floor *ra-row-h* 2)) +ra-fg+)
        (%ra-text fb 40 (%ra-baseline y *ra-row-h* 12) (%ra-fit (format nil "Up to ~a" pname) (- w 50) 12)
                  :size 12)
        (%pic-target pn 0 y w *ra-row-h* (lambda () (pictures-up pn)) "Up")))
    (multiple-value-bind (cols rows cw ch top) (%pic-grid pn)
      (declare (ignore rows))
      (multiple-value-bind (bw bh) (%pic-thumb-box pn)
        (when (zerop n)
          (%pic-centred-text fb 0 w (+ top 40) "No pictures here" 13 +ra-dim+)
          (%pic-centred-text fb 0 w (+ top 60)
                             (format nil "PNG, JPEG, GIF, WebP or SVG files in ~a" (namestring (pictures-folder pn)))
                             10 +ra-dim+))
        (loop for e in (subseq items (min n (* page (%pic-per-page pn))) (min n (* (1+ page) (%pic-per-page pn))))
              for i from 0
              do (let* ((e e) (x (* (mod i cols) cw)) (y (+ top (* (floor i cols) ch)))
                        ;; the last column takes what the division left over
                        (cwi (if (= (mod i cols) (1- cols)) (- w x) cw))
                        (bx (+ x 6)) (by (+ y 6)))
                   (glass:fb-rect fb (+ x 2) (+ y 2) (- cwi 4) (- ch 4) +ra-row+)
                   (if (eq (pic-entry-kind e) :dir)
                       (progn (%pic-folder-glyph fb (+ bx (floor bw 2)) (+ by (floor bh 2) 2) (min 48 (floor bw 2)))
                              (%pic-centred-text fb x cwi (- (+ y ch) 10) (pic-entry-name e) 11 +ra-fg+)
                              (%pic-target pn x y cwi ch (lambda () (pictures-open pn (pic-entry-path e)))
                                           (pic-entry-name e)))
                       (let ((th (gethash (%pic-key e) (pictures-thumbs pn)))
                             (idx (position e (pictures-images pn))))
                         (cond ((consp th) (%pic-blit-centred fb bx by bw bh th))
                               ((null th) (%pic-centred-text fb bx bw (+ by (floor bh 2) 4) "..." 12 +ra-dim+))
                               (t (%pic-centred-text fb bx bw (+ by (floor bh 2) 6)
                                                     (if (eq th :too-big) "too big" "?") (if (eq th :too-big) 10 20)
                                                     +ra-dim+)))
                         (%pic-centred-text fb x cwi (- (+ y ch) 10) (pic-entry-name e) 10 +ra-dim+)
                         (%pic-target pn x y cwi ch
                                      (lambda () (setf (pictures-mode pn) :view (pictures-index pn) idx))
                                      (pic-entry-name e))))))))))

(defun %pic-step (pn d)
  "The next (D 1) or previous (D -1) picture -- WRAPPING: past the last is the first."
  (let ((n (length (pictures-images pn))))
    (when (plusp n) (setf (pictures-index pn) (mod (+ (pictures-index pn) d) n)))))

(defun %pic-back (pn)
  "Back to the list, on the page holding the picture that was showing."
  (let ((pos (position (nth (pictures-index pn) (pictures-images pn)) (%pic-items pn))))
    (setf (pictures-mode pn) :list)
    (when pos (setf (pictures-page pn) (floor pos (%pic-per-page pn))))))

(defun %pic-draw-view (pn)
  (let* ((fb (panel-fb pn)) (w (glass:fb-width fb)) (h (glass:fb-height fb))
         (images (pictures-images pn)) (n (length images))
         (e (nth (pictures-index pn) images))
         (v (and e (gethash (%pic-key e) (pictures-views pn)))))
    (%pic-head pn (if e (pic-entry-name e) "")
               (format nil "~@[~a  ~]~d/~d" (and (consp v) (format nil "~dx~d" (fourth v) (fifth v)))
                       (1+ (pictures-index pn)) n))
    (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
      (glass:fb-rect fb bx by bw bh +ra-bg+)
      (cond ((consp v) (%pic-blit-centred fb bx by bw bh v))
            ((null v)
             (let ((th (and e (gethash (%pic-key e) (pictures-thumbs pn)))))
               (when (consp th) (%pic-blit-centred fb bx by bw (- bh 40) th)))
             (%pic-centred-text fb bx bw (- (+ by bh) 16) "Loading..." 12 +ra-dim+))
            (t (%pic-centred-text fb bx bw (+ by (floor bh 2))
                                  (if (eq v :too-big) "Too big to show here" "Can't show this picture")
                                  13 +ra-dim+))))
    ;; the bar: three big buttons, the whole third each
    (let* ((y (- h *pic-bar-h*)) (third (floor w 3)))
      (loop for (label icon action) in (list (list "Prev" :left (lambda () (%pic-step pn -1)))
                                             (list "Back" nil (lambda () (%pic-back pn)))
                                             (list "Next" :right (lambda () (%pic-step pn 1))))
            for x from 0 by third
            for bw = (if (string= label "Next") (- w (* 2 third)) third)
            do (let* ((tw (%ra-width label 13))
                      (gw (if icon (+ tw 22) tw))
                      (gx (+ x (floor (- bw gw) 2)))
                      (cy (+ y (floor *pic-bar-h* 2))))
                 (glass:fb-rect fb x y bw *pic-bar-h* +ra-control+)
                 (glass:fb-rect fb x y 1 *pic-bar-h* +ra-bg+)
                 (glass:fb-rect fb x y bw 1 +ra-bg+)
                 (case icon
                   (:left (%ra-icon fb :left (+ gx 6) cy +ra-fg+)
                    (%ra-text fb (+ gx 22) (%ra-baseline y *pic-bar-h* 13) label :size 13))
                   (:right (%ra-text fb gx (%ra-baseline y *pic-bar-h* 13) label :size 13)
                    (%ra-icon fb :right (+ gx tw 14) cy +ra-fg+))
                   (t (%ra-text fb gx (%ra-baseline y *pic-bar-h* 13) label :size 13)))
                 (%pic-target pn x y bw *pic-bar-h* action label))))))

(defun pictures-panel-redraw (pn)
  (let ((fb (panel-fb pn)))
    (setf (panel-targets pn) '())
    (glass:fb-rect fb 0 0 (glass:fb-width fb) (glass:fb-height fb) +ra-bg+)
    (if (and (eq (pictures-mode pn) :view) (pictures-images pn))
        (%pic-draw-view pn)
        (progn (setf (pictures-mode pn) :list) (%pic-draw-list pn)))
    t))

;;; input and the turn

(defun pictures-panel-press (pn x y)
  "A press at X,Y in the panel's framebuffer: the last-drawn target under it.  T if one was hit."
  (let ((tg (find-if (lambda (r) (and (<= (rt-x r) x (+ (rt-x r) (rt-w r) -1))
                                      (<= (rt-y r) y (+ (rt-y r) (rt-h r) -1))))
                     (panel-targets pn))))
    (when tg
      (ignore-errors (funcall (rt-action tg)))
      (setf (panel-dirty pn) t)
      t)))

(defun pictures-panel-key (pn keysym)
  "Left / Right: the previous or next picture (or page); Escape or BackSpace: back to the list, or
   up a folder."
  (let ((view (eq (pictures-mode pn) :view)))
    (case keysym
      (#xff51 (if view (%pic-step pn -1) (setf (pictures-page pn) (max 0 (1- (pictures-page pn))))))
      (#xff53 (if view (%pic-step pn 1) (setf (pictures-page pn) (min (1- (%pic-pages pn)) (1+ (pictures-page pn))))))
      (#xff55 (unless view (setf (pictures-page pn) (max 0 (1- (pictures-page pn))))))
      (#xff56 (unless view (setf (pictures-page pn) (min (1- (%pic-pages pn)) (1+ (pictures-page pn))))))
      ((#xff1b #xff08) (if view (%pic-back pn) (pictures-up pn)))
      (t (return-from pictures-panel-key nil)))
    (setf (panel-dirty pn) t)
    t))

(defun pictures-panel-dirty-p (pn)
  "Repaint iff something it shows changed: a tap, a thumbnail or picture arriving for what is on
   show, or (looked at every few seconds) the folder.  Never waits on a decode."
  (when (> (- (get-internal-real-time) (pictures-listed-at pn))
           (* *pictures-relist-seconds* internal-time-units-per-second))
    (when (%pic-relist pn) (setf (panel-dirty pn) t)))
  ;; the wish list follows what the last tap changed, so make the picture first
  (when (%pic-pump pn) (setf (panel-dirty pn) t))
  (when (panel-dirty pn)
    (setf (panel-dirty pn) nil)
    (pictures-panel-redraw pn)))

(defun pictures-panel-close (pn)
  (when (pictures-requests pn) (%pic-post (pictures-requests pn) (list :stop)))
  (setf (pictures-thread pn) nil))

;;; ---- as an application: the contract MEDIA-APP (and a desk) calls --------------------------------

(defun pictures-app (fb)
  "Pictures in FB: (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN) -- no RESIZE-FN, no TEXT-P.
   MEDIA-APP finds this by name for its Pictures tab."
  (let ((pn (make-pictures-panel fb)) (down nil))
    (values (lambda (pressed keysym) (when pressed (pictures-panel-key pn keysym)))
            (lambda (mask x y)                ; on the press, not on every move while held
              (let ((now (logbitp 0 mask)))
                (when (and now (not down)) (pictures-panel-press pn x y))
                (setf down now)))
            (lambda () (pictures-panel-dirty-p pn))
            nil
            (lambda () (pictures-panel-close pn)))))
