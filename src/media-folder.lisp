;;;; media-folder.lisp -- what Media's Pictures and Documents tabs share: a folder that is never left
;;;; for one above it, a paged view of what is in it, a worker thread of the tab's own fed a replacing
;;;; wish list through mailboxes, a cooperative fallback for a host with no threads, one item opened
;;;; to fill the tab with a Prev / Back / Next bar, and the DIRTY-P discipline.
;;;;
;;;; Its own system (glass/media-folder) because neither tab's decoder belongs in it: Pictures brings
;;;; pigment, Documents brings folio, and this file needs neither.  A tab is a subclass of FOLDER-PANEL
;;;; that says what files it lists (FOLDER-TYPES), how to make what it shows (FOLDER-RUNNER, a GLOBAL
;;;; function the worker calls -- never a closure, so it crosses a modus thread), what it wants made now
;;;; (FOLDER-WISH), what to keep of an answer (FOLDER-STORE), and how a cell and the opened item look.
;;;;
;;;; THE FOLDER.  An environment variable, else a folder in HOME, else Media's folder (*MEDIA-ROOT*) --
;;;; read when the tab is first shown, not at load (a saved core is a snapshot of load time).  NEVER
;;;; ABOVE THAT ROOT, as warp's library: folders and files are compared RESOLVED, so a link inside the
;;;; root that points out of it is outside, and is neither listed nor entered.  The up row stops there.
;;;;
;;;; NOTHING SLOW ON THE WINDOW MANAGER'S THREAD.  Decoding (a big JPEG, a heavy PDF page: seconds of
;;;; pure-CL work) happens on a WORKER thread, one per open tab, started the first time something needs
;;;; making and stopped when the window closes.  It is told what to make by message and answers by
;;;; message, through two mailboxes made here and handed over as :ARGUMENTS of a global function, with
;;;; every reply %SHAREd on the way in and its pixels allocated shared (%SHARED-ARRAY).  That is the
;;;; shape desk-serve and rfb.lisp use, so it holds where threads share nothing (modus) as well as on
;;;; SBCL.  The requests are a WISH LIST, replaced whole: what is on show first, then its neighbours,
;;;; then the page's thumbnails and the next page's.  The worker keeps a cache of its own (a hash table
;;;; it made, on its thread) that the runner may use -- Documents keeps its parsed PDFs there -- and
;;;; that goes when the thread does.
;;;;
;;;; WITHOUT A THREAD (the tab's THREADS NIL, or a host where MAKE-THREAD fails) the same jobs run
;;;; COOPERATIVELY: one job per DIRTY-P turn, on the caller's thread, with a cache the panel holds
;;;; until it is closed.  The decoders are not resumable, so a turn that runs one stalls for that one;
;;;; that is the degraded mode, and each tab says what it refuses to make in it.
;;;;
;;;; THE LOOK IS THE STATIONS PANEL'S (+RA-...+ colours, its head, its 48-px pager chevrons, its text
;;;; helpers), and its conventions: PAGED, not scrolled; every target recorded as it is drawn, with a
;;;; label a test can find it by; the whole cell the target; a repaint only when something it shows
;;;; changed.

(in-package #:glass.desk)

(export '(folder-panel folder-open folder-up folder-panel-press folder-panel-key folder-panel-dirty-p
          folder-panel-pointer
          folder-panel-close folder-path folder-mode folder-index folder-page folder-dirs folder-files
          folder-thumbs folder-views folder-thread folder-listing pic-entry pic-entry-name pic-entry-path
          pic-entry-kind))

;;; ---- the folder -------------------------------------------------------------------------------------

(defun %pic-dir (s)
  "S (a string or pathname naming a directory), resolved, as a directory pathname, or NIL."
  (and s (plusp (length (namestring s)))
       (ignore-errors (truename (%ra-dir-pathname s)))))

(defun %folder-root (env subdir home)
  "The environment variable ENV, else HOME's SUBDIR, else *MEDIA-ROOT*, else the current directory:
   the first of them that exists, RESOLVED.  Read at use, not at load."
  (let ((v (sb-ext:posix-getenv env)))
    (or (and v (plusp (length v)) (%pic-dir v))
        (and home (%pic-dir (merge-pathnames subdir (%ra-dir-pathname home))))
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
   (kind :initarg :kind :reader pic-entry-kind :documentation ":dir or :file"))
  (:documentation "A folder or a file in the open folder."))

(defun %pic-hidden-p (name) (or (zerop (length name)) (char= (char name 0) #\.)))

(defun folder-listing (root folder types)
  "(values DIRS FILES) in FOLDER, each sorted by name, case folded; FILES those whose extension is
   one of TYPES.  Only what resolves inside ROOT: a link that leads out is not there at all, rather
   than a row that refuses."
  (flet ((sorted (l) (sort l #'string< :key (lambda (e) (string-downcase (pic-entry-name e))))))
    (let ((dirs '()) (files '()))
      (dolist (d (or (ignore-errors (uiop:subdirectories folder)) '()))
        (let ((name (car (last (pathname-directory d))))
              (to (ignore-errors (truename d))))
          (when (and (stringp name) (not (%pic-hidden-p name)) to (%pic-within-p root to))
            (push (make-instance 'pic-entry :name name :path to :kind :dir) dirs))))
      (dolist (f (or (ignore-errors (uiop:directory-files folder)) '()))
        (let ((name (file-namestring f)) (type (pathname-type f)))
          (when (and type (member type types :test #'string-equal) (not (%pic-hidden-p name)))
            (let ((to (ignore-errors (truename f))))
              (when (and to (%pic-within-p root to))
                (push (make-instance 'pic-entry :name name :path to :kind :file) files))))))
      (values (sorted dirs) (sorted files)))))

(defun %pic-read (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence b s) b)))

;;; ---- pixels (the worker's side) ------------------------------------------------------------------------

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

;;; ---- the mailboxes and the worker ---------------------------------------------------------------------
;;; The mailboxes: a struct, as rfb.lisp's RFB-MAILBOX is, because that is what %SHARE is known to
;;; carry between modus threads; the list it holds is shared a cell at a time on the way in.

(defstruct (pic-box (:constructor %make-pic-box))
  (lock (sb-thread:make-mutex :name "glass-media-folder"))
  (cv (sb-thread:make-waitqueue :name "glass-media-folder"))
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

(defun %folder-worker (requests replies runner)
  "The worker thread: wait for a wish list, run its first job through RUNNER (a global function's
   name, called with the job and this thread's own cache), answer, look again.  A new list replaces
   the old; (:STOP) ends it, and with it the cache."
  (let ((jobs '()) (done '()) (cache (make-hash-table :test 'equal)))
    (handler-case
        ;; scribe's rasterizer scratch, gamma tables and caches are globals: a decoder that draws
        ;; text (folio's pages, an SVG's) while the window manager draws its own would corrupt both,
        ;; so the worker draws with a private set
        (scribe:with-thread-state ()
         (loop
          (let ((batch (%pic-take requests (if jobs nil 1))))
            (when (assoc :stop batch) (return-from %folder-worker nil))
            (let ((latest (find :jobs batch :key #'car :from-end t)))
              ;; what was just answered may be on a list posted before the answer was seen
              (when latest
                (setf jobs (remove-if (lambda (j) (member (second j) done :test #'equal)) (second latest))
                      done '()))))
          (let ((job (pop jobs)))
            (when job
              (%pic-post replies (funcall runner job cache))
              (push (second job) done)))))
      (serious-condition () nil))))

;;; ---- the panel ---------------------------------------------------------------------------------------

(defparameter *pic-cell-w* 120 "The grid's cells are about this wide (never fewer than two across).")
(defparameter *pic-name-h* 20 "The name line under a thumbnail.")
(defparameter *pic-bar-h* 56 "The viewer's Prev / Back / Next bar: bigger than a row, for a thumb.")
(defparameter *folder-relist-seconds* 5 "How often the open folder is looked at again.")

(defclass folder-panel ()
  ((fb :initarg :fb :reader panel-fb)
   (root :initarg :root :reader folder-root-of :reader pictures-root-of)
   (folder :initarg :folder :accessor folder-path :accessor pictures-folder)
   (dirs :initform '() :accessor folder-dirs :accessor pictures-dirs)
   (files :initform '() :accessor folder-files :accessor pictures-images)
   (listed-at :initform 0 :accessor folder-listed-at)
   (mode :initform :list :accessor folder-mode :accessor pictures-mode :documentation ":list or :view")
   (index :initform 0 :accessor folder-index :accessor pictures-index
          :documentation "The file open, in :view.")
   (page :initform 0 :accessor folder-page :accessor pictures-page :documentation "The list's page.")
   (targets :initform '() :accessor panel-targets)
   (dirty :initform t :accessor panel-dirty)
   ;; THE DETAIL (glass.wm's eleventh app value): the screen's density when the window manager has
   ;; asked for the open file drawn at it, else 1; and the last such drawing, kept until it changes
   (detail-scale :initform 1 :accessor folder-detail-scale)
   (detail-cache :initform nil :accessor folder-detail-cache)
   ;; what FOLDER-WISH last looked at (FOLDER-WISH-KEY): unchanged, and no reply in, it is not asked again
   (wish-key :initform nil :accessor folder-last-wish-key)
   (thumbs :initform (make-hash-table :test 'equal) :reader folder-thumbs :reader pictures-thumbs
           :documentation "Key -> what the cell shows.  The window's, for as long as it is open.")
   (views :initform (make-hash-table :test 'equal) :reader folder-views :reader pictures-views
          :documentation "Key -> what the opened item shows; trimmed to what is near.")
   (wanted :initform nil :accessor folder-wanted :documentation "The wish list last posted (its keys).")
   (requests :initform nil :accessor folder-requests :accessor pictures-requests)
   (replies :initform nil :accessor folder-replies :accessor pictures-replies)
   (thread :initform nil :accessor folder-thread :accessor pictures-thread)
   (threads :initarg :threads :initform t :accessor folder-threads-p :accessor pictures-threads-p)
   (coop-cache :initform (make-hash-table :test 'equal) :accessor folder-coop-cache
               :documentation "Cooperatively, the runner's cache (a thread has its own)."))
  (:documentation "A folder tab of one Media window: Pictures and Documents are its kinds."))

;;; what a kind says

(defgeneric folder-label (pn) (:documentation "The root's name in the head and the up row."))
(defgeneric folder-types (pn) (:documentation "The extensions listed."))
(defgeneric folder-runner (pn) (:documentation "The global function (a symbol) that makes a job."))
(defgeneric folder-thread-name (pn) (:method ((pn folder-panel)) "glass-media-folder"))
(defgeneric folder-wish (pn) (:documentation "The jobs wanted now, most wanted first."))
(defgeneric folder-store (pn reply) (:documentation "Keep an answer; T if it changes what is on show."))
(defgeneric folder-trim (pn) (:method ((pn folder-panel)) nil))

(defgeneric folder-wish-key (pn)
  (:documentation "Everything FOLDER-WISH and FOLDER-TRIM read but the worker's replies: while it is
   EQUAL and no reply has come in, neither is asked again.  They build every job's key, strings
   and all, and asked each frame they were most of the main thread's time.")
  (:method ((pn folder-panel))
    (list (folder-mode pn) (folder-index pn) (folder-page pn) (folder-files pn)
          (glass:fb-width (panel-fb pn)) (glass:fb-height (panel-fb pn))
          (folder-theater-p pn) (folder-detail-scale pn))))
(defgeneric folder-draw-cell (pn entry x y w h) (:documentation "A file's cell (and its target)."))
(defgeneric folder-draw-view (pn) (:documentation "The opened file, filling the panel."))
(defgeneric folder-list-info (pn ndirs nfiles) (:documentation "The head's right-hand count."))
(defgeneric folder-empty-hint (pn) (:documentation "The second line of an empty folder."))
(defgeneric folder-step (pn d) (:documentation "The next (D 1) or previous (D -1) in the viewer."))
(defgeneric folder-name-h (pn) (:method ((pn folder-panel)) *pic-name-h*))
(defgeneric folder-relist-seconds (pn) (:method ((pn folder-panel)) *folder-relist-seconds*))

(defgeneric folder-grid (pn)
  (:documentation "(values COLS ROWS CELL-W CELL-H TOP): the grid under the head (and the up row).")
  (:method ((pn folder-panel))
    (let* ((fb (panel-fb pn)) (w (glass:fb-width fb)) (h (glass:fb-height fb))
           (cols (max 2 (floor w *pic-cell-w*)))
           (cw (floor w cols))
           (top (%folder-top pn))
           ;; about square cells, as many rows as come nearest, stretched or squeezed to fill the
           ;; height -- a page with a band of nothing under it reads as a list that ran out
           (room (max 36 (- h top)))
           (rows (max 1 (min (floor room 36) (round room cw))))
           (ch (floor room rows)))
      (values cols rows cw ch top))))

(defgeneric folder-thumb-box (pn)
  (:documentation "(values W H) a thumbnail is fitted into.")
  (:method ((pn folder-panel))
    (multiple-value-bind (cols rows cw ch) (folder-grid pn)
      (declare (ignore cols rows))
      (values (max 1 (- cw 12)) (max 1 (- ch 12 (folder-name-h pn)))))))

(defgeneric folder-panel-key (pn keysym)
  (:documentation "Left / Right: the previous or next (item, or page of the list); PageUp / PageDown
   the list's pages; Escape or BackSpace: back to the list, or up a folder.  T if it was taken."))

;;; navigation

(defun folder-relist (pn)
  "Read the open folder again.  T if what it holds changed."
  (setf (folder-listed-at pn) (get-internal-real-time))
  (multiple-value-bind (dirs files) (folder-listing (folder-root-of pn) (folder-path pn) (folder-types pn))
    (flet ((names (l) (mapcar #'pic-entry-name l)))
      (unless (and (equal (names dirs) (names (folder-dirs pn)))
                   (equal (names files) (names (folder-files pn))))
        (let ((open (and (eq (folder-mode pn) :view) (nth (folder-index pn) (folder-files pn)))))
          (setf (folder-dirs pn) dirs (folder-files pn) files
                (folder-index pn) (or (and open (position (pic-entry-name open) files :key #'pic-entry-name
                                                          :test #'string=))
                                      (max 0 (min (folder-index pn) (1- (length files)))))))
        (when (and (eq (folder-mode pn) :view) (null files)) (setf (folder-mode pn) :list))
        t))))

(defun folder-open (pn dir)
  "Navigate to DIR -- only if it resolves inside the root; anything else leaves the folder where it
   is.  Returns the folder."
  (let ((to (%pic-dir dir)))
    (when (and to (%pic-within-p (folder-root-of pn) to))
      (setf (folder-path pn) to (folder-page pn) 0 (folder-mode pn) :list (folder-index pn) 0
            (folder-dirs pn) '() (folder-files pn) '() (panel-dirty pn) t)
      (folder-relist pn))
    (folder-path pn)))

(defun folder-up (pn)
  "The parent folder -- but not past the root, where up is where you already are."
  (let ((f (folder-path pn)))
    (if (equal (namestring f) (namestring (folder-root-of pn)))
        f
        (folder-open pn (make-pathname :directory (butlast (pathname-directory f)) :name nil :type nil
                                       :defaults f)))))

(defun %pic-at-root-p (pn) (equal (namestring (folder-path pn)) (namestring (folder-root-of pn))))

(defun %folder-top (pn) (+ *ra-head-h* (if (%pic-at-root-p pn) 0 *ra-row-h*)))

;;; layout

(defun %pic-grid (pn) (folder-grid pn))
(defun %pic-thumb-box (pn) (folder-thumb-box pn))

(defun %pic-items (pn) (append (folder-dirs pn) (folder-files pn)))

(defun %pic-per-page (pn) (multiple-value-bind (cols rows) (folder-grid pn) (* cols rows)))

(defun %pic-pages (pn) (max 1 (ceiling (length (%pic-items pn)) (%pic-per-page pn))))

(defun folder-theater-p (pn)
  "THEATER: a file open in a panel wider than it is tall (a turned phone).  The file is the whole
   panel -- no head, no bar -- a tap on its left or right half steps back or on (the Left and Right
   keys), and the window it is in may be shown full-screen (Media's theater)."
  (let ((fb (panel-fb pn)))
    (and (eq (folder-mode pn) :view) (folder-files pn)
         (> (glass:fb-width fb) (glass:fb-height fb)) t)))

(defun %pic-view-box (pn)
  "(values X Y W H) the viewer's area: between the head and the bar -- in THEATER, all of it."
  (let ((fb (panel-fb pn)))
    (if (folder-theater-p pn)
        (values 0 0 (glass:fb-width fb) (glass:fb-height fb))
        (values 0 *ra-head-h* (glass:fb-width fb) (max 1 (- (glass:fb-height fb) *ra-head-h* *pic-bar-h*))))))

(defun %pic-key (entry) (namestring (pic-entry-path entry)))

(defun %pic-page-images (pn page)
  "The files (not folders) on the list's PAGE."
  (let* ((items (%pic-items pn)) (per (%pic-per-page pn)) (n (length items)))
    (remove-if-not (lambda (e) (eq (pic-entry-kind e) :file))
                   (subseq items (min n (* page per)) (min n (* (1+ page) per))))))

;;; the worker: a wish list, posted when it changes

(defun %pic-start-thread (pn)
  "The worker thread, made the first time there is something to make.  NIL (and cooperative from
   now on) if this host cannot make one."
  (or (folder-thread pn)
      (let ((rq (glass::%share (%make-pic-box))) (rp (glass::%share (%make-pic-box))))
        (let ((th (ignore-errors (sb-thread:make-thread #'%folder-worker
                                                        :arguments (list rq rp (folder-runner pn))
                                                        :name (folder-thread-name pn)))))
          (if th
              (setf (folder-requests pn) rq (folder-replies pn) rp (folder-thread pn) th)
              (progn (setf (folder-threads-p pn) nil) nil))))))

(defun %pic-pump (pn)
  "Take the worker's answers and tell it what is wanted now.  T if something on show changed."
  (let ((changed nil) (got nil))
    (when (folder-replies pn)
      (dolist (r (%pic-take (folder-replies pn)))
        (setf got t)
        (when (folder-store pn r) (setf changed t))))
    (let ((key (folder-wish-key pn)))
      (when (and (folder-threads-p pn) (not got) (equal key (folder-last-wish-key pn)))
        (return-from %pic-pump changed))
      (setf (folder-last-wish-key pn) key))
    (folder-trim pn)
    (let ((wish (folder-wish pn)))
      (if (folder-threads-p pn)
          (unless (equal (mapcar #'second wish) (folder-wanted pn))
            (when (or wish (folder-thread pn))
              (when (%pic-start-thread pn)
                (setf (folder-wanted pn) (mapcar #'second wish))
                (%pic-post (folder-requests pn) (list :jobs wish)))))
          ;; cooperative: one job this turn
          (when wish
            (when (folder-store pn (funcall (folder-runner pn) (first wish) (folder-coop-cache pn)))
              (setf changed t)))))
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
  "VALUE is (W H PIXELS . more): centred in the box."
  (destructuring-bind (w h px &rest more) value
    (declare (ignore more))
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
  (let ((root (pathname-directory (folder-root-of pn))) (f (pathname-directory (folder-path pn))))
    (format nil "~{~a~^ / ~}" (cons (folder-label pn) (nthcdr (length root) f)))))

(defun %pic-head (pn title info &optional pager)
  "The head: TITLE at the left, INFO at the right, and the pager chevrons when PAGER.  None in THEATER."
  (when (folder-theater-p pn) (return-from %pic-head nil))
  (let* ((fb (panel-fb pn)) (w (glass:fb-width fb)) (right (- w 8)))
    (glass:fb-rect fb 0 0 w *ra-head-h* +ra-head+)
    (when pager
      (let ((page (folder-page pn)) (pages (%pic-pages pn)))
        (%pic-control pn (- w (* 2 *ra-control-w*)) 0 *ra-control-w* *ra-head-h* :left "Previous page"
                      (lambda () (setf (folder-page pn) (max 0 (1- page)))))
        (%pic-control pn (- w *ra-control-w*) 0 *ra-control-w* *ra-head-h* :right "Next page"
                      (lambda () (setf (folder-page pn) (min (1- pages) (1+ page)))))
        (decf right (+ 8 (* 2 *ra-control-w*)))))
    (let* ((iw (%ra-width info 10)))
      (%ra-text fb (- right iw) (%ra-baseline 0 *ra-head-h* 10) info :size 10 :color +ra-dim+)
      (%ra-text fb 8 (%ra-baseline 0 *ra-head-h* 12) (%ra-fit title (- right iw 24) 12) :size 12))))

(defun %pic-up-icon (fb cx cy color)
  (loop for i from 0 to 6
        do (glass:fb-rect fb (- cx i) (+ cy -4 i) (1+ (* 2 i)) 2 color)))

(defun %folder-draw-dir (pn e x y w h)
  "A folder's cell: the glyph and its name, the whole cell the target.  Laid out across a wide,
   short cell (a row), or down a squarer one (a grid cell)."
  (let ((fb (panel-fb pn)))
    (glass:fb-rect fb (+ x 2) (+ y 2) (- w 4) (- h 4) +ra-row+)
    (if (> w (* 2 h))
        (progn (%pic-folder-glyph fb (+ x 36) (+ y (floor h 2) 2) 36)
               (%ra-text fb 72 (%ra-baseline y h 13) (%ra-fit (pic-entry-name e) (- w 80) 13) :size 13))
        (multiple-value-bind (bw bh) (folder-thumb-box pn)
          (let ((bx (+ x 6)) (by (+ y 6)))
            (%pic-folder-glyph fb (+ bx (floor bw 2)) (+ by (floor bh 2) 2) (min 48 (floor bw 2)))
            (%pic-centred-text fb x w (- (+ y h) 10) (pic-entry-name e) 11 +ra-fg+))))
    (%pic-target pn x y w h (lambda () (folder-open pn (pic-entry-path e))) (pic-entry-name e))))

(defun %pic-draw-list (pn)
  (let* ((fb (panel-fb pn)) (w (glass:fb-width fb))
         (items (%pic-items pn)) (n (length items))
         (pages (%pic-pages pn))
         (page (setf (folder-page pn) (max 0 (min (folder-page pn) (1- pages)))))
         (nd (length (folder-dirs pn))) (nf (length (folder-files pn))))
    (%pic-head pn (%pic-title pn)
               (format nil "~a~@[  ~a~]" (folder-list-info pn nd nf)
                       (and (> pages 1) (format nil "~d/~d" (1+ page) pages)))
               (> pages 1))
    (unless (%pic-at-root-p pn)
      (let* ((y *ra-head-h*)
             (parent (butlast (pathname-directory (folder-path pn))))
             (pname (if (<= (length parent) (length (pathname-directory (folder-root-of pn))))
                        (folder-label pn) (car (last parent)))))
        (glass:fb-rect fb 0 y w *ra-row-h* +ra-row+)
        (glass:fb-rect fb 0 (+ y *ra-row-h* -1) w 1 +ra-bg+)
        (%pic-up-icon fb 20 (+ y (floor *ra-row-h* 2)) +ra-fg+)
        (%ra-text fb 40 (%ra-baseline y *ra-row-h* 12) (%ra-fit (format nil "Up to ~a" pname) (- w 50) 12)
                  :size 12)
        (%pic-target pn 0 y w *ra-row-h* (lambda () (folder-up pn)) "Up")))
    (multiple-value-bind (cols rows cw ch top) (folder-grid pn)
      (declare (ignore rows))
      (when (zerop n)
        (%pic-centred-text fb 0 w (+ top 40) (format nil "No ~(~a~) here" (folder-label pn)) 13 +ra-dim+)
        (%pic-centred-text fb 0 w (+ top 60) (format nil "~a in ~a" (folder-empty-hint pn) (namestring (folder-path pn)))
                           10 +ra-dim+))
      (loop for e in (subseq items (min n (* page (%pic-per-page pn))) (min n (* (1+ page) (%pic-per-page pn))))
            for i from 0
            do (let* ((x (* (mod i cols) cw)) (y (+ top (* (floor i cols) ch)))
                      ;; the last column takes what the division left over
                      (cwi (if (= (mod i cols) (1- cols)) (- w x) cw)))
                 (if (eq (pic-entry-kind e) :dir)
                     (%folder-draw-dir pn e x y cwi ch)
                     (folder-draw-cell pn e x y cwi ch)))))))

(defun %pic-back (pn)
  "Back to the list, on the page holding the file that was open."
  (let ((pos (position (nth (folder-index pn) (folder-files pn)) (%pic-items pn))))
    (setf (folder-mode pn) :list)
    (when pos (setf (folder-page pn) (floor pos (%pic-per-page pn))))))

(defun %folder-bar (pn &key (prev t) (next t))
  "The viewer's bar: three big buttons, the whole third each -- Prev, Back, Next.  PREV / NEXT NIL:
   that one is drawn dim (there is nothing that way) but still a target, which does nothing.  None
   in THEATER (the halves of the file are its Prev and Next)."
  (when (folder-theater-p pn) (return-from %folder-bar nil))
  (let* ((fb (panel-fb pn)) (w (glass:fb-width fb)) (h (glass:fb-height fb))
         (y (- h *pic-bar-h*)) (third (floor w 3)))
    (loop for (label icon action on) in (list (list "Prev" :left (lambda () (folder-step pn -1)) prev)
                                              (list "Back" nil (lambda () (%pic-back pn)) t)
                                              (list "Next" :right (lambda () (folder-step pn 1)) next))
          for x from 0 by third
          for bw = (if (string= label "Next") (- w (* 2 third)) third)
          do (let* ((tw (%ra-width label 13))
                    (gw (if icon (+ tw 22) tw))
                    (gx (+ x (floor (- bw gw) 2)))
                    (cy (+ y (floor *pic-bar-h* 2)))
                    (fg (if on +ra-fg+ #x4a525c)))
               (glass:fb-rect fb x y bw *pic-bar-h* +ra-control+)
               (glass:fb-rect fb x y 1 *pic-bar-h* +ra-bg+)
               (glass:fb-rect fb x y bw 1 +ra-bg+)
               (case icon
                 (:left (%ra-icon fb :left (+ gx 6) cy fg)
                  (%ra-text fb (+ gx 22) (%ra-baseline y *pic-bar-h* 13) label :size 13 :color fg))
                 (:right (%ra-text fb gx (%ra-baseline y *pic-bar-h* 13) label :size 13 :color fg)
                  (%ra-icon fb :right (+ gx tw 14) cy fg))
                 (t (%ra-text fb gx (%ra-baseline y *pic-bar-h* 13) label :size 13 :color fg)))
               (%pic-target pn x y bw *pic-bar-h* action label)))))

(defun folder-panel-redraw (pn)
  (let ((fb (panel-fb pn)))
    (setf (panel-targets pn) '())
    (glass:fb-rect fb 0 0 (glass:fb-width fb) (glass:fb-height fb) +ra-bg+)
    (if (and (eq (folder-mode pn) :view) (folder-files pn))
        (folder-draw-view pn)
        (progn (setf (folder-mode pn) :list) (%pic-draw-list pn)))
    t))

;;; input and the turn

(defun folder-panel-press (pn x y)
  "A press at X,Y in the panel's framebuffer: the last-drawn target under it.  T if one was hit."
  (let ((tg (find-if (lambda (r) (and (<= (rt-x r) x (+ (rt-x r) (rt-w r) -1))
                                      (<= (rt-y r) y (+ (rt-y r) (rt-h r) -1))))
                     (panel-targets pn))))
    (when tg
      (ignore-errors (funcall (rt-action tg)))
      (setf (panel-dirty pn) t)
      t)))

(defmethod folder-panel-key ((pn folder-panel) keysym)
  (let ((view (eq (folder-mode pn) :view)))
    (case keysym
      (#xff51 (if view (folder-step pn -1) (setf (folder-page pn) (max 0 (1- (folder-page pn))))))
      (#xff53 (if view (folder-step pn 1) (setf (folder-page pn) (min (1- (%pic-pages pn)) (1+ (folder-page pn))))))
      (#xff55 (unless view (setf (folder-page pn) (max 0 (1- (folder-page pn))))))
      (#xff56 (unless view (setf (folder-page pn) (min (1- (%pic-pages pn)) (1+ (folder-page pn))))))
      ((#xff1b #xff08) (if view (%pic-back pn) (folder-up pn)))
      (t (return-from folder-panel-key nil)))
    (setf (panel-dirty pn) t)
    t))

(defun folder-panel-dirty-p (pn)
  "Repaint iff something it shows changed: a tap, an answer arriving for what is on show, or
   (looked at every few seconds) the folder.  Never waits on the worker."
  (when (> (- (get-internal-real-time) (folder-listed-at pn))
           (* (folder-relist-seconds pn) internal-time-units-per-second))
    (when (folder-relist pn) (setf (panel-dirty pn) t)))
  ;; the wish list follows what the last tap changed, so make what is on show first
  (when (%pic-pump pn) (setf (panel-dirty pn) t))
  (when (panel-dirty pn)
    (setf (panel-dirty pn) nil)
    (folder-panel-redraw pn)))

(defun folder-panel-close (pn)
  "Stop the worker (its cache goes with it) and drop the cooperative cache."
  (when (folder-requests pn) (%pic-post (folder-requests pn) (list :stop)))
  (setf (folder-thread pn) nil)
  (clrhash (folder-coop-cache pn))
  (clrhash (folder-views pn)))

(defgeneric folder-panel-pointer (pn edge x y)
  (:documentation "The pointer at X,Y: EDGE :PRESS (the button went down), :DRAG (it moved while
   held) or :RELEASE.  By default a target acts on the press, not on every move while held.")
  (:method ((pn folder-panel) edge x y)
    (when (eq edge :press) (folder-panel-press pn x y))))

(defgeneric folder-detail (pn scale)
  (:documentation "The open file drawn at SCALE times the panel's pixels, for glass.wm's detail layer:
   (values X Y FB BOX) -- FB at X,Y confined to BOX (X Y W H), all in SCREEN pixels from the
   panel's corner -- or NIL (nothing sharper to show).")
  (:method ((pn folder-panel) scale) (declare (ignore scale)) nil))

(defun %folder-app (pn)
  "PN as an application: (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN RESIZE-FN TEXT-P
   SELECTION-FN DRAG-P FULLSCREEN-P) -- no text, no selection.  It draws from its framebuffer's size
   every time, so a resize is the framebuffer and a redraw.  In THEATER (FOLDER-THEATER-P) a press
   on the left half is Left, on the right half Right; no drag is its own; and it asks to be shown
   full-screen (FULLSCREEN-P, past the contract's nine, read by Media)."
  (let ((down nil))
    (values (lambda (pressed keysym) (when pressed (folder-panel-key pn keysym)))
            (lambda (mask x y)
              (let ((now (logbitp 0 mask)))
                (if (folder-theater-p pn)
                    (when (and now (not down))
                      (folder-panel-key pn (if (< x (floor (glass:fb-width (panel-fb pn)) 2)) #xff51 #xff53)))
                    (folder-panel-pointer pn (cond ((and now (not down)) :press) (now :drag) (down :release)
                                                   (t :move))
                                          x y))
                (setf down now)))
            (lambda () (folder-panel-dirty-p pn))
            nil
            (lambda () (folder-panel-close pn))
            (lambda (w h) (glass:fb-resize (panel-fb pn) w h +ra-bg+) (setf (panel-dirty pn) t))
            nil nil
            (lambda (x y) (declare (ignore x y)) (not (folder-theater-p pn)))
            (lambda () (folder-theater-p pn))
            (lambda (scale) (folder-detail pn scale)))))
