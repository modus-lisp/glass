;;;; media-documents.lisp -- the Documents tab of the Media window: a folder of PDFs, each a row with
;;;; its first page, its title and its page count, and one document read a page at a time with
;;;; Prev / Back / Next.  folio's first front end.
;;;;
;;;; Its own system (glass/media-documents), because the rendering is folio's (pure-CL PDF: parse,
;;;; decrypt where there is no password, render a page to pixels) and nothing else in glass depends on
;;;; folio.  MEDIA-APP finds DOCUMENTS-APP BY NAME (and only with FOLIO loaded), so a desk built without
;;;; this file has Media exactly as before.
;;;;
;;;; What every folder tab does -- the folder and never above its root, the paged list, the worker
;;;; thread fed a replacing wish list through mailboxes, the cooperative fallback, the viewer's bar,
;;;; DIRTY-P -- is media-folder.lisp's, and its header says how.  This file is what is Documents' own:
;;;;
;;;; THE FOLDER.  GLASS_DOCUMENTS, else ~/Documents, else Media's folder (*MEDIA-ROOT*); .pdf files.
;;;;
;;;; THE JOBS (%DOC-RUN-JOB, on the worker; a page can take seconds).  :THUMB -- parse the PDF, render
;;;; its first page into the row's box, and answer with its title (Info /Title, else none: the row then
;;;; shows the file name) and page count.  :LOW -- a page at about a third of its final resolution,
;;;; which is also how the page's printed area is found (below).  :PAGE -- the page at full resolution.
;;;; Opened, the wish list is: this page low, this page, the next page, the previous page, then the
;;;; list's thumbnails -- so the neighbours are rendered ahead and turning a page is instant.
;;;;
;;;; THE PARSED PDFs ARE THE WORKER'S.  The thread keeps the documents it has parsed (the last
;;;; *DOCUMENTS-KEEP-PARSED*) in a cache of its own, on its own thread, so a folio document never
;;;; crosses threads; only pixels (allocated shared), strings and numbers come back.  They go when the
;;;; window closes and its worker stops.  Rendered pages are kept for the open page and its two
;;;; neighbours only; thumbnails for as long as the window is open.
;;;;
;;;; THE FIT: THE PRINTED AREA, WHOLE.  A page is shown whole -- no scrolling inside a page, so Next
;;;; is always the next thing to read -- but with its blank margins trimmed: the low render finds the
;;;; inked area, which (padded a little, and never less than *DOCUMENT-MIN-CROP* of the page either
;;;; way, so a short last page or a centred title does not jump in size) is fitted into the tab, aspect
;;;; kept.  On a 400x700 phone in portrait a Letter page's area is 400x572 under the head and bar: the
;;;; whole page fits at 0.65 px/pt (10-pt text 6.5 px tall, hard to read); its printed area, typically
;;;; 1 inch in from each side, fits at about 0.85 px/pt -- a third bigger, the size that reads.  That
;;;; is also fit-width: on a portrait phone a page is limited by its width, so fitting the width loses
;;;; nothing of the page and fitting the whole page gains nothing.
;;;;
;;;; COOPERATIVELY (*DOCUMENTS-THREADS* NIL), one job per DIRTY-P turn on the caller's thread: a page
;;;; render is not resumable, so a turn that renders one STALLS FOR IT (a typical page half a second,
;;;; a heavy one several); a PDF over *DOCUMENTS-COOP-MAX-BYTES* is then not opened at all.
;;;;
;;;; A PDF folio cannot read (not a PDF, damaged beyond its scan fallback) or cannot decrypt (it needs
;;;; a password) is a row that says so and a reader that says so -- never an error.
;;;;
;;;; Prev / Next CLAMP: a document has a first and a last page; past them the button is drawn dim.

(in-package #:glass.desk)

(export '(documents-app documents-root documents-panel make-documents-panel documents-page-number
          documents-info documents-zoom documents-pan *document-zoom* *document-types* *documents-threads* *documents-coop-max-bytes*))

;;; ---- formats and limits ----------------------------------------------------------------------------

(defparameter *document-types* '("pdf") "What folio reads, by extension.")
(defparameter *document-max-bytes* (* 128 1024 1024) "A bigger file is not opened.")
(defparameter *documents-threads* t
  "Render on a thread of the tab's own (the default).  NIL: cooperatively -- see the header.")
(defparameter *documents-coop-max-bytes* (* 1024 1024)
  "Cooperatively, a PDF bigger than this is not opened: a turn must not stall on it.")
(defparameter *documents-relist-seconds* 5 "How often the open folder is looked at again.")
(defparameter *documents-keep-parsed* 4 "Parsed PDFs a worker keeps (the most recently used).")
(defparameter *document-max-scale* 3 "A page is never shown at more than this many px per pt.")

(defparameter *document-strip-pixels* 400000
  "A :SHARP page (glass.wm's detail layer: the screen's full density) is rendered in strips of
   about this many pixels (folio's :WINDOW), each packed into the page and dropped.  A phone's
   document worker has 64 MB and keeps the parsed PDF live (a 10 MB deck: its bytes and streams);
   gesso's working arrays are the canvas's size, doubles.  Measured on modus: a whole page of
   800K pixels thrashed the collector for minutes, and stalled the main thread with it.")
(defparameter *document-low-fraction* 0.35 "The low render's resolution, of the full one's.")
(defparameter *document-min-crop* 0.66
  "The trimmed area is at least this fraction of the page, across and down.")
(defparameter *document-row-h* 84 "A document's row: its first page beside its title.")

(defvar *documents-folio-lock* (sb-thread:make-mutex :name "glass-folio")
  "folio keeps a few process-wide caches (CMaps, fonts): two windows' workers take turns at it.")

(defun documents-root (&key (home (user-homedir-pathname)))
  "GLASS_DOCUMENTS, else HOME's Documents, else *MEDIA-ROOT*, else the current directory: the first
   of them that exists, RESOLVED.  Read at use, not at load."
  (%folder-root "GLASS_DOCUMENTS" "Documents/" home))

;;; ---- the worker's side: no window, no panel, only the job and the worker's cache -------------------

(defun %doc-text (bytes)
  "A PDF text string's BYTES: UTF-16BE behind its FE FF mark, else PDFDocEncoding read as Latin-1.
   Control characters dropped, blanks trimmed; NIL when nothing is left."
  (let ((s (if (and (>= (length bytes) 2) (= (aref bytes 0) #xfe) (= (aref bytes 1) #xff))
               (with-output-to-string (o)
                 (loop for i from 2 below (1- (length bytes)) by 2
                       do (write-char (code-char (logior (ash (aref bytes i) 8) (aref bytes (1+ i)))) o)))
               (map 'string #'code-char bytes))))
    (let ((clean (string-trim " " (remove-if (lambda (c) (< (char-code c) 32)) s))))
      (and (plusp (length clean)) clean))))

(defun %doc-title (doc)
  "The PDF's Info /Title, or NIL.  Never signals."
  (ignore-errors
    (let* ((tr (folio:document-trailer doc))
           (info (and tr (folio:resolve doc (folio:dict-get tr "Info")))))
      (when (folio:pdf-dict-p info)
        (let ((title (folio:resolve doc (folio:dict-get info "Title"))))
          (and (folio:pdf-string-p title) (%doc-text (folio:pdf-string-bytes title))))))))

(defun %doc-open (cache path max-bytes)
  "PATH's parsed document from the worker's CACHE, parsing it (again, if the file changed) when it is
   not there.  (values DOC TITLE PAGES) or (values NIL REASON) -- :too-big, :broken (folio cannot read
   it, or finds no pages in it), :locked (encrypted, and folio cannot decrypt it without a password)."
  (let* ((date (ignore-errors (file-write-date path)))
         (hit (gethash path cache)))
    (if (and hit (eql (first hit) date))
        (values-list (rest hit))
        (let ((entry
                (handler-case
                    (let ((size (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))))
                      (if (> size max-bytes)
                          (list nil :too-big)
                          (let ((doc (sb-thread:with-mutex (*documents-folio-lock*)
                                       (folio:parse-pdf (%pic-read path)))))
                            (cond ((eq (folio:encryption-status doc) :unsupported) (list nil :locked))
                                  ;; no pages: what folio's scan fallback makes of a file that is not a PDF
                                  ((not (plusp (folio:page-count doc))) (list nil :broken))
                                  (t (list doc (%doc-title doc) (folio:page-count doc)))))))
                  (serious-condition () (list nil :broken)))))
          ;; keep the last few parsed; a refusal is small, keep that too
          (setf (gethash path cache) (cons date entry))
          (when (first entry)
            (let ((order (cons path (remove path (gethash :order cache) :test #'equal))))
              (dolist (old (nthcdr *documents-keep-parsed* order)) (remhash old cache))
              (setf (gethash :order cache) (subseq order 0 (min (length order) *documents-keep-parsed*)))))
          (values-list entry)))))

(defun %doc-shown-size (doc n)
  "Page N's (values W H) in points as it is shown: its crop box (else media box), turned by /Rotate."
  (let* ((page (folio:get-page doc n))
         (cb (folio:page-cropbox page))
         (box (if (and (consp cb) (= (length cb) 4)) cb (folio:page-mediabox page)))
         (w (max 1 (abs (- (third box) (first box))))) (h (max 1 (abs (- (fourth box) (second box))))))
    (if (= 90 (mod (or (folio:page-rotate page) 0) 180)) (values h w) (values w h))))

(defun %doc-render (doc n scale)
  "Page N at SCALE px per pt, as a pigment image (straight-alpha RGBA)."
  (sb-thread:with-mutex (*documents-folio-lock*)
    (folio:render-page doc n :scale scale)))

(defun %doc-inked (img)
  "The fractions (FX0 FY0 FX1 FY1) of IMG holding anything that is not near white, or NIL."
  (let* ((w (pigment:img-w img)) (h (pigment:img-h img)) (px (pigment:img-rgba img))
         (x0 w) (y0 h) (x1 -1) (y1 -1))
    (declare (type (simple-array (unsigned-byte 8) (*)) px) (type fixnum w h x0 y0 x1 y1))
    (dotimes (y h)
      (dotimes (x w)
        (let ((i (* 4 (+ x (* y w)))))
          (when (and (> (aref px (+ i 3)) 16)
                     (or (< (aref px i) 232) (< (aref px (+ i 1)) 232) (< (aref px (+ i 2)) 232)))
            (setf x0 (min x0 x) x1 (max x1 x) y0 (min y0 y) y1 (max y1 y))))))
    (and (>= x1 x0) (>= y1 y0)
         (list (/ x0 w) (/ y0 h) (/ (1+ x1) w) (/ (1+ y1) h)))))

(defun %doc-crop (inked)
  "The page fractions (FX0 FY0 FX1 FY1) shown: the INKED area padded by 2% of the page, at least
   *DOCUMENT-MIN-CROP* of it each way, inside the page.  The whole page when nothing is inked."
  (flet ((span (lo hi)
           (let* ((lo (max 0 (- lo 0.02))) (hi (min 1 (+ hi 0.02))) (min *document-min-crop*))
             (when (< (- hi lo) min)
               (let ((c (/ (+ lo hi) 2)))
                 (setf lo (- c (/ min 2)) hi (+ c (/ min 2)))
                 (when (< lo 0) (setf hi (- hi lo) lo 0))
                 (when (> hi 1) (setf lo (- lo (- hi 1)) hi 1))))
             (values (float lo) (float hi)))))
    (if inked
        (destructuring-bind (x0 y0 x1 y1) inked
          (multiple-value-bind (a b) (span x0 x1)
            (multiple-value-bind (c d) (span y0 y1)
              (list a c b d))))
        (list 0.0 0.0 1.0 1.0))))

(defun %doc-geometry (pw ph crop box-w box-h)
  "(values SCALE DW DH): the CROP of a PWxPH-point page fitted into BOX-WxBOX-H, aspect kept."
  (destructuring-bind (fx0 fy0 fx1 fy1) crop
    (let* ((cw (* pw (- fx1 fx0))) (ch (* ph (- fy1 fy0)))
           (s (min (/ box-w cw) (/ box-h ch) *document-max-scale*)))
      (values s (max 1 (min box-w (round (* cw s)))) (max 1 (min box-h (round (* ch s))))))))

(defun %doc-cut (img crop)
  "(values RGBA W H): the CROP fractions of IMG, copied out."
  (let* ((w (pigment:img-w img)) (h (pigment:img-h img)) (px (pigment:img-rgba img)))
    (destructuring-bind (fx0 fy0 fx1 fy1) crop
      (let* ((x0 (max 0 (min (1- w) (floor (* fx0 w))))) (y0 (max 0 (min (1- h) (floor (* fy0 h)))))
             (x1 (max (1+ x0) (min w (ceiling (* fx1 w))))) (y1 (max (1+ y0) (min h (ceiling (* fy1 h)))))
             (cw (- x1 x0)) (ch (- y1 y0))
             (out (make-array (* 4 cw ch) :element-type '(unsigned-byte 8))))
        (dotimes (y ch)
          (replace out px :start1 (* 4 y cw) :start2 (* 4 (+ x0 (* (+ y y0) w))) :end2 (* 4 (+ x1 (* (+ y y0) w)))))
        (values out cw ch)))))

(defun %doc-sharp (doc n crop box-w box-h file)
  "Render the CROP of page N fitted into BOX-WxBOX-H at full size into FILE (the page cache's
   format: W and H, then X8R8G8B8 big-endian), a strip at a time (*DOCUMENT-STRIP-PIXELS*), each
   strip composited on the page's background, packed and written, then dropped.  (values W H).

   NO PAGE OF PIXELS IN MEMORY, AND NONE SHARED.  A worker's MODUS:MAKE-SHARED-ARRAY comes out of
   the runtime's lock arena (64 MB, 8 bytes reserved an element whatever the type): three or four
   landscape pages of it and the app stopped -- \"the lock arena is exhausted\".  The file is the
   result; the main thread reads it into its own heap (FOLDER-STORE)."
  (multiple-value-bind (pw ph) (%doc-shown-size doc n)
    (let ((s (%doc-geometry pw ph crop box-w box-h)))
      (multiple-value-bind (iw ih) (sb-thread:with-mutex (*documents-folio-lock*) (folio:page-pixel-size doc n :scale s))
        (destructuring-bind (fx0 fy0 fx1 fy1) crop
          (let* ((x0 (max 0 (min (1- iw) (floor (* fx0 iw))))) (y0 (max 0 (min (1- ih) (floor (* fy0 ih)))))
                 (w (max 1 (min (round box-w) (- (min iw (ceiling (* fx1 iw))) x0))))
                 (h (max 1 (min (round box-h) (- (min ih (ceiling (* fy1 ih))) y0))))
                 (band (max 8 (floor *document-strip-pixels* w)))
                 (tmp (concatenate 'string (namestring file) ".part"))
                 (br (ldb (byte 8 16) +ra-bg+)) (bg (ldb (byte 8 8) +ra-bg+)) (bb (ldb (byte 8 0) +ra-bg+)))
            (declare (type fixnum w h band br bg bb))
            (with-open-file (out tmp :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
              (write-sequence (%doc-cache-header w h) out)
              ;; each image decoded once for the page, and its fonts loaded once, not once a strip
              (let ((folio:*image-memo* (make-hash-table :test 'eq))
                    (folio:*font-memo* (make-hash-table :test 'eq)))
                (loop for sy of-type fixnum from 0 below h by band
                      do (let* ((sh (min band (- h sy)))
                                (img (sb-thread:with-mutex (*documents-folio-lock*)
                                       (folio:render-page doc n :scale s :window (list x0 (+ y0 sy) w sh))))
                                (px (pigment:img-rgba img))
                                (b (make-array (* 4 w sh) :element-type '(unsigned-byte 8))))
                           (declare (type (simple-array (unsigned-byte 8) (*)) px b) (type fixnum sh))
                           (dotimes (i (* w sh))
                             (let* ((j (* 4 i)) (a (aref px (+ j 3))) (na (- 255 a)))
                               (declare (type fixnum j a na))
                               (setf (aref b j) 0
                                     (aref b (+ j 1)) (floor (+ (* (aref px j) a) (* br na)) 255)
                                     (aref b (+ j 2)) (floor (+ (* (aref px (+ j 1)) a) (* bg na)) 255)
                                     (aref b (+ j 3)) (floor (+ (* (aref px (+ j 2)) a) (* bb na)) 255))))
                           (write-sequence b out)))))
            (rename-file tmp file)
            (%doc-cache-trim)
            (values w h)))))))

(defun %doc-low (doc n box-w box-h)
  "Page N rendered small: (values IMG CROP)."
  (multiple-value-bind (pw ph) (%doc-shown-size doc n)
    (let* ((fit (min (/ box-w pw) (/ box-h ph) *document-max-scale*))
           (img (%doc-render doc n (max 0.05 (* fit *document-low-fraction*)))))
      (values img (%doc-crop (%doc-inked img))))))

;;; ---- rendered pages on disk ------------------------------------------------------------------------
;;;
;;; A page is rendered once.  The worker keeps each :PAGE and :SHARP render it makes in the cache
;;; directory, named by the file (its path and date), the page, the kind and the box it was fitted
;;; to, and reads it back instead of rendering again: turning back to a page, reopening a deck, a
;;; relaunch.  Raw X8R8G8B8 behind its width and height -- nothing to decode.  Any failure here is
;;; only a miss.

(defparameter *document-cache-files* 200 "Rendered pages kept on disk; the oldest go first.")

(defun %doc-cache-dir ()
  "GLASS_PAGE_CACHE, else HOME's Library/Caches/glass-pages/ (an iOS app's own caches), made if need be."
  (ignore-errors
    (let* ((env (ignore-errors (sb-ext:posix-getenv "GLASS_PAGE_CACHE")))
           (d (if (and env (plusp (length env)))
                  (pathname (if (char= (char env (1- (length env))) #\/) env (concatenate 'string env "/")))
                  (merge-pathnames "Library/Caches/glass-pages/" (user-homedir-pathname)))))
      (ensure-directories-exist d)
      d)))

(defun %doc-cache-file (path page kind box-w box-h)
  (let ((dir (%doc-cache-dir)) (date (ignore-errors (file-write-date path))))
    (when (and dir date)
      (merge-pathnames (format nil "~36r-~d-~(~a~)-~dx~d-v2.px"
                               (sxhash (format nil "~a|~d" (namestring path) date)) page kind
                               (round box-w) (round box-h))
                       dir))))

(defun %doc-cache-header (w h)
  (let ((b (make-array 8 :element-type '(unsigned-byte 8))))
    (loop for v in (list w h) for o from 0 by 4
          do (setf (aref b o) (ldb (byte 8 24) v) (aref b (+ o 1)) (ldb (byte 8 16) v)
                   (aref b (+ o 2)) (ldb (byte 8 8) v) (aref b (+ o 3)) (ldb (byte 8 0) v)))
    b))

(defun %doc-cache-size (file)
  "(values W H) of a whole cached page in FILE, or NIL -- without reading its pixels."
  (ignore-errors
    (when (and file (probe-file file))
      (with-open-file (in file :element-type '(unsigned-byte 8))
        (let ((hd (make-array 8 :element-type '(unsigned-byte 8))))
          (when (= 8 (read-sequence hd in))
            (let ((w (logior (ash (aref hd 0) 24) (ash (aref hd 1) 16) (ash (aref hd 2) 8) (aref hd 3)))
                  (h (logior (ash (aref hd 4) 24) (ash (aref hd 5) 16) (ash (aref hd 6) 8) (aref hd 7))))
              (when (and (plusp (* w h)) (= (file-length in) (+ 8 (* 4 w h))))
                (values w h)))))))))

(defun %doc-cache-trim ()
  "The oldest cached pages past *DOCUMENT-CACHE-FILES* out."
  (ignore-errors
    (let ((all (directory (merge-pathnames "*.px" (%doc-cache-dir)))))
      (when (> (length all) *document-cache-files*)
        (dolist (old (nthcdr *document-cache-files*
                             (sort all #'> :key (lambda (f) (or (ignore-errors (file-write-date f)) 0)))))
          (ignore-errors (delete-file old)))))))

(defvar *doc-px-pool* nil
  "Page pixel arrays the main thread has finished with (FOLDER-TRIM), for %DOC-CACHE-READ to fill
   again.  MAIN THREAD ONLY.  A sharp landscape page is 10 MB, and a fresh one a page turn lived
   long enough for modus's region-0 collector to tenure it: old space filled with dead pages, every
   collection copied more, and after a dozen pages the heap was exhausted.  A few arrays, reused,
   are tenured once.")

(defparameter *doc-px-pool-size* 12)

(defparameter *doc-band-pixels* 250000
  "The most pixels in one array a cached page is read into on the main thread; a bigger page is
   read as bands of whole rows (a list of arrays, top first).  modus main 246562e3 exhausts
   region 0 when the main thread allocates large arrays while a worker thread is alive: 4 MB
   arrays with the worker blocked in CONDITION-WAIT (the document worker, idle), 9 MB with one
   sleeping; 1 MB survives either.  A sharp landscape page is 9.8 MB.  Reported to the modus
   session; this is the workaround.")

(defun %doc-px-get (n)
  (let ((px (find n *doc-px-pool* :key #'length)))
    (if px
        (progn (setf *doc-px-pool* (delete px *doc-px-pool* :count 1)) px)
        (make-array n :element-type '(unsigned-byte 32)))))

(defun %doc-px-put (px)
  (when (and (typep px '(simple-array (unsigned-byte 32) (*))) (> (length px) 65536)
             (not (member px *doc-px-pool*)))
    (push px *doc-px-pool*)
    (when (> (length *doc-px-pool*) *doc-px-pool-size*)
      (setf *doc-px-pool* (subseq *doc-px-pool* 0 *doc-px-pool-size*)))))

(defun %doc-cache-read (file)
  "(values W H PIXELS) from FILE, or NIL.  ON THE MAIN THREAD (FOLDER-STORE): PIXELS come from
   *DOC-PX-POOL* when it has one the size, and the file is read a slice at a time -- no page-sized
   array is made that need not be.  PIXELS is one array, or past *DOC-BAND-PIXELS* a list of
   bands, each whole rows."
  (ignore-errors
    (when (and file (probe-file file))
      (with-open-file (in file :element-type '(unsigned-byte 8))
        (let ((hd (make-array 8 :element-type '(unsigned-byte 8))))
          (when (= 8 (read-sequence hd in))
            (let* ((w (logior (ash (aref hd 0) 24) (ash (aref hd 1) 16) (ash (aref hd 2) 8) (aref hd 3)))
                   (h (logior (ash (aref hd 4) 24) (ash (aref hd 5) 16) (ash (aref hd 6) 8) (aref hd 7)))
                   (n (* w h)))
              (when (and (plusp n) (= (file-length in) (+ 8 (* 4 n))))
                (let* ((b (make-array 262144 :element-type '(unsigned-byte 8)))
                       (rows (if (<= n *doc-band-pixels*) h (max 1 (floor *doc-band-pixels* w))))
                       (bands (loop for y from 0 below h by rows
                                    collect (%doc-px-get (* w (min rows (- h y)))))))
                  (declare (type (simple-array (unsigned-byte 8) (*)) b))
                  (dolist (px bands)
                    (let ((m (length px)) (i 0))
                      (declare (type (simple-array (unsigned-byte 32) (*)) px) (fixnum m i))
                      (loop while (< i m)
                            do (let ((got (floor (read-sequence b in :end (* 4 (min 65536 (- m i)))) 4)))
                                 (declare (fixnum got))
                                 (when (zerop got) (return-from %doc-cache-read nil))
                                 (dotimes (k got)
                                   (let ((j (* 4 k)))
                                     (setf (aref px (+ i k)) (logior (ash (aref b j) 24) (ash (aref b (+ j 1)) 16)
                                                                     (ash (aref b (+ j 2)) 8) (aref b (+ j 3))))))
                                 (incf i got)))))
                  (values w h (if (cdr bands) bands (car bands))))))))))))

(defun %doc-cache-write (file w h px)
  "PX (W x H) to FILE, then the oldest past *DOCUMENT-CACHE-FILES* out."
  (declare (type (simple-array (unsigned-byte 32) (*)) px))
  (ignore-errors
    (when file
      (let* ((n (* w h)) (b (make-array (+ 8 (* 4 n)) :element-type '(unsigned-byte 8))))
        (declare (type (simple-array (unsigned-byte 8) (*)) b))
        (loop for v in (list w h) for o from 0 by 4
              do (setf (aref b o) (ldb (byte 8 24) v) (aref b (+ o 1)) (ldb (byte 8 16) v)
                       (aref b (+ o 2)) (ldb (byte 8 8) v) (aref b (+ o 3)) (ldb (byte 8 0) v)))
        (dotimes (i n)
          (let ((v (aref px i)) (j (+ 8 (* 4 i))))
            (setf (aref b j) (ldb (byte 8 24) v) (aref b (+ j 1)) (ldb (byte 8 16) v)
                  (aref b (+ j 2)) (ldb (byte 8 8) v) (aref b (+ j 3)) (ldb (byte 8 0) v))))
        (let ((tmp (concatenate 'string (namestring file) ".part")))
          (with-open-file (out tmp :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
            (write-sequence b out))
          (rename-file tmp file)))
      (%doc-cache-trim)
      t)))

(defun %doc-run-job (job cache)
  "JOB is (KIND KEY PATH PAGE BOX-W BOX-H MAX-BYTES).  The reply: (KIND KEY W H PIXELS TITLE PAGES),
   or (KIND KEY NIL REASON TITLE PAGES).  Never signals.  CACHE is the worker's: parsed documents,
   and each page's crop (so its full render need not find it again)."
  (destructuring-bind (kind key path page box-w box-h max-bytes) job
    (multiple-value-bind (doc title pages) (%doc-open cache path max-bytes)
      (if (null doc)
          (list kind key nil title nil nil)       ; TITLE is the reason here
          (handler-case
              (ecase kind
                (:thumb
                 (multiple-value-bind (pw ph) (%doc-shown-size doc 0)
                   (let ((img (%doc-render doc 0 (min (/ box-w pw) (/ box-h ph) *document-max-scale*))))
                     (multiple-value-bind (w h px)
                         (%pic-scale (pigment:img-rgba img) (pigment:img-w img) (pigment:img-h img)
                                     box-w box-h +ra-row+ 1)
                       (list kind key w h px title pages)))))
                ((:low :page :sharp)
                 (let* ((ckey (format nil "crop|~a#~d" path page))
                        (file (%doc-cache-file path page kind box-w box-h)))
                  ;; A RENDER IS A FILE: the worker replies with the page cache's file, and the main
                  ;; thread reads it (FOLDER-STORE) -- a page of pixels made by a worker for the main
                  ;; thread would come out of the runtime's lock arena (see %DOC-SHARP)
                  (multiple-value-bind (cw ch) (%doc-cache-size file)
                   (if cw
                    (list kind key cw ch (namestring file) title pages)
                   (multiple-value-bind (pw ph) (%doc-shown-size doc page)
                     (multiple-value-bind (low crop)
                         (let ((c (gethash ckey cache)))
                           (if (and c (member kind '(:page :sharp))) (values nil c) (%doc-low doc page box-w box-h)))
                       (setf (gethash ckey cache) crop)
                       (if (eq kind :sharp)
                        (if file
                            (multiple-value-bind (w h) (%doc-sharp doc page crop box-w box-h file)
                              (list kind key w h (namestring file) title pages))
                            (list kind key nil :page-fail title pages))
                       (multiple-value-bind (s dw dh) (%doc-geometry pw ph crop box-w box-h)
                         (let ((img (if (eq kind :low) low (%doc-render doc page s))))
                           (multiple-value-bind (rgba iw ih) (%doc-cut img crop)
                             (multiple-value-bind (w h px) (%pic-scale rgba iw ih dw dh +ra-bg+ 100 (null file))
                               (if (and file (%doc-cache-write file w h px))
                                   (list kind key w h (namestring file) title pages)
                                   (list kind key w h px title pages))))))))))))))
            (serious-condition () (list kind key nil :page-fail title pages)))))))

;;; ---- the panel ---------------------------------------------------------------------------------------

(defclass documents-panel (folder-panel)
  ((page-number :initform 0 :accessor documents-page-number :documentation "The page open, from 0.")
   (info :initform (make-hash-table :test 'equal) :reader documents-info
         :documentation "Resolved namestring -> (TITLE . PAGES), as the worker found them.")
   (zoom :initform 1 :accessor documents-zoom
         :documentation "1: the printed area whole.  *DOCUMENT-ZOOM*: that, so many times bigger, panned.")
   (pan :initform (cons 0 0) :accessor documents-pan
        :documentation "Zoomed, the view's top left as fractions (FX . FY) of the page shown.")
   (drag :initform nil :accessor documents-drag
         :documentation "A press on the page: (X Y PAN MOVED), until it is released."))
  (:documentation "The Documents tab of one Media window."))

(defun make-documents-panel (fb &key (root (documents-root)) (threads *documents-threads*))
  (let ((pn (make-instance 'documents-panel :fb fb :root root :folder root :threads threads)))
    (folder-relist pn)
    pn))

(defmethod folder-label ((pn documents-panel)) "Documents")
(defmethod folder-types ((pn documents-panel)) *document-types*)
(defmethod folder-runner ((pn documents-panel)) '%doc-run-job)
(defmethod folder-thread-name ((pn documents-panel)) "glass-documents")
(defmethod folder-relist-seconds ((pn documents-panel)) *documents-relist-seconds*)
(defmethod folder-empty-hint ((pn documents-panel)) "PDF files")
(defmethod folder-list-info ((pn documents-panel) nd nf)
  (format nil "~[~:;~:*~d folder~:p  ~]~d document~:p" nd nf))

(defmethod folder-grid ((pn documents-panel))
  "Rows, one document each (two across a wide window): a title needs the width a grid cell lacks."
  (let* ((fb (panel-fb pn)) (w (glass:fb-width fb)) (h (glass:fb-height fb))
         (cols (if (>= w 640) 2 1)) (cw (floor w cols))
         (top (%folder-top pn))
         (room (max 36 (- h top)))
         (rows (max 1 (floor room *document-row-h*)))
         (ch (floor room rows)))
    (values cols rows cw ch top)))

(defmethod folder-thumb-box ((pn documents-panel))
  (multiple-value-bind (cols rows cw ch) (folder-grid pn)
    (declare (ignore cols rows cw))
    (let ((bh (max 1 (- ch 12)))) (values (round (* bh 4) 5) bh))))

(defun %doc-key (kind path page &optional (zoom 1))
  "A rendered page's key: its kind, its zoom (when not 1), the file and the page."
  (format nil "~(~a~)~:[@~d~;~*~]|~a#~d" kind (eql zoom 1) zoom path page))

(defun %dk (pn kind path page)
  "PN's key for a render of PAGE: the page's key, and the size of the box it is fitted into -- a
   panel that changes size (a turned phone, Media's theater) asks for renders at its new size, and
   one made for the old size, even one arriving late from the worker, is never shown in its place."
  (multiple-value-bind (w h) (%doc-box pn)
    (format nil "~a/~dx~d" (%doc-key kind path page (documents-zoom pn)) (round w) (round h))))

(defun %doc-box (pn)
  "(values W H) a page is fitted into: the view, times the zoom."
  (multiple-value-bind (x y w h) (%pic-view-box pn)
    (declare (ignore x y))
    (values (* w (documents-zoom pn)) (* h (documents-zoom pn)))))

(defun %doc-open-entry (pn) (and (eq (folder-mode pn) :view) (nth (folder-index pn) (folder-files pn))))

(defun %doc-pages (pn e) (cdr (gethash (%pic-key e) (documents-info pn))))
(defun %doc-shown-title (pn e) (or (car (gethash (%pic-key e) (documents-info pn))) (pic-entry-name e)))

(defun %doc-refusal (pn e)
  "Why E cannot be read, if the worker has said: a keyword, or NIL."
  (let ((th (gethash (%pic-key e) (folder-thumbs pn))))
    (and (keywordp th) (not (eq th :page-fail)) th)))

(defmethod folder-wish-key ((pn documents-panel))
  (list* (documents-page-number pn) (documents-zoom pn) (call-next-method)))

(defmethod folder-wish ((pn documents-panel))
  "Opened: this page small, this page, the next, the previous; then the list's thumbnails, this
   page's and the next's."
  (let* ((jobs '())
         (max-bytes (if (folder-threads-p pn) *document-max-bytes* *documents-coop-max-bytes*))
         (e (%doc-open-entry pn)))
    (flet ((want (kind e page bw bh table)
             (let ((k (if (eq kind :thumb) (%pic-key e) (%dk pn kind (%pic-key e) page))))
               (unless (or (gethash k table) (find k jobs :key #'second :test #'equal))
                 (push (list kind k (pic-entry-path e) page bw bh max-bytes) jobs)))))
      (when (and e (not (%doc-refusal pn e)))
        (multiple-value-bind (w h) (%doc-box pn)
          (let* ((p (documents-page-number pn)) (n (%doc-pages pn e))
                 (views (folder-views pn)))
            (unless (gethash (%dk pn :page (%pic-key e) p) views)
              (want :low e p w h views))
            (want :page e p w h views)
            ;; SHARP: once the window manager has asked for the page at the screen's density
            ;; (FOLDER-DETAIL), the page at that many times the box, for the detail layer
            (let ((ds (folder-detail-scale pn)))
              (when (> ds 1) (want :sharp e p (* w ds) (* h ds) views)))
            (when (or (null n) (< (1+ p) n)) (want :page e (1+ p) w h views))
            ;; ... and the next page's, ahead of the turn (kept on disk, so it is done once)
            (let ((ds (folder-detail-scale pn)))
              (when (and (> ds 1) (or (null n) (< (1+ p) n))) (want :sharp e (1+ p) (* w ds) (* h ds) views)))
            (when (plusp p) (want :page e (1- p) w h views)))))
      (multiple-value-bind (bw bh) (folder-thumb-box pn)
        (dolist (page (list (folder-page pn) (1+ (folder-page pn))))
          (dolist (f (%pic-page-images pn page))
            (want :thumb f 0 bw bh (folder-thumbs pn))))))
    (nreverse jobs)))

(defmethod folder-store ((pn documents-panel) reply)
  (destructuring-bind (kind key w &rest more) reply
    (let* ((ok (and w (or (not (stringp (second more)))
                          ;; the render is in the page cache: read it here, into the main thread's heap
                          (multiple-value-bind (fw fh px) (%doc-cache-read (second more))
                            (if px
                                (setf w fw more (list* fh px (cddr more)))
                                ;; gone (trimmed) or unreadable: a failed page, as the worker would say
                                (progn (setf w nil more (list :page-fail (third more) (fourth more)))
                                       nil))))))
           (value (if ok (list w (first more) (second more)) (first more)))
           (title (if ok (third more) (second more)))
           (pages (if ok (fourth more) (third more)))
           (path (if (eq kind :thumb) key (subseq key (1+ (position #\| key)) (position #\# key :from-end t))))
           (open (%doc-open-entry pn))
           (open-p (and open (equal path (%pic-key open)))))
      (when pages (setf (gethash path (documents-info pn)) (cons title pages)))
      (cond ((eq kind :thumb)
             (setf (gethash key (folder-thumbs pn)) value)
             (or open-p
                 (and (eq (folder-mode pn) :list)
                      (find key (%pic-page-images pn (folder-page pn)) :key #'%pic-key :test #'equal)
                      t)))
            (t
             ;; a refusal of the whole document (not one page's) is the document's
             (when (and (not ok) (not (eq value :page-fail)))
               (setf (gethash path (folder-thumbs pn)) value))
             (setf (gethash key (folder-views pn)) value)
             (and open-p t))))))

(defmethod folder-trim ((pn documents-panel))
  "Rendered pages: the open page and its two neighbours, at the zoom shown (and the open page's small
   one until it is rendered), and the open page's and the next one's :SHARP (FOLDER-DETAIL) -- kept
   here or FOLDER-WISH asks for them again, forever; nothing when no document is open."
  (let* ((e (%doc-open-entry pn)) (p (documents-page-number pn))
         (keep (and e (let ((path (%pic-key e)))
                        (list* (%dk pn :low path p) (%dk pn :sharp path p) (%dk pn :sharp path (1+ p))
                               (mapcar (lambda (d) (%dk pn :page path (+ p d))) '(0 1 -1)))))))
    (when (and e (gethash (%dk pn :page (%pic-key e) p) (folder-views pn)))
      (setf keep (remove (%dk pn :low (%pic-key e) p) keep :test #'equal)))
    (loop for k being the hash-keys of (folder-views pn)
          unless (member k keep :test #'equal) collect k into gone
          finally (dolist (k gone)
                    (let ((v (gethash k (folder-views pn))))
                      (remhash k (folder-views pn))
                      ;; its pixels back to the pool -- unless the detail layer still shows them
                      (when (and (consp v) (not (eq (third v) (car (folder-detail-cache pn)))))
                        (if (listp (third v)) (mapc #'%doc-px-put (third v)) (%doc-px-put (third v)))))))))

;;; drawing

(defun %doc-reason-text (reason)
  (case reason
    (:locked "Password-protected")
    (:broken "Not a readable PDF")
    (:too-big "Too big to open here")
    (t "Can't draw this page")))

(defmethod folder-draw-cell ((pn documents-panel) e x y w h)
  (let* ((fb (panel-fb pn)) (th (gethash (%pic-key e) (folder-thumbs pn)))
         (pages (%doc-pages pn e)) (idx (position e (folder-files pn))))
    (multiple-value-bind (bw bh) (folder-thumb-box pn)
      (let ((bx (+ x 8)) (by (+ y 6)) (tx (+ x 8 bw 14)))
        (glass:fb-rect fb (+ x 2) (+ y 2) (- w 4) (- h 4) +ra-row+)
        (cond ((consp th) (%pic-blit-centred fb bx by bw bh th))
              (t (glass:fb-rect fb (+ bx (floor bw 8)) by (- bw (floor bw 4)) bh +ra-control+)
                 (%pic-centred-text fb bx bw (+ by (floor bh 2) 5) (if th "!" "...") (if th 16 12) +ra-dim+)))
        (let ((room (- (+ x w) tx 10)) (mid (+ y (floor h 2))))
          (%ra-text fb tx (- mid 4) (%ra-fit (%doc-shown-title pn e) room 13) :size 13)
          (%ra-text fb tx (+ mid 14)
                    ;; the file name under a title it is not; nothing yet but the dots while it is read
                    (%ra-fit (let ((name (and (string/= (%doc-shown-title pn e) (pic-entry-name e)) (pic-entry-name e))))
                               (cond ((keywordp th) (%doc-reason-text th))
                                     (pages (format nil "~d page~:p~@[  ~a~]" pages name))
                                     (t "...")))
                             room 10)
                    :size 10 :color (if (keywordp th) +ra-bad+ +ra-dim+)))
        (%pic-target pn x y w h
                     (lambda () (setf (folder-mode pn) :view (folder-index pn) idx (documents-page-number pn) 0
                                      (documents-zoom pn) 1 (documents-pan pn) (cons 0 0)))
                     (pic-entry-name e))))))

(defmethod folder-step ((pn documents-panel) d)
  "The next (D 1) or previous (D -1) page -- CLAMPED to the document: before the page count is known
   only back."
  (let* ((e (%doc-open-entry pn)) (n (and e (%doc-pages pn e))) (p (documents-page-number pn))
         (to (max 0 (if n (min (1- n) (+ p d)) (min p (+ p d))))))
    (unless (= to p)
      ;; a new page is read from its top left (zoomed: the top of its first column)
      (setf (documents-pan pn) (cons 0 0) (documents-page-number pn) to))
    to))

;;; zoom

(defparameter *document-zoom* 2
  "Tapping the page zooms it this many times (the printed area this many times the tab's size,
   panned by dragging or the arrow keys): a column of a two-column paper fills a phone's width.")

(defun %doc-shown (pn)
  "The open page's rendering at the zoom shown, (W H PIXELS) -- or its low one -- or NIL."
  (let ((e (%doc-open-entry pn)) (p (documents-page-number pn)))
    (and e (let ((v (gethash (%dk pn :page (%pic-key e) p) (folder-views pn)))
                 (low (gethash (%dk pn :low (%pic-key e) p) (folder-views pn))))
             (or (and (consp v) v) (and (consp low) low))))))

(defun %doc-pan-max (w h bw bh)
  "(values FX FY): the furthest a W x H page can be panned in a BW x BH view."
  (values (if (> w bw) (- 1 (/ bw w)) 0) (if (> h bh) (- 1 (/ bh h)) 0)))

(defun %doc-set-pan (pn fx fy)
  "Pan to FX, FY (fractions of the page shown), kept inside it."
  (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
    (declare (ignore bx by))
    (multiple-value-bind (w h) (let ((v (%doc-shown pn)))
                                 (if v (values (first v) (second v))
                                     (values (* bw (documents-zoom pn)) (* bh (documents-zoom pn)))))
      (multiple-value-bind (mx my) (%doc-pan-max w h bw bh)
        (setf (documents-pan pn) (cons (max 0 (min mx fx)) (max 0 (min my fy))))))))

(defun %doc-toggle-zoom (pn x y)
  "Zoom in on the point X,Y (it stays under the finger), or back out to the whole printed area."
  (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
    (if (/= (documents-zoom pn) 1)
        (setf (documents-zoom pn) 1 (documents-pan pn) (cons 0 0))
        (let ((v (%doc-shown pn)) (z *document-zoom*))
          (setf (documents-zoom pn) z (documents-pan pn) (cons 0 0))
          (when v
            (destructuring-bind (w h &rest px) v
              (declare (ignore px))
              (let* ((ox (+ bx (floor (- bw w) 2))) (oy (+ by (floor (- bh h) 2)))
                     (fx (max 0 (min 1 (/ (- x ox) w)))) (fy (max 0 (min 1 (/ (- y oy) h)))))
                (%doc-set-pan pn (- fx (/ (- x bx) (* z w))) (- fy (/ (- y by) (* z h)))))))))))

(defmethod folder-panel-pointer ((pn documents-panel) edge x y)
  "On the open page: a tap (a press and a release without moving) zooms in or out; zoomed, a drag
   pans.  Elsewhere, as every folder tab: targets act on the press."
  (let ((drag (documents-drag pn)))
    (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
      (case edge
        (:press
         (if (and (eq (folder-mode pn) :view) (%doc-open-entry pn)
                  (not (%doc-refusal pn (%doc-open-entry pn)))
                  (<= bx x (+ bx bw -1)) (<= by y (+ by bh -1)))
             (setf (documents-drag pn) (list x y (documents-pan pn) nil))
             (call-next-method)))
        (:drag
         (when drag
           (destructuring-bind (x0 y0 pan moved) drag
             (when (or moved (> (max (abs (- x x0)) (abs (- y y0))) 8))
               (setf (fourth drag) t)
               (when (/= (documents-zoom pn) 1)
                 (let ((v (%doc-shown pn)))
                   (when v
                     (%doc-set-pan pn (- (car pan) (/ (- x x0) (first v))) (- (cdr pan) (/ (- y y0) (second v))))
                     (setf (panel-dirty pn) t))))))))
        (:release
         (when drag
           (setf (documents-drag pn) nil)
           (unless (fourth drag)
             (%doc-toggle-zoom pn x y)
             (setf (panel-dirty pn) t))))))))

(defmethod folder-panel-key ((pn documents-panel) keysym)
  "Reading: Left / PageUp the previous page, Right / PageDown the next, Home / End the first and the
   last; + / - zoom in and out, and zoomed, Up / Down move most of a view down the page; Escape or
   BackSpace back to the list.  The list: as every folder tab."
  (if (eq (folder-mode pn) :view)
      (let ((n (let ((e (%doc-open-entry pn))) (and e (%doc-pages pn e))))
            (zoomed (/= (documents-zoom pn) 1)))
        (case keysym
          ((#xff51 #xff55) (folder-step pn -1))
          ((#xff53 #xff56) (folder-step pn 1))
          (#xff50 (folder-step pn (- (documents-page-number pn))))
          (#xff57 (when n (folder-step pn (- n 1 (documents-page-number pn)))))
          ((#x2b #x3d) (unless zoomed (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
                                        (%doc-toggle-zoom pn (+ bx (floor bw 2)) (+ by (floor bh 2))))))
          (#x2d (when zoomed (%doc-toggle-zoom pn 0 0)))
          ((#xff52 #xff54)
           (let ((v (%doc-shown pn)))
             (when (and zoomed v)
               (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
                 (declare (ignore bx by bw))
                 (%doc-set-pan pn (car (documents-pan pn))
                               (+ (cdr (documents-pan pn))
                                  (* (if (= keysym #xff52) -0.8 0.8) (/ bh (second v)))))))))
          ((#xff1b #xff08) (%pic-back pn))
          (t (return-from folder-panel-key nil)))
        (setf (panel-dirty pn) t)
        t)
      (call-next-method)))

(defun %doc-blit (pn value)
  "A rendered page in the view: centred where it is smaller, panned where it is bigger."
  (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
    (destructuring-bind (w h px &rest more) value
      (declare (ignore more))
      (multiple-value-bind (mx my) (%doc-pan-max w h bw bh)
        (let ((fx (min mx (car (documents-pan pn)))) (fy (min my (cdr (documents-pan pn)))))
          (%pic-blit (panel-fb pn)
                     (if (> w bw) (- bx (round (* fx w))) (+ bx (floor (- bw w) 2)))
                     (if (> h bh) (- by (round (* fy h))) (+ by (floor (- bh h) 2)))
                     w h px))))))

(defmethod folder-detail ((pn documents-panel) scale)
  "The open page at SCALE times the panel's pixels, laid out as %DOC-BLIT lays the page out
   (centred, or panned when zoomed), confined to the view box: (values X Y FB BOX) in SCREEN
   pixels.  FB is the worker's own pixels, wrapped, not copied -- a landscape page is 10 MB, and
   copying it on the main thread stalled the desk for seconds.  The first ask only records SCALE
   and asks for the render (FOLDER-WISH's :SHARP); NIL until it arrives."
  (unless (eql scale (folder-detail-scale pn))
    (setf (folder-detail-scale pn) scale (panel-dirty pn) t))
  (let* ((e (%doc-open-entry pn))
         (v (and e (> scale 1) (not (%doc-refusal pn e))
                 (gethash (%dk pn :sharp (%pic-key e) (documents-page-number pn)) (folder-views pn)))))
    (when (consp v)
      (destructuring-bind (w h px &rest more) v
        (declare (ignore more))
        (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
          (let* ((sw (* bw scale)) (sh (* bh scale))
                 (c (folder-detail-cache pn))
                 ;; one framebuffer, or (a page read in bands) a list of (DY . FB), top first
                 (pfb (if (and c (eq (car c) px))
                          (cdr c)
                          (let ((f (if (listp px)
                                       (let ((dy 0))
                                         (mapcar (lambda (band)
                                                   (let ((bh (floor (length band) w)))
                                                     (prog1 (cons dy (glass::%make-framebuffer :width w :height bh :pixels band))
                                                       (incf dy bh))))
                                                 px))
                                       (glass::%make-framebuffer :width w :height h :pixels px))))
                            (setf (folder-detail-cache pn) (cons px f))
                            f))))
            (multiple-value-bind (mx my) (%doc-pan-max w h sw sh)
              (let ((fx (min mx (car (documents-pan pn)))) (fy (min my (cdr (documents-pan pn)))))
                (values (+ (* bx scale) (if (> w sw) (- (round (* fx w))) (floor (- sw w) 2)))
                        (+ (* by scale) (if (> h sh) (- (round (* fy h))) (floor (- sh h) 2)))
                        pfb
                        (list (* bx scale) (* by scale) sw sh))))))))))

(defun %doc-note (fb bx bw y text)
  "TEXT on a dark pill, centred across BX..BX+BW at Y: readable over a white page."
  (let* ((tw (%ra-width text 12)) (pw (+ tw 24)) (x (+ bx (floor (- bw pw) 2))))
    (glass:fb-rect fb x y pw 26 +ra-head+)
    (%ra-text fb (+ x 12) (%ra-baseline y 26 12) text :size 12 :color +ra-fg+)))

(defmethod folder-draw-view ((pn documents-panel))
  (let* ((fb (panel-fb pn))
         (e (%doc-open-entry pn))
         (path (and e (%pic-key e)))
         (p (documents-page-number pn))
         (n (and e (%doc-pages pn e)))
         (refusal (and e (%doc-refusal pn e)))
         (v (and e (gethash (%dk pn :page path p) (folder-views pn))))
         (low (and e (gethash (%dk pn :low path p) (folder-views pn)))))
    (multiple-value-bind (bx by bw bh) (%pic-view-box pn)
      (glass:fb-rect fb bx by bw bh +ra-bg+)
      (cond (refusal
             (%pic-centred-text fb bx bw (+ by (floor bh 2) -6) (%doc-reason-text refusal) 15 +ra-fg+)
             (%pic-centred-text fb bx bw (+ by (floor bh 2) 18)
                                (case refusal
                                  (:locked "It needs a password, which this reader cannot ask for.")
                                  (:broken "folio could not read this file.")
                                  (:too-big "Larger than this tab will open.")
                                  (t (pic-entry-name e)))
                                11 +ra-dim+))
            ((consp v) (%doc-blit pn v))
            ((keywordp v)
             (%pic-centred-text fb bx bw (+ by (floor bh 2)) (%doc-reason-text v) 13 +ra-dim+))
            (t
             (let ((stand-in (or (and (consp low) low)
                                 (and (zerop p) (let ((th (gethash path (folder-thumbs pn)))) (and (consp th) th))))))
               (when stand-in
                 (if (eq stand-in low) (%doc-blit pn stand-in) (%pic-blit-centred fb bx by bw bh stand-in)))
               (%doc-note fb bx bw (- (+ by bh) 40) "Rendering...")))))
    ;; over the page, which zoomed is bigger than the view
    (%pic-head pn (if e (%doc-shown-title pn e) "")
               (if refusal ""
                   (format nil "~a~:[~;  ~dx~]"
                           (if n (format nil "page ~d/~d" (1+ p) n) (format nil "page ~d" (1+ p)))
                           (/= (documents-zoom pn) 1) (documents-zoom pn))))
    (%folder-bar pn :prev (and (not refusal) (plusp p)) :next (and (not refusal) n (< (1+ p) n)))))

;;; the contract

(defun documents-app (fb)
  "Documents in FB: (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN) -- no RESIZE-FN, no TEXT-P.
   MEDIA-APP finds this by name for its Documents tab."
  (%folder-app (make-documents-panel fb)))
