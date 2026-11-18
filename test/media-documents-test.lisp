;;;; test/media-documents-test.lisp -- Media's Documents tab (glass/media-documents): the tab only when the
;;;; system is loaded, the folder (GLASS_DOCUMENTS, ~/Documents, Media's folder) and never above it, the
;;;; listing with titles and page counts, first-page thumbnails made off the caller's thread, the reader
;;;; (fitted page, page i/n, Prev / Back / Next that CLAMP, keys, the neighbours rendered ahead, a low
;;;; render while the full one comes, only the near pages kept), password-protected and broken PDFs
;;;; reported, folders and the up row, the cooperative fallback, and closing.
;;;;   sbcl --script test/media-documents-test.lisp      (exits 0 iff every check passes)
;;;;
;;;; The fixture is a folder of real PDFs from folio's own test directory (an IRS form with a title and six
;;;; pages, a 16-page paper, a one-page sample, one with no title, one AES-encrypted with an empty user
;;;; password, one that needs a password), a file that is not a PDF at all, a text file, a hidden PDF, a
;;;; subfolder with a subfolder, and two links that lead OUT of the folder (a directory and a file).  Set
;;;; DOCUMENTS_PNG_DIR to also write the window as PNGs (480x520 and 400x700, the list and a page open).

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

(defpackage #:glass-media-documents-test (:use #:cl))
(in-package #:glass-media-documents-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&MEDIA-DOCUMENTS ~:[FAIL~;ok  ~] ~a~%" ok name)
  (unless ok (incf *fails*))
  ok)

;;; ---- before glass/media-documents: no Documents tab ----------------------------------------------------

(check "without glass/media-documents: Music and Stations, no Documents tab"
       (equal (glass.desk::%media-tab-keys) '(:music :stations)))

(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
    (asdf:load-system :glass/media-documents)))

(check "with it: a Documents tab (and not Pictures, which it does not bring)"
       (equal (glass.desk::%media-tab-keys) '(:music :stations :documents)))
(check "Documents needs folio (it is loaded with it)" (and (find-package "FOLIO") t))

;;; ---- the fixture ----------------------------------------------------------------------------------

(defvar *folio-test* (merge-pathnames "test/" (asdf:system-source-directory :folio)))
(defvar *base* (format nil "~a/glass-media-documents-~d/" (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                       (sb-posix:getpid)))
(defvar *root* (format nil "~aDocuments/" *base*))
(defvar *outside* (format nil "~aoutside/" *base*))
(ensure-directories-exist (format nil "~areports/old/" *root*))
(ensure-directories-exist *outside*)
(defun in (name) (format nil "~a~a" *root* name))

(defun copy (from to)
  (with-open-file (i (merge-pathnames from *folio-test*) :element-type '(unsigned-byte 8))
    (with-open-file (o to :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (let ((b (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence b i) (write-sequence b o)))))

(copy "gov-w9.pdf" (in "gov-w9.pdf"))
(copy "real-bert.pdf" (in "real-bert.pdf"))
(copy "real-sample.pdf" (in "real-sample.pdf"))
(copy "text-test.pdf" (in "text-test.pdf"))
(copy "enc-aes-128.pdf" (in "enc-aes-128.pdf"))
(copy "enc-userpw.pdf" (in "enc-userpw.pdf"))
(copy "a-rect.pdf" (in ".hidden.pdf"))
(copy "a-rect.pdf" (in "reports/a-rect.pdf"))
(copy "b-flate.pdf" (in "reports/old/b-flate.pdf"))
(copy "a-rect.pdf" (format nil "~asecret.pdf" *outside*))
(with-open-file (o (in "broken.pdf") :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
  (write-sequence (map '(vector (unsigned-byte 8)) #'char-code "%PDF-1.4 this is not really a PDF at all") o)
  (write-sequence (make-array 200 :element-type '(unsigned-byte 8) :initial-element 7) o))
(with-open-file (o (in "notes.txt") :direction :output :if-exists :supersede) (write-line "not a document" o))
(sb-posix:symlink *outside* (string-right-trim "/" (in "outside")))
(sb-posix:symlink (format nil "~asecret.pdf" *outside*) (in "escape.pdf"))

(defvar *names* '("broken.pdf" "enc-aes-128.pdf" "enc-userpw.pdf" "gov-w9.pdf" "real-bert.pdf" "real-sample.pdf"
                  "text-test.pdf"))

;;; ---- the folder -----------------------------------------------------------------------------------

(defvar *home* (format nil "~ahome/" *base*))
(ensure-directories-exist (format nil "~aDocuments/" *home*))
(defvar *bare-home* (format nil "~abare/" *base*))
(ensure-directories-exist *bare-home*)
(defun same-dir (a b) (equal (namestring (truename a)) (namestring (truename b))))

(sb-posix:setenv "GLASS_DOCUMENTS" *root* 1)
(check "GLASS_DOCUMENTS names the folder" (same-dir (glass.desk:documents-root :home *home*) *root*))
(sb-posix:unsetenv "GLASS_DOCUMENTS")
(check "else ~/Documents" (same-dir (glass.desk:documents-root :home *home*) (format nil "~aDocuments/" *home*)))
(let ((glass.desk:*media-root* *outside*))
  (check "else Media's folder" (same-dir (glass.desk:documents-root :home *bare-home*) *outside*)))
(sb-posix:setenv "GLASS_DOCUMENTS" *root* 1)

(multiple-value-bind (dirs files) (glass.desk::folder-listing (truename *root*) (truename *root*) '("pdf"))
  (let ((dn (mapcar #'glass.desk:pic-entry-name dirs)) (fn (mapcar #'glass.desk:pic-entry-name files)))
    (check "listing: the .pdf files, sorted; not the text file, not the hidden one" (equal fn *names*))
    (check "listing: the subfolder" (equal dn '("reports")))
    (check "a link that leads out of the folder (a directory) is not listed" (not (member "outside" dn :test #'string=)))
    (check "nor one to a PDF outside" (not (member "escape.pdf" fn :test #'string=)))))

;;; ---- the window ---------------------------------------------------------------------------------------

(defun save-png (fb path)
  (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb))
         (px (make-array (* w h 3) :element-type '(unsigned-byte 8))))
    (dotimes (y h) (dotimes (x w)
      (let ((c (glass:fb-get fb x y)) (i (* 3 (+ x (* y w)))))
        (setf (aref px i) (ldb (byte 8 16) c) (aref px (+ i 1)) (ldb (byte 8 8) c) (aref px (+ i 2)) (ldb (byte 8 0) c)))))
    (png-pure:write-png-file px w h path)))

(defvar *png-dir* (let ((d (sb-ext:posix-getenv "DOCUMENTS_PNG_DIR"))) (and d (plusp (length d)) (string-right-trim "/" d))))
(defun shot (fb name) (when *png-dir* (ensure-directories-exist (format nil "~a/" *png-dir*))
                        (save-png fb (format nil "~a/~a" *png-dir* name))))

(defvar *panels* '())
(defmethod initialize-instance :after ((p glass.desk:documents-panel) &key) (push p *panels*))

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
(defun key (on-key sym) (funcall on-key t sym) (funcall on-key nil sym))
(defun now () (/ (get-internal-real-time) internal-time-units-per-second))
(defun pkey (name) (namestring (truename (in name))))
(defun page-key (name page) (glass.desk::%doc-key :page (pkey name) page))
(defun low-key (name page) (glass.desk::%doc-key :low (pkey name) page))
(defun views (pn) (glass.desk:folder-views pn))
(defun thumbs (pn) (glass.desk:folder-thumbs pn))
(defun info (pn name) (gethash (pkey name) (glass.desk:documents-info pn)))

(defun settle (dirty-p pn seconds)
  "Call DIRTY-P until every document on PN's list page has its thumbnail (or a refusal).  (values done
   slowest-call-seconds calls-while-waiting)."
  (let ((end (+ (now) seconds)) (slowest 0) (calls 0))
    (loop
      (let ((t0 (now)))
        (funcall dirty-p)
        (setf slowest (max slowest (- (now) t0))))
      (incf calls)
      (let ((page (glass.desk::%pic-page-images pn (glass.desk:folder-page pn))))
        (when (every (lambda (e) (gethash (glass.desk::%pic-key e) (thumbs pn))) page)
          (return (values t slowest calls))))
      (when (> (now) end) (return (values nil slowest calls)))
      (sleep 0.01))))

(defun wait-for (dirty-p seconds test)
  "Call DIRTY-P until TEST is true.  (values ok slowest-call)."
  (let ((end (+ (now) seconds)) (slowest 0))
    (loop (let ((t0 (now))) (funcall dirty-p) (setf slowest (max slowest (- (now) t0))))
          (when (funcall test) (return (values t slowest)))
          (when (> (now) end) (return (values nil slowest)))
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

(defun scale-for (name page box-w box-h)
  "The px-per-pt the reader shows NAME's PAGE at in a BOX-WxBOX-H area (worked out here, the same way)."
  (let ((doc (folio:parse-pdf (glass.desk::%pic-read (in name)))))
    (multiple-value-bind (pw ph) (glass.desk::%doc-shown-size doc page)
      (multiple-value-bind (img crop) (glass.desk::%doc-low doc page box-w box-h)
        (declare (ignore img))
        (values (glass.desk::%doc-geometry pw ph crop box-w box-h) (min (/ box-w pw) (/ box-h ph)))))))

(defun view-h (h) (- h glass.desk::*media-tab-h* glass.desk::*ra-head-h* glass.desk::*pic-bar-h*))

(let ((glass.desk:*media-root* *root*) (glass.desk::*media-library* nil))
  (multiple-value-bind (fb on-key on-pointer dirty-p close-fn) (open-media 480 520)
    (check "it paints when opened" (funcall dirty-p))
    (check "and Documents is not made until it is shown (no folder read, nothing parsed)" (null *panels*))
    (check "three tabs, Documents the last third"
           (eq (glass.desk::%media-tab-at '(:music :stations :documents) 480 479) :documents))
    (tap on-pointer 400 18)
    (let ((t0 (now)))
      (funcall dirty-p)
      (check "a tap on Documents shows it at once (the first paint does not wait for a PDF)" (< (- (now) t0) 0.5)))
    (let ((pn (first *panels*)))
      (check "the Documents tab is the one marked" (= (glass:fb-get fb 400 34) glass.desk::+ra-on+))
      (check "it opens on the folder GLASS_DOCUMENTS names" (same-dir (glass.desk:folder-path pn) *root*))
      (check "the first page: rows, the subfolder first, whole rows the target, at least 48 px tall"
             (let ((cells (remove-if (lambda (tg) (member (glass.desk::rt-label tg) '("Previous page" "Next page")
                                                          :test #'equal))
                                     (targets pn))))
               (and (= 5 (length cells)) (target pn "reports") (target pn "broken.pdf") (target pn "gov-w9.pdf")
                    (every (lambda (tg) (and (= 480 (glass.desk::rt-w tg)) (>= (glass.desk::rt-h tg) 48))) cells))))
      (check "8 items at 5 a page: a pager" (and (target pn "Next page") (target pn "Previous page")))
      (check "thumbnails are not there yet: they are being made elsewhere"
             (notevery (lambda (e) (gethash (glass.desk::%pic-key e) (thumbs pn)))
                       (glass.desk::%pic-page-images pn 0)))
      (check "on a worker thread of the tab's own" (and (glass.desk:folder-thread pn)
                                                       (sb-thread:thread-alive-p (glass.desk:folder-thread pn))))
      (multiple-value-bind (done slowest calls) (settle dirty-p pn 120)
        (check "every first page on the list page arrives" done)
        (check (format nil "and DIRTY-P never waited on folio (slowest call ~,3f s over ~d calls)" slowest calls)
               (and (< slowest 0.25) (> calls 5))))
      (check "the title is the PDF's Info /Title, with its page count"
             (equal (info pn "gov-w9.pdf") '("Form W-9 (Rev. March 2024)" . 6)))
      (check "the encrypted one with no user password opens (folio decrypts it): title and pages"
             (equal (info pn "enc-aes-128.pdf") '("sample" . 1)))
      (check "the one that needs a password is reported, quietly" (eq (gethash (pkey "enc-userpw.pdf") (thumbs pn)) :locked))
      (check "the broken one is reported, quietly" (eq (gethash (pkey "broken.pdf") (thumbs pn)) :broken))
      (check "a thumbnail is the first page, upright, inside the row's box"
             (let ((v (gethash (pkey "gov-w9.pdf") (thumbs pn))))
               (multiple-value-bind (bw bh) (glass.desk::folder-thumb-box pn)
                 (and (consp v) (<= (first v) bw) (= (second v) bh) (< (first v) (second v))))))
      (check "and the rows say so: the IRS form's row has its white page and its title drawn"
             (let ((tg (target pn "gov-w9.pdf")))
               (loop for x from 8 below 70 thereis
                     (= #xffffff (glass:fb-get fb x (+ glass.desk::*media-tab-h* (glass.desk::rt-y tg) 40))))))
      (check "and then it is quiet: no repaint for a second with nothing new" (quiet-p dirty-p 1))
      (shot fb "documents-list-480x520.png")
      ;; the list's pages
      (tap-label on-pointer pn "Next page") (funcall dirty-p)
      (check "Next page: the rest" (and (= 1 (glass.desk:folder-page pn)) (target pn "text-test.pdf")
                                        (not (target pn "broken.pdf"))))
      (settle dirty-p pn 60)
      (check "a PDF with no title shows its file name (text-test), a page count still"
             (and (equal (info pn "text-test.pdf") '(nil . 1))
                  (equal (glass.desk::%doc-shown-title pn (find "text-test.pdf" (glass.desk:folder-files pn)
                                                                :key #'glass.desk:pic-entry-name :test #'string=))
                         "text-test.pdf")))
      (check "the 16-page paper (its Title is empty: the file name)" (equal (info pn "real-bert.pdf") '(nil . 16)))
      (tap-label on-pointer pn "Previous page") (funcall dirty-p)
      ;; the reader
      (tap-label on-pointer pn "gov-w9.pdf")
      (funcall dirty-p)
      (check "a tap on a document opens it, at page 1"
             (and (eq (glass.desk:folder-mode pn) :view) (zerop (glass.desk:documents-page-number pn))))
      (check "with the big controls: Prev, Back, Next, a third of the width each, 56 px tall"
             (every (lambda (l) (let ((tg (target pn l)))
                                  (and tg (>= (glass.desk::rt-w tg) 160) (>= (glass.desk::rt-h tg) 56))))
                    '("Prev" "Back" "Next")))
      (let ((low-first nil))
        (multiple-value-bind (ok slowest)
            (wait-for dirty-p 30 (lambda ()
                                   (when (and (consp (gethash (low-key "gov-w9.pdf" 0) (views pn)))
                                              (not (gethash (page-key "gov-w9.pdf" 0) (views pn))))
                                     (unless low-first
                                       (funcall dirty-p) (shot fb "documents-rendering-480x520.png"))
                                     (setf low-first t))
                                   (consp (gethash (page-key "gov-w9.pdf" 0) (views pn)))))
          (check "the page is rendered off-thread, and arrives" ok)
          (check (format nil "a low render shows first (\"Rendering...\" over it) while the full one comes, ~
                              DIRTY-P never waiting (slowest ~,3f s)" slowest)
                 (and low-first (< slowest 0.25)))))
      (funcall dirty-p)
      (let ((v (gethash (page-key "gov-w9.pdf" 0) (views pn))))
        (check "fitted: the printed area whole inside the tab, as wide or as tall as it can be"
               (and (consp v) (<= (first v) 480) (<= (second v) (view-h 520))
                    (or (>= (first v) 479) (>= (second v) (1- (view-h 520))))))
        (check "drawn: white page in the middle of the view"
               (= #xffffff (glass:fb-get fb 240 (+ glass.desk::*media-tab-h* 36 (floor (view-h 520) 2))))))
      (check "the head: the title and page 1/6"
             (and (string= (glass.desk::%doc-shown-title pn (glass.desk::%doc-open-entry pn)) "Form W-9 (Rev. March 2024)")))
      (check "the next page is rendered ahead (and there is no previous to render)"
             (and (wait-for dirty-p 30 (lambda () (consp (gethash (page-key "gov-w9.pdf" 1) (views pn)))))
                  (null (gethash (page-key "gov-w9.pdf" -1) (views pn)))))
      (shot fb "documents-page-480x520.png")
      (tap-label on-pointer pn "Prev") (funcall dirty-p)
      (check "Prev on the first page CLAMPS: still page 1" (zerop (glass.desk:documents-page-number pn)))
      (tap-label on-pointer pn "Next") (funcall dirty-p)
      (check "Next: page 2, already there (no wait on turning)"
             (and (= 1 (glass.desk:documents-page-number pn)) (consp (gethash (page-key "gov-w9.pdf" 1) (views pn)))))
      (check "and both neighbours are rendered ahead"
             (wait-for dirty-p 30 (lambda () (and (consp (gethash (page-key "gov-w9.pdf" 0) (views pn)))
                                                  (consp (gethash (page-key "gov-w9.pdf" 2) (views pn)))))))
      (key on-key #xff56) (funcall dirty-p)
      (check "PageDown: the next page" (= 2 (glass.desk:documents-page-number pn)))
      (key on-key #xff55) (funcall dirty-p)
      (check "PageUp: the previous" (= 1 (glass.desk:documents-page-number pn)))
      (key on-key #xff53) (funcall dirty-p)
      (check "Right: the next" (= 2 (glass.desk:documents-page-number pn)))
      (key on-key #xff51) (funcall dirty-p)
      (check "Left: the previous" (= 1 (glass.desk:documents-page-number pn)))
      (key on-key #xff57) (funcall dirty-p)
      (check "End: the last page" (= 5 (glass.desk:documents-page-number pn)))
      (tap-label on-pointer pn "Next") (funcall dirty-p)
      (check "Next on the last page CLAMPS: still page 6" (= 5 (glass.desk:documents-page-number pn)))
      (key on-key #xff53) (funcall dirty-p)
      (check "and so does Right" (= 5 (glass.desk:documents-page-number pn)))
      (wait-for dirty-p 30 (lambda () (consp (gethash (page-key "gov-w9.pdf" 4) (views pn)))))
      (check "rendered pages kept: the open one and its neighbours only"
             (let ((ks (loop for k being the hash-keys of (views pn) collect k)))
               (and (<= (length ks) 3)
                    (every (lambda (k) (member k (list (page-key "gov-w9.pdf" 5) (page-key "gov-w9.pdf" 4)
                                                       (low-key "gov-w9.pdf" 5))
                                               :test #'equal))
                           ks))))
      (key on-key #xff50) (funcall dirty-p)
      (check "Home: the first page" (zerop (glass.desk:documents-page-number pn)))
      (key on-key #xff1b) (funcall dirty-p)
      (check "Escape: back to the list, on the page holding the document"
             (and (eq (glass.desk:folder-mode pn) :list) (= 0 (glass.desk:folder-page pn)) (target pn "gov-w9.pdf")))
      (funcall dirty-p)
      (check "and no rendered page is kept once nothing is open" (zerop (hash-table-count (views pn))))
      (tap-label on-pointer pn "gov-w9.pdf") (funcall dirty-p)
      (tap-label on-pointer pn "Back") (funcall dirty-p)
      (check "Back (the button): the list" (eq (glass.desk:folder-mode pn) :list))
      ;; refusals
      (tap-label on-pointer pn "enc-userpw.pdf") (funcall dirty-p)
      (check "opening the password-protected one shows why, not a page, and asks nothing of the worker"
             (and (eq (glass.desk:folder-mode pn) :view)
                  (target pn "Back")
                  (notany (lambda (j) (member (first j) '(:low :page))) (glass.desk::folder-wish pn))))
      (shot fb "documents-locked-480x520.png")
      (tap-label on-pointer pn "Next") (funcall dirty-p)
      (check "and Next does nothing there" (zerop (glass.desk:documents-page-number pn)))
      (key on-key #xff08) (funcall dirty-p)
      (tap-label on-pointer pn "broken.pdf") (funcall dirty-p)
      (check "the broken one too" (and (eq (glass.desk:folder-mode pn) :view) (target pn "Back")
                                       (eq :broken (glass.desk::%doc-refusal pn (glass.desk::%doc-open-entry pn)))))
      (key on-key #xff08) (funcall dirty-p)
      ;; folders
      (check "at the root there is no up row" (not (target pn "Up")))
      (tap-label on-pointer pn "reports") (funcall dirty-p)
      (check "a tap on a folder opens it" (same-dir (glass.desk:folder-path pn) (in "reports/")))
      (check "its documents and its folder, and an up row"
             (and (target pn "a-rect.pdf") (target pn "old") (target pn "Up")))
      (tap-label on-pointer pn "old") (funcall dirty-p)
      (check "and one inside that" (same-dir (glass.desk:folder-path pn) (in "reports/old/")))
      (tap-label on-pointer pn "Up") (funcall dirty-p)
      (check "Up: the parent" (same-dir (glass.desk:folder-path pn) (in "reports/")))
      (key on-key #xff08) (funcall dirty-p)
      (check "BackSpace: up again, to the root" (same-dir (glass.desk:folder-path pn) *root*))
      (glass.desk:folder-up pn)
      (check "up STOPS at the root" (same-dir (glass.desk:folder-path pn) *root*))
      (glass.desk:folder-open pn (in "outside/"))
      (check "the link that leads out is not entered, even asked by name" (same-dir (glass.desk:folder-path pn) *root*))
      (glass.desk:folder-open pn *outside*)
      (check "nor the folder it leads to" (same-dir (glass.desk:folder-path pn) *root*))
      (glass.desk:folder-open pn (format nil "~areports/../../" *root*))
      (check "nor a path that climbs out" (same-dir (glass.desk:folder-path pn) *root*))
      (funcall dirty-p)
      ;; the other tabs still work, and Documents stays painted while hidden
      (tap on-pointer 240 18) (funcall dirty-p)
      (check "Stations is the middle tab" (= (glass:fb-get fb 240 34) glass.desk::+ra-on+))
      (tap on-pointer 400 18)
      (check "and back on Documents it shows at once" (funcall dirty-p))
      (let ((th (glass.desk:folder-thread pn)))
        (funcall close-fn)
        (check "closing the window stops its worker (and the parsed PDFs it held go with it)"
               (loop repeat 200 thereis (not (sb-thread:thread-alive-p th)) do (sleep 0.02))))
      (check "and drops its rendered pages" (zerop (hash-table-count (views pn)))))))

;;; ---- the phone: 400x700 ------------------------------------------------------------------------------
(let ((glass.desk:*media-root* *root*) (glass.desk::*media-library* nil))
  (multiple-value-bind (fb on-key on-pointer dirty-p close-fn) (open-media 400 700)
    (declare (ignore on-key))
    (funcall dirty-p)
    (tap on-pointer 350 18) (funcall dirty-p)
    (let ((pn (first *panels*)))
      (check "400x700: a fresh window has its own thumbnail cache" (zerop (hash-table-count (thumbs pn))))
      (check "400x700: rows the whole width, 7 to a page" (let ((tg (target pn "gov-w9.pdf")))
                                                            (and tg (= 400 (glass.desk::rt-w tg)) (= 7 (glass.desk::%pic-per-page pn)))))
      (settle dirty-p pn 120)
      (funcall dirty-p)
      (shot fb "documents-list-400x700.png")
      (tap-label on-pointer pn "real-bert.pdf")
      (wait-for dirty-p 60 (lambda () (consp (gethash (page-key "real-bert.pdf" 0) (views pn)))))
      (funcall dirty-p)
      (let ((v (gethash (page-key "real-bert.pdf" 0) (views pn))))
        (check "400x700: a paper's page fills the width (portrait: fit-width IS the whole printed area)"
               (and (consp v) (= 400 (first v)) (<= (second v) (view-h 700)))))
      (multiple-value-bind (s whole) (scale-for "real-bert.pdf" 0 400 (view-h 700))
        (check (format nil "400x700: margins trimmed, the page reads at ~,2f px/pt, not the whole page's ~,2f" s whole)
               (and (> s (* 1.1 whole)) (> s 0.7))))
      (shot fb "documents-page-400x700.png")
      (tap-label on-pointer pn "Next")
      (wait-for dirty-p 60 (lambda () (consp (gethash (page-key "real-bert.pdf" 1) (views pn)))))
      (funcall dirty-p)
      (shot fb "documents-page2-400x700.png")
      (check "400x700: page 2 of 16" (= 1 (glass.desk:documents-page-number pn)))
      ;; zoom: a tap on the page
      (let ((cy (+ glass.desk::*media-tab-h* glass.desk::*ra-head-h* 150)))
        (tap on-pointer 100 cy) (funcall dirty-p)
        (check "a tap on the page zooms in" (= 2 (glass.desk:documents-zoom pn)))
        (check "zoomed in where it was tapped (left column, upper half): panned to the left, part way down"
               (let ((pan (glass.desk:documents-pan pn))) (and (< 0 (car pan) 0.2) (< 0 (cdr pan) 0.3))))
        (check "the zoomed page is rendered off-thread and arrives"
               (wait-for dirty-p 60 (lambda () (consp (gethash (glass.desk::%doc-key :page (pkey "real-bert.pdf") 1 2)
                                                               (views pn))))))
        (funcall dirty-p)
        (let ((v (gethash (glass.desk::%doc-key :page (pkey "real-bert.pdf") 1 2) (views pn))))
          (check (format nil "zoomed: twice the fitted size (~{~d~^x~} px in a 400x~d view)" (and v (subseq v 0 2)) (view-h 700))
                 (and (consp v) (>= (max (/ (first v) 400) (/ (second v) (view-h 700))) 1.95))))
        (shot fb "documents-zoom-400x700.png")
        (let ((before (car (glass.desk:documents-pan pn))))
          (funcall on-pointer 1 300 cy) (funcall on-pointer 1 200 cy) (funcall on-pointer 1 100 cy)
          (funcall on-pointer 0 100 cy) (funcall dirty-p)
          (check "a drag pans (and does not zoom out)"
                 (and (> (car (glass.desk:documents-pan pn)) before) (= 2 (glass.desk:documents-zoom pn)))))
        (let ((before (cdr (glass.desk:documents-pan pn))))
          (funcall on-key t #xff54) (funcall dirty-p)
          (check "Down moves down the page" (> (cdr (glass.desk:documents-pan pn)) before)))
        (shot fb "documents-zoom-panned-400x700.png")
        (tap-label on-pointer pn "Next") (funcall dirty-p)
        (check "Next keeps the zoom and starts the page at its top left"
               (and (= 2 (glass.desk:documents-page-number pn)) (= 2 (glass.desk:documents-zoom pn))
                    (equal (glass.desk:documents-pan pn) '(0 . 0))))
        (tap on-pointer 200 cy) (funcall dirty-p)
        (check "and a tap zooms back out to the whole printed area" (= 1 (glass.desk:documents-zoom pn))))
      (funcall close-fn))))

;;; ---- without a thread: cooperative, one job a turn ------------------------------------------------------
(let* ((fb (glass:make-framebuffer 480 484))
       (pn (let ((glass.desk:*documents-threads* nil) (glass.desk:*documents-coop-max-bytes* (* 300 1024)))
             (glass.desk:make-documents-panel fb :root (truename *root*)))))
  (let ((glass.desk:*documents-coop-max-bytes* (* 300 1024)))
    (setf (glass.desk:folder-page pn) 1 (glass.desk::panel-dirty pn) t)
    (multiple-value-bind (done) (settle (lambda () (glass.desk:folder-panel-dirty-p pn)) pn 120)
      (check "cooperatively (no thread): the thumbnails still arrive, one a turn" done))
    (check "with no thread made" (null (glass.desk:folder-thread pn)))
    (check "and a PDF too big to render in turns is not opened"
           (eq :too-big (gethash (pkey "real-bert.pdf") (thumbs pn))))
    (setf (glass.desk:folder-page pn) 0 (glass.desk:folder-mode pn) :view
          (glass.desk:folder-index pn) (position "gov-w9.pdf" (glass.desk:folder-files pn)
                                                 :key #'glass.desk:pic-entry-name :test #'string=))
    (check "cooperatively a page still opens"
           (wait-for (lambda () (glass.desk:folder-panel-dirty-p pn)) 60
                     (lambda () (consp (gethash (page-key "gov-w9.pdf" 0) (views pn))))))
    (glass.desk:folder-panel-close pn)
    (check "and closing drops the parsed PDFs it held" (zerop (hash-table-count (glass.desk::folder-coop-cache pn))))))

;;; ---- with Pictures too: four tabs ------------------------------------------------------------------------
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
    (asdf:load-system :glass/media-pictures)))
(check "with glass/media-pictures as well: four tabs, Documents last"
       (equal (glass.desk::%media-tab-keys) '(:music :stations :pictures :documents)))
(let ((fb (glass:make-framebuffer 400 700)))
  (multiple-value-bind (k p dirty-p c close-fn) (glass.desk:media-app fb)
    (declare (ignore k c))
    (funcall dirty-p)
    (tap p 350 18) (funcall dirty-p)
    (check "and at phone width each is a 100-px fingertip target, Documents' the last"
           (and (eq (glass.desk::%media-tab-at '(:music :stations :pictures :documents) 400 350) :documents)
                (= (glass:fb-get fb 350 34) glass.desk::+ra-on+)))
    (shot fb "documents-four-tabs-400x700.png")
    (funcall close-fn)))

;;; ---- the app on its own -----------------------------------------------------------------------------------
(let ((fb (glass:make-framebuffer 320 240)))
  (multiple-value-bind (k p d c x r) (glass.desk:documents-app fb)
    (check "DOCUMENTS-APP is a desk app too: five values, no RESIZE-FN" (and (functionp k) (functionp p) (functionp d)
                                                                            (null c) (functionp x) (null r)))
    (funcall d) (funcall x)))

(ignore-errors (sb-ext:run-program "/bin/rm" (list "-rf" *base*)))
(format t "~&MEDIA-DOCUMENTS ~:[FAIL (~d)~;PASS~]~%" (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
