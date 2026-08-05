;;;; render-fix.lisp — an upstream McCLIM bug, carried here until it is fixed there.
;;;;
;;;; mcclim-render draws a string by first turning it into a vector of GLYPH CODES
;;;; and then looking each code back up in the font's cache.  A code is not a glyph
;;;; index: mcclim-truetype packs the character AND THE NEXT ONE into it (that is
;;;; how it kerns), as
;;;;
;;;;     code = char-code | (next-char-code << 21)
;;;;
;;;; so a code needs up to 42 bits — but the buffer it is written into
;;;; (RENDER-MEDIUM-%BUFFER%, filled by GLYPH-CODES-BUFFER) is (UNSIGNED-BYTE 32).
;;;; Any character above U+07FF that FOLLOWS another character therefore overflows
;;;; it and the drawing thread dies with a TYPE-ERROR — which, for an app, means
;;;; the window stops responding the moment it tries to draw a curly apostrophe or
;;;; an em dash.  That is not exotic text; it is most prose on the web.
;;;;
;;;; The CLX backend is unaffected because it hands X real (small) glyph ids.  The
;;;; fix here is the whole fix: make the buffer wide enough for the codes that are
;;;; actually put in it.  Nothing else reads its element type — the slot's :TYPE
;;;; declaration is not enforced by SBCL, and the only THE that asserted 32 bits
;;;; was inside this very function.
;;;;
;;;; Delete this file when mcclim-render widens the buffer upstream.

(in-package #:mcclim-render)

(defun glyph-codes-buffer (medium length)
  (let ((buffer (render-medium-%buffer% medium)))
    ;; The element type is checked as well as the length: the medium's initial
    ;; buffer is a 1024-element (unsigned-byte 32) array, so a short string would
    ;; otherwise keep the narrow one and keep crashing.
    (unless (and (typep buffer '(simple-array (unsigned-byte 64) (*)))
                 (>= (length buffer) length))
      (setf buffer (make-array (max length 1024)
                               :element-type '(unsigned-byte 64)
                               :adjustable nil :fill-pointer nil)
            (render-medium-%buffer% medium) buffer))
    buffer))
