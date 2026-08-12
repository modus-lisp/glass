;;;; selection-menu-gate.lisp — the selection menu: interception, pass-through, and the
;;;; line between a selection and a clipboard.
;;;;
;;;; Right-clicking a window that has text highlighted must offer to speak THAT TEXT, and
;;;; a right-click anywhere else must reach the application exactly as it always did.  The
;;;; whole gate is driven over a real RFB connection to a real WM on a real port: nothing
;;;; here is called that a finger could not have caused, because the bug this replaced was
;;;; precisely a rule that read correctly and behaved wrongly under a pointer.
;;;;
;;;; The load-bearing checks are the negative ones:
;;;;
;;;;   * a window with no selection concept at all gets its button 3 — press AND release.
;;;;   * a window whose selection has been DISMISSED gets its button 3 back.  This is the
;;;;     case the clipboard could not see: a clipboard outlives the highlight on purpose,
;;;;     so the old rule popped a menu over a page with nothing selected and offered to
;;;;     read out what had been copied minutes earlier.
;;;;   * a window whose selection has been REPLACED speaks the new one, not the old.
;;;;   * the clipboard moving under the WM's feet — filled, cleared, owned by somebody
;;;;     else — changes NOTHING about this menu.  That is the separation, stated as a test.
;;;;
;;;; and the root menu's "Speak clipboard" is checked to be the opposite: present with no
;;;; selection anywhere, and reading the clipboard rather than any window.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive \
;;;;        --load backend/inspect/selection-menu-gate.lisp
;;;;
;;;; It serves on 5958 to stay clear of the live desktops.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass)) (require :sb-concurrency)
    (ignore-errors (asdf:load-system :glass/speech))
    (asdf:load-asd (merge-pathnames "../mcclim-glass.asd" *load-truename*))
    (asdf:load-system :mcclim-glass)))

(defpackage #:glass-selmenu-gate (:use #:cl)) (in-package #:glass-selmenu-gate)

;;; ---- an RFB client with no screen ------------------------------------------
;;;
;;; It never decodes a framebuffer: what this gate reads is the WM's own state, in this
;;; same image.  The client exists to be the thing that pressed the button.

(defun r8 (s) (read-byte s))
(defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v))
(defun w32 (s v) (w16 s (ash v -16)) (w16 s v))

(defun connect (port)
  (loop repeat 500
        do (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
             (handler-case
                 (progn (sb-bsd-sockets:socket-connect
                         sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                        (return-from connect
                          (sb-bsd-sockets:socket-make-stream
                           sock :input t :output t :element-type '(unsigned-byte 8)
                                :buffering :full)))
               (error () (ignore-errors (sb-bsd-sockets:socket-close sock)) (sleep 0.05))))))

(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s) (w8 s 1) (force-output s)
  (let ((w (r16 s)) (h (r16 s)))
    (rn s 16) (let ((nl (r32 s))) (rn s nl))
    (w8 s 2) (w8 s 0) (w16 s 2) (w32 s 16) (w32 s 0) (force-output s)
    (values w h)))

(defun ptr (s mask x y) (w8 s 5) (w8 s mask) (w16 s x) (w16 s y) (force-output s) (sleep 0.08))
(defun rclick (s x y) (ptr s 4 x y) (ptr s 0 x y) (sleep 0.3))
(defun lclick (s x y) (ptr s 1 x y) (ptr s 0 x y) (sleep 0.4))

(defvar *fails* 0)
(defun ck (name got want)
  (let ((ok (equal got want)))
    (unless ok (incf *fails*))
    (format t "~&~:[FAIL~;ok  ~] ~a~:[  got ~s want ~s~;~]~%" ok name ok got want)
    (finish-output)))
(defun ck-that (name v) (ck name (and v t) t))

;;; ---- a surface that records what the WM forwarded to it ---------------------

(defvar *got* '())                      ; every pointer mask the WM let through
(defvar *sel* nil)                      ; what this surface currently has "highlighted"

(defun probe-surface (fb)
  (glass:fb-rect fb 0 0 (glass:fb-width fb) (glass:fb-height fb) (glass:rgb 240 240 200))
  (values (lambda (down k) (declare (ignore down k)))
          (lambda (mask lx ly) (declare (ignore lx ly)) (push mask *got*))
          (lambda () nil)))

(defun menu-titles (port)
  (let ((m (clim-glass::glass-port-menu port)))
    (and m (cons (clim-glass::wm-menu-title m)
                 (mapcar #'car (clim-glass::wm-menu-items m))))))

(defun dismiss (s port)
  (lclick s 700 440)
  (setf (clim-glass::glass-port-menu port) nil))

(let ((vport 5958))
  (sb-thread:make-thread
   (lambda () (handler-case (clim-glass:run-wm '() :port vport :width 720 :height 460
                                                   :menu '(("Terminal" :terminal)))
                (error (e) (format t "~&WM ERROR ~a~%" e)))))
  (let ((s (connect vport)))
    (handshake s)
    (sleep 1.5)
    ;; THE TRAP: find-glass-port defaults to :port 5900 — pass the port explicitly.
    (let* ((port (clim-glass:find-glass-port :port vport))
           (cb (glass:session-clipboard))
           (speech (and (clim-glass::wm-speech-fn "SPEAK") t))
           (surf (clim-glass::add-surface port #'probe-surface
                                          :title "probe" :width 300 :height 200)))
      (ck-that "port found on 5958 (not 5900)"
               (and port (= (clim-glass::glass-port-screen-w port) 720)))
      (ck-that "this image has a voice (else the menus are empty by design)" speech)
      (let* ((cx (clim-glass::wm-surface-x surf)) (cy (clim-glass::wm-surface-y surf))
             (px (+ cx 40)) (py (+ cy 40)))     ; a point inside the surface's CONTENT

        ;; ---- 1. no selection CONCEPT -> right-click reaches the app --------------
        ;; The surface has no selection-fn at all, like a terminal or a file browser.
        (glass:clipboard-clear cb)
        (setf *got* '())
        (rclick s px py)
        (ck "no selection concept: no menu" (menu-titles port) nil)
        (ck-that "no selection concept: app got the press" (member 4 *got*))
        (ck-that "no selection concept: app got the release too" (member 0 *got*))

        ;; ---- 2. a full clipboard is NOT a selection ------------------------------
        ;; The old rule would have opened a menu here.  Nothing about the clipboard can
        ;; give a window a selection it does not have.
        (glass:clipboard-own cb (list :some-window) :text "words from somewhere else"
                                :name "other")
        (setf *got* '())
        (rclick s px py)
        (ck "clipboard full, nothing selected: no menu" (menu-titles port) nil)
        (ck-that "clipboard full, nothing selected: app got button 3" (member 4 *got*))

        ;; ---- 3. a LIVE selection -> the menu, and NO pass-through ----------------
        (setf (clim-glass::wm-surface-selection-fn surf) (lambda () *sel*)
              *sel* "the quick brown fox")
        (setf *got* '())
        (ptr s 4 px py) (sleep 0.35)            ; press only, as the phone sends it
        (ck "live selection: menu opens" (menu-titles port) '("Selection" "Speak selection"))
        (ck "live selection: app got NOTHING" *got* '())
        (ptr s 0 px py) (sleep 0.2)             ; the release the phone sends right after
        (ck "release does not dismiss or fire" (menu-titles port) '("Selection" "Speak selection"))
        (ck "release did not reach the app either" *got* '())
        (let ((m (clim-glass::glass-port-menu port)))
          (ck "menu at the pointer"
              (list (clim-glass::wm-menu-x m) (clim-glass::wm-menu-y m)) (list px py)))
        (dismiss s port)
        (ck "click-outside dismisses" (menu-titles port) nil)

        ;; ---- 4. the rule reads the LIVE text, and follows it ---------------------
        ;; The menu item closes over what the window said at click time, so a selection
        ;; that has been replaced cannot be the one spoken.
        (setf *sel* "A")
        (ck "the rule reads the window, not the clipboard (A)"
            (clim-glass::wm-surface-live-selection surf) "A")
        (setf *sel* "B")                        ; select A, then select B
        (ck "a replaced selection reads as the new one (B)"
            (clim-glass::wm-surface-live-selection surf) "B")
        (glass:clipboard-own cb (list :some-window) :text "STALE" :name "other")
        (ck "and a clipboard saying otherwise changes nothing"
            (clim-glass::wm-surface-live-selection surf) "B")

        ;; ---- 5. DISMISS the selection -> the press goes back to the app -----------
        ;; The case that motivated all of this.  The clipboard still holds "STALE".
        (setf *sel* nil)
        (setf *got* '())
        (rclick s px py)
        (ck "dismissed selection: NO menu" (menu-titles port) nil)
        (ck-that "dismissed selection: app got the press" (member 4 *got*))
        (ck-that "dismissed selection: app got the release" (member 0 *got*))
        (ck-that "the clipboard still holds the old text (it is not the selection)"
                 (equal (glass:clipboard-text cb) "STALE"))
        ;; An empty-string selection is not a selection either.
        (setf *sel* "")
        (setf *got* '())
        (rclick s px py)
        (ck "empty-string selection: no menu" (menu-titles port) nil)
        (ck-that "empty-string selection: app got button 3" (member 4 *got*))
        ;; Nor is a window whose answer signals — it must not be able to eat a press.
        (setf (clim-glass::wm-surface-selection-fn surf) (lambda () (error "busy")))
        (setf *got* '())
        (rclick s px py)
        (ck "a signalling window: no menu" (menu-titles port) nil)
        (ck-that "a signalling window: app got button 3" (member 4 *got*))
        (setf (clim-glass::wm-surface-selection-fn surf) (lambda () *sel*) *sel* "the fox")

        ;; ---- 6. a LEFT click over a live selection still reaches the app ----------
        (setf *got* '())
        (lclick s px py)
        (ck-that "left-click still reaches the app" (member 1 *got*))

        ;; ---- 7. "Stop speaking" appears only while speaking -----------------------
        (ck "quiet: the selection menu is one item"
            (mapcar #'car (clim-glass::wm-selection-menu-items "hi")) '("Speak selection"))
        (ck "quiet: the root menu's clipboard half is one item"
            (mapcar #'car (clim-glass::wm-clipboard-menu-items)) '("Speak clipboard"))

        ;; ---- 8. the ROOT menu speaks the CLIPBOARD --------------------------------
        ;; Reachable from the bare workspace, where there is no window to ask, and there
        ;; whether or not anything is selected anywhere.
        (rclick s 700 20)                        ; the wallpaper, clear of every window
        (ck "root menu ends with Speak clipboard"
            (last (menu-titles port)) '("Speak clipboard"))
        (ck "root menu still launches things" (second (menu-titles port)) "Terminal")
        (dismiss s port)
        (setf *sel* nil)                         ; nothing selected anywhere at all
        (rclick s 700 20)
        (ck "and it is there with nothing selected"
            (last (menu-titles port)) '("Speak clipboard"))
        (dismiss s port)
        ;; Asking "is it speaking?" the lazy way would CREATE the session speaker —
        ;; a thread and a silent source on the mix — every time a menu opened.
        (ck "opening the menus did not conjure a voice out of nothing"
            (and (symbol-value (find-symbol "*SESSION-SPEAKER*" '#:glass)) t) nil)

        ;; ---- 9. clamped when opened hard against the bottom-right corner ----------
        (let ((edge (clim-glass::add-surface port #'probe-surface
                                             :title "edge" :width 260 :height 160)))
          (setf (clim-glass::wm-surface-selection-fn edge) (lambda () "over here")
                (clim-glass::wm-surface-x edge) 430 (clim-glass::wm-surface-y edge) 290)
          (sleep 0.4)
          (let ((ex 640) (ey 400))              ; inside the content, clear of the resize grab
            (ck "the click is on content, not the resize corner"
                (nth-value 1 (clim-glass::wm-hit port ex ey)) :content)
            (ptr s 4 ex ey) (sleep 0.4)
            (let ((m (clim-glass::glass-port-menu port)))
              (ck-that "menu opens over the second selecting window" m)
              (when m
                (ck-that "clamped inside the screen"
                         (and (<= (+ (clim-glass::wm-menu-x m)
                                     (glass:fb-width (clim-glass::wm-menu-fb m))) 720)
                              (<= (+ (clim-glass::wm-menu-y m)
                                     (glass:fb-height (clim-glass::wm-menu-fb m))) 460)))
                (ck-that "the clamp actually moved it left" (< (clim-glass::wm-menu-x m) ex))))
            (ptr s 0 ex ey) (sleep 0.1)))))
    (ignore-errors (close s))))

(format t "~&==== ~[ALL PASS~:;~:*~d FAIL~]~%" *fails*)
(finish-output)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
