;;;; term-gallery.lisp — a font/rendering gallery for the glass terminal.  Feeds a
;;;; showcase straight to the VT parser (no shell) — JetBrains Mono text + operators,
;;;; ANSI/256/truecolour, box-drawing + blocks, Symbols Nerd Font icons, a powerline
;;;; prompt, and Twemoji colour emoji — renders it, and dumps a PNG.
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load inspect/term-gallery.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass/term :glass/text :zpng))))
(in-package :glass-term)

(defparameter *e* (string (code-char 27)))     ; ESC
(defun cp (x) (string (code-char x)))          ; a codepoint as a 1-char string (Nerd PUA / powerline)
(defun feed-string (tm s)
  (loop for b across (sb-ext:string-to-octets s :external-format :utf-8) do (feed-byte tm b)))

(defun save-fb-png (fb path)
  (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (px (glass:fb-pixels fb))
         (png (make-instance 'zpng:png :width w :height h :color-type :truecolor)) (d (zpng:data-array png)))
    (dotimes (y h) (dotimes (x w)
      (let ((v (aref px (+ (* y w) x))))
        (setf (aref d y x 0) (ldb (byte 8 16) v) (aref d y x 1) (ldb (byte 8 8) v) (aref d y x 2) (ldb (byte 8 0) v)))))
    (zpng:write-png png path)))

(let* ((tm (make-terminal :cols 100 :rows 32 :ppem 16)) (e *e*))
  (flet ((out (s) (feed-string tm s))
         ;; CR+LF: fed straight to the parser there's no tty ONLCR, so a bare LF
         ;; moves down but keeps the column (a rightward staircase) — emit CR too.
         (nl () (feed-string tm (format nil "~a[0m~C~C" *e* #\Return #\Linefeed))))
    (macrolet ((line (&rest parts) `(progn ,@(loop for p in parts collect `(out ,p)) (nl))))
      ;; ---- title ----
      (line (format nil "~a[1;38;5;81m  glass terminal  ~a[0;38;5;245m· JetBrains Mono · Symbols Nerd Font · Twemoji" e e))
      (line "")
      ;; ---- text + programming operators ----
      (line (format nil "~a[38;5;252mABCDEFGHIJKLMNOPQRSTUVWXYZ  abcdefghijklmnopqrstuvwxyz  0123456789" e))
      (line (format nil "~a[38;5;252mThe quick brown fox   -> => != >= <= == === |> <| :: -- ++ /* */ && ||  #[]{}()" e))
      (line (format nil "~a[1;38;5;255mbold  ~a[0;3;38;5;252mitalic  ~a[0;4;38;5;252munderline  ~a[0;38;5;250mregular" e e e e))
      (line "")
      ;; ---- ANSI 16 ----
      (out (format nil "~a[38;5;245m  ANSI 16   " e))
      (dotimes (i 8) (out (format nil "~a[4~dm  " e i)))
      (out "  ")
      (dotimes (i 8) (out (format nil "~a[10~dm  " e i)))
      (nl)
      ;; ---- 256 colour cube ----
      (out (format nil "~a[38;5;245m  256 cube  " e))
      (loop for i from 16 to 231 by 3 do (out (format nil "~a[48;5;~dm " e i)))
      (nl)
      ;; ---- truecolour ----
      (out (format nil "~a[38;5;245m  truecolor " e))
      (dotimes (i 72) (let ((r (round (* 255 (/ i 71.0)))) (g (round (* 255 (- 1 (/ i 71.0))))))
                        (out (format nil "~a[48;2;~d;~d;140m " e r g))))
      (nl) (line "")
      ;; ---- box drawing + blocks ----
      (line (format nil "~a[38;5;250m  box  ┌───┬───┐   ╔═══╦═══╗    shades ░▒▓█   bars ▁▂▃▄▅▆▇█   ● ◐ ○  ■ □  ◆ ◇  ★ ☆" e))
      (line (format nil "~a[38;5;250m       │ a │ b │   ║ x ║ y ║    arrows ← ↑ → ↓  ⟶  ✓ ✗   µΩ λ ∑ ∆ π √ ∞ ≈ ≠ ≤ ≥" e))
      (line (format nil "~a[38;5;250m       └───┴───┘   ╚═══╩═══╝" e))
      (line "")
      ;; ---- Nerd Font icons ----
      (out (format nil "~a[38;5;252m  Nerd " e))
      (dolist (icon '(#xf07b #xf07c #xf015 #xf013 #xf02b #xf0e7 #xf023 #xf121 #xf1d3 #xe718 #xe73c #xe7a8 #xf303 #xe712 #xf02d #xf1eb))
        (out (format nil " ~a[38;5;110m~a" e (cp icon))))
      (nl) (line "")
      ;; ---- powerline prompt ----
      (out (format nil "~a[38;5;245m  prompt " e))
      (out (format nil "~a[48;5;114;38;5;236m  main ~a[48;5;238;38;5;114m~a" e e (cp #xe0b0)))
      (out (format nil "~a[38;5;250m ~a ~~/glass ~a[48;5;240;38;5;238m~a" e (cp #xf07b) e (cp #xe0b0)))
      (out (format nil "~a[38;5;250m ~a  ~a[0;38;5;240m~a~a[0m" e (cp #xf015) e (cp #xe0b0) e))
      (nl) (line "")
      ;; ---- colour emoji (Twemoji) ----
      (line (format nil "~a[38;5;252m  emoji  😀 🎉 🚀 👍 🔥 ✨ 💡 🐧 🦀 🎨 ⭐ 🌙 ⚡ 🧪 📦 🎯 💾 🔒" e))))
  (render tm)
  (save-fb-png (terminal-fb tm) "/tmp/term-gallery.png")
  (format t "~&wrote /tmp/term-gallery.png (~dx~d)~%"
          (glass:fb-width (terminal-fb tm)) (glass:fb-height (terminal-fb tm)))
  (kill-terminal tm))
(finish-output) (sb-ext:exit)
