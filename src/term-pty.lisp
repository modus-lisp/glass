;;;; term-pty.lisp — the pty half of glass/term: what a terminal over a real pseudo-terminal
;;;; needs and a terminal over anything else does not.  The window size (TIOCSWINSZ) and echo
;;;; (termios), through SB-POSIX and SB-ALIEN; term.lisp calls them through
;;;; *PTY-SET-WINSIZE* and *PTY-ENABLE-ECHO*, which this sets.  A Lisp with no pty -- modus on
;;;; bare metal, whose SB-POSIX has no termios -- builds term.lisp without this file.

(in-package #:glass-term)

(defconstant +tiocswinsz+
  #+darwin #x80087467                   ; BSD encodes direction and size in the request
  #-darwin #x5414                       ; Linux numbers its tty ioctls sequentially
  "TIOCSWINSZ, which is not the same number on every kernel.

   Linux numbers its tty ioctls sequentially; BSD encodes the direction and the
   argument size into the request, so the same operation is #x5414 there and
   #x80087467 here.  The Linux value used to be hard-coded, so on macOS this failed —
   silently, inside IGNORE-ERRORS — and every program in the terminal was told nothing
   about the window size.  A shell copes; anything that draws does not.

   Fixing the number was not sufficient on its own: see SET-WINSIZE for the variadic
   calling convention that was failing underneath it.")

(defun set-winsize (stream rows cols)
  "TIOCSWINSZ on the pty master so programs know the size (best-effort)."
  (ignore-errors
   (sb-alien:with-alien ((ws (sb-alien:array sb-alien:unsigned-short 4)))
     (setf (sb-alien:deref ws 0) rows (sb-alien:deref ws 1) cols
           (sb-alien:deref ws 2) 0 (sb-alien:deref ws 3) 0)
     ;; THROUGH SB-POSIX, not a bare EXTERN-ALIEN.  ioctl(2) is variadic, and on
     ;; arm64 Darwin variadic arguments are passed on the STACK while fixed ones go in
     ;; registers — so declaring it as a plain three-argument function hands the
     ;; pointer to a register ioctl never reads, and every call returns -1.  Measured:
     ;; the same call is 0 through SB-POSIX and -1 through EXTERN-ALIEN, on the same
     ;; fd, with either kernel's request number.  SBCL provides a non-variadic entry
     ;; point precisely for this.
     (sb-posix::ioctl-with-pointer-arg
      (sb-sys:fd-stream-fd stream) +tiocswinsz+ (sb-alien:addr ws)))))

(defun enable-echo (stream)
  "Turn ECHO on for the pty.

   SBCL's :pty t leaves the line discipline in -echo, so a shell reading in canonical
   mode never echoes your keystrokes: you type blind, the command runs, and the command
   line never appears.

   THROUGH SB-POSIX, not a raw ioctl.  This used to poke TCGETS/TCSETS (0x5401/0x5402)
   and set bit 3 of byte 12, which is Linux's request numbering and Linux's struct
   termios — where c_lflag is a 4-byte field at offset 12.  On Darwin the requests are
   TIOCGETA/TIOCSETA (0x40487413/0x40487414), tcflag_t is EIGHT bytes, and c_lflag is
   at offset 24.  The ioctl therefore failed on macOS, inside IGNORE-ERRORS, and the
   terminal had no local echo with nothing said about why.  The docstring admitted it
   was Linux-only; the caller did not, and neither did the behaviour.

   SB-POSIX already knows all of that per platform.  There was never a reason to
   restate it here, and restating it is what made it wrong in one place."
  (ignore-errors
    (let* ((fd (sb-sys:fd-stream-fd stream))
           (tio (sb-posix:tcgetattr fd)))
      (setf (sb-posix:termios-lflag tio)
            (logior (sb-posix:termios-lflag tio) sb-posix:echo))
      (sb-posix:tcsetattr fd sb-posix:tcsanow tio)
      t)))

(setf *pty-set-winsize* #'set-winsize
      *pty-enable-echo* #'enable-echo)
