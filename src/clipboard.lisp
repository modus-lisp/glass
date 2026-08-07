;;;; src/clipboard.lisp — the desktop's selection, held once and read by whoever is pasting.
;;;;
;;;; glass exports a screen, and a mix of everything the session is playing.  This is the third
;;;; thing a session has exactly one of: what was last copied.
;;;;
;;;; The shape follows from the same fact the mixer's does — a session has SEVERAL possible
;;;; transports.  A VNC client carries the selection in ClientCutText/ServerCutText, a WebRTC
;;;; data channel would carry it as a message, a local app has it in a variable.  Put the
;;;; clipboard inside a transport and only that transport's clients can paste; the next transport
;;;; grows a second one, and the session then has two clipboards that disagree about what "the"
;;;; selection is.  So the clipboard lives here, beside the framebuffer, and a transport is a thin
;;;; thing that converts.
;;;;
;;;; Where it is NOT the mixer, which matters more than where it is:
;;;;
;;;; IT IS DISCRETE, SO THERE IS NO CLOCK.  Audio has to be produced whether anyone is listening
;;;; or not, because a gap in it is audible; the selection is one value that changes when somebody
;;;; copies and at no other time.  Nothing here runs a thread.  A consumer does not hold a cursor
;;;; into a ring of recent values either — a paste wants the CURRENT selection, never the one from
;;;; 400 ms ago — so instead of a pull cursor, consumers get change NOTIFICATION: a listener is
;;;; called when the selection changes, and reads the value it is now.
;;;;
;;;; NOTHING IS MIXED.  Two sources of audio sum, and both are heard.  Two writers of a clipboard
;;;; do not sum: the second one wins entirely.  That makes "who wrote it" a real part of the
;;;; state and not a decoration, so the model here is X11's — OWNERSHIP.  The clipboard holds an
;;;; owner and the owner's content, and it buys three things that a bare string does not:
;;;;
;;;;   - A late reader can be answered by the owner.  Content may be a PROVIDER thunk instead of
;;;;     a string, so an app that copies a 40 MB buffer declares the selection now and serializes
;;;;     it only if somebody actually pastes.
;;;;   - "The selection went away when that app closed" is expressible.  CLIPBOARD-DISOWN clears
;;;;     the selection only if the caller still holds it, which is exactly the guard a window's
;;;;     teardown needs: an app that copied, lost the selection to somebody else, and then quit
;;;;     must not wipe the selection it no longer owns.
;;;;   - A transport can tell its own echo apart.  The RFB sender skips a client that is itself
;;;;     the owner, so a paste does not bounce back to the client that just sent it.
;;;;
;;;; What is deliberately NOT taken from X11: targets/format negotiation, a separate PRIMARY and
;;;; CLIPBOARD selection, and incremental (INCR) transfer.  There is one selection and one format,
;;;; text, because that is what every transport we have can carry.  Ownership without targets is
;;;; the part that pays for itself today.
;;;;
;;;; TEXT IS LATIN-1 ON THE WIRE.  Not a policy of ours — RFC 6143 §7.5.6 and §7.6.4 define
;;;; ClientCutText and ServerCutText as Latin-1, with line endings as a single LF.  UTF-8 needs
;;;; the extended-clipboard pseudo-encoding (-1063), which is a different message with its own
;;;; capability handshake and a zlib-compressed payload.  This file does Latin-1 completely and
;;;; correctly and leaves the extension for later, rather than half-doing both: a clipboard that
;;;; is right for eight-bit text and honest about the rest is more useful than one that mangles
;;;; both.  The conversion lives here, not in the transport, because it is the same conversion
;;;; every eight-bit transport needs.

(in-package #:glass)

;;; ---- the clipboard ---------------------------------------------------------
;;;
;;; The lock is feature-gated the way the framebuffer's is, so the same file loads on an
;;; implementation without sb-thread (modus, which supplies its own concurrency).

#+sb-thread (defun %clip-make-lock () (sb-thread:make-mutex :name "glass-clipboard"))
#-sb-thread (defun %clip-make-lock () nil)

#+sb-thread (defmacro %with-clip-locked ((cb) &body body)
              `(sb-thread:with-mutex ((clipboard-lock ,cb)) ,@body))
#-sb-thread (defmacro %with-clip-locked ((cb) &body body) `(progn ,cb ,@body))

(define-record (clipboard (:constructor %make-clipboard))
  (lock (%clip-make-lock))
  (owner nil)                       ; opaque identity of the holder, or NIL = nobody holds it
  (owner-name "" :type string)      ; a label for a report — the owner itself may be anything
  (content nil)                     ; a string, a provider thunk, or NIL (no selection)
  (cached nil)                      ; last provider result, memoized per SERIAL
  (cached-serial -1 :type fixnum)
  (serial 0 :type fixnum)           ; bumped on every real change; 0 = never set
  (stamp 0)                         ; universal time of the last change
  (listeners '())                   ; (key . function), called on change
  (writes 0 :type fixnum)
  (reads 0 :type fixnum))

(defun make-clipboard ()
  "An empty clipboard: no owner, no selection, serial 0."
  (%make-clipboard))

;;; ---- change notification ---------------------------------------------------
;;;
;;; A listener is called AFTER the change, with the lock released.  Both halves matter: a
;;; listener that reads the clipboard back (the RFB sender does) would deadlock on a recursive
;;; read under the writer's lock, and a listener that is slow (a socket write) would hold every
;;; other writer off for as long as it takes.  A listener that signals is reported and skipped —
;;; one broken consumer must not make the session's copy/paste fail for the others.

(defun clipboard-listen (cb key fn)
  "Call FN with (CLIPBOARD SERIAL OWNER) whenever the selection changes.  KEY identifies the
listener for removal (an RFB client uses itself), and re-registering the same KEY replaces it —
so a reconnecting consumer cannot accumulate duplicates."
  (%with-clip-locked (cb)
    (setf (clipboard-listeners cb)
          (cons (cons key fn) (remove key (clipboard-listeners cb) :key #'car))))
  key)

(defun clipboard-unlisten (cb key)
  "Drop the listener registered under KEY.  Returns T if it was there."
  (%with-clip-locked (cb)
    (let ((hit (assoc key (clipboard-listeners cb))))
      (when hit
        (setf (clipboard-listeners cb) (remove hit (clipboard-listeners cb)))
        t))))

(defun %clipboard-notify (cb serial owner listeners)
  (dolist (l listeners)
    (handler-case (funcall (cdr l) cb serial owner)
      (serious-condition (e)
        (ignore-errors
         (format *error-output* "~&glass clipboard: listener ~a failed: ~a~%" (car l) e)
         (force-output *error-output*))))))

;;; ---- taking the selection --------------------------------------------------

(defun clipboard-own (cb owner &key text provider (name nil))
  "OWNER takes the selection, offering TEXT (a string) or PROVIDER (a thunk returning one).

Returns the new serial, or the unchanged serial if this was not a change.  A repeat of the
selection that is already there — the SAME literal text — updates the owner but does NOT bump
the serial or notify, which is what keeps two connected VNC clients from ping-ponging: A sends
its clipboard, we hand it to B, B's viewer sets its local clipboard and sends it back, and
without this guard that is an infinite round trip of identical text.  A PROVIDER cannot be
compared without calling it, so it always counts as a change."
  (when (and text provider)
    (error "glass clipboard: give TEXT or PROVIDER, not both"))
  (let (serial listeners (changed nil))
    (%with-clip-locked (cb)
      (let ((new (or provider text)))
        (setf changed (not (and (stringp new) (stringp (clipboard-content cb))
                                (string= new (clipboard-content cb)))))
        (setf (clipboard-owner cb) owner
              (clipboard-owner-name cb) (or name (clipboard-owner-name cb) "")
              (clipboard-content cb) new)
        (when changed
          (incf (clipboard-serial cb))
          (incf (clipboard-writes cb))
          (setf (clipboard-stamp cb) (get-universal-time)
                (clipboard-cached cb) nil
                (clipboard-cached-serial cb) -1))
        (setf serial (clipboard-serial cb)
              listeners (copy-list (clipboard-listeners cb)))))
    (when changed (%clipboard-notify cb serial owner listeners))
    serial))

(defun clipboard-set (cb text &key (owner :local) (name "local"))
  "Put TEXT on the clipboard.  The one-liner over CLIPBOARD-OWN, for a caller that has the
string in hand and no identity worth naming."
  (clipboard-own cb owner :text text :name name))

(defun clipboard-disown (cb owner)
  "OWNER gives up the selection.  A no-op unless OWNER still holds it — an app that copied,
then lost the selection to somebody else, must not wipe the current selection on its way out.
Returns T if the selection was actually cleared.

This clears the SESSION's selection; it does not tell a connected VNC client to clear its own.
RFB has no way to say \"there is no selection\" (an empty ServerCutText says \"the selection is
the empty string\", which would destroy the user's local clipboard), so a disown is silence on
the wire."
  (let (serial listeners (cleared nil))
    (%with-clip-locked (cb)
      (when (and (clipboard-owner cb) (eq owner (clipboard-owner cb)))
        (setf (clipboard-owner cb) nil (clipboard-owner-name cb) ""
              (clipboard-content cb) nil (clipboard-cached cb) nil
              (clipboard-cached-serial cb) -1
              (clipboard-stamp cb) (get-universal-time)
              cleared t)
        (incf (clipboard-serial cb)))
      (setf serial (clipboard-serial cb)
            listeners (copy-list (clipboard-listeners cb))))
    (when cleared (%clipboard-notify cb serial nil listeners))
    cleared))

(defun clipboard-clear (cb)
  "Drop the selection whoever owns it."
  (clipboard-disown cb (clipboard-owner cb)))

;;; ---- reading it ------------------------------------------------------------

(defun clipboard-text (cb)
  "The current selection as a string, or NIL if there is none.

Returns (values TEXT SERIAL OWNER).  SERIAL is what a consumer remembers to know whether it has
already seen this value; OWNER lets a transport recognise its own writes and not echo them back.

A provider is called OUTSIDE the lock (it is app code and may be slow, and it may well want to
read the clipboard itself) and memoized against the serial, so ten clients pasting one selection
call the owner once."
  (let (content serial owner cached)
    (%with-clip-locked (cb)
      (setf content (clipboard-content cb) serial (clipboard-serial cb)
            owner (clipboard-owner cb))
      (when (= (clipboard-cached-serial cb) serial) (setf cached (clipboard-cached cb)))
      (incf (clipboard-reads cb)))
    (cond
      ((null content) (values nil serial owner))
      ((stringp content) (values content serial owner))
      (cached (values cached serial owner))
      (t (let ((text (handler-case (funcall content)
                       (serious-condition (e)
                         (ignore-errors
                          (format *error-output* "~&glass clipboard: provider failed: ~a~%" e))
                         nil))))
           (when (stringp text)
             (%with-clip-locked (cb)
               ;; only memoize if the selection has not moved on under us
               (when (= (clipboard-serial cb) serial)
                 (setf (clipboard-cached cb) text (clipboard-cached-serial cb) serial))))
           (values (and (stringp text) text) serial owner))))))

(defun clipboard-report (cb)
  "A line about the selection, for a control socket."
  (multiple-value-bind (text serial owner) (clipboard-text cb)
    (declare (ignore owner))
    (%with-clip-locked (cb)
      (format nil "clipboard serial=~d owner=~a~@[ (~a)~] chars=~a writes=~d reads=~d listeners=~d"
              serial (if (clipboard-owner cb) (clipboard-owner-name cb) "none")
              (and (clipboard-content cb) (not (stringp (clipboard-content cb))) "provider")
              (if text (length text) "-")
              (clipboard-writes cb) (clipboard-reads cb)
              (length (clipboard-listeners cb))))))

;;; ---- the session's clipboard -----------------------------------------------
;;;
;;; A session has ONE selection, so the process running the desktop has one clipboard, on the
;;; same argument as *SESSION-MIXER*: the things that want to reach it — a transport, a control
;;; socket eval, an app that copies — should not each have to be handed it.  Created on first
;;; use, and a convenience rather than a constraint: MAKE-CLIPBOARD still makes as many
;;; independent clipboards as a caller wants, and a test binds *SESSION-CLIPBOARD* to one.

(defvar *session-clipboard* nil
  "The clipboard for this process's session, or NIL before anything asked for one.")

(defvar *session-clipboard-lock* (%clip-make-lock))

(defun session-clipboard ()
  "This process's session clipboard, creating it on first call.  Idempotent — two transports
racing at startup get the same selection, not one each."
  #+sb-thread
  (sb-thread:with-mutex (*session-clipboard-lock*)
    (or *session-clipboard* (setf *session-clipboard* (make-clipboard))))
  #-sb-thread
  (or *session-clipboard* (setf *session-clipboard* (make-clipboard))))

;;; ---- Latin-1, the eight-bit wire format ------------------------------------

(defparameter *latin1-substitute* #\?
  "What a character outside Latin-1 becomes on the way out.  Substituting rather than dropping
keeps the length and the shape of the text — a paste that silently loses characters looks like
the clipboard worked.")

(defparameter *max-cut-text* (* 8 1024 1024)
  "Longest cut text we will hold from a client, in bytes.  A length field is 32 bits, so a
buggy or hostile client can announce 4 GB; the bytes still have to be consumed to stay in sync
with the stream, but they do not have to be kept.")

(defun clipboard-normalize-newlines (text)
  "CRLF and lone CR become LF.  RFC 6143 §7.5.6: cut text uses a single LF for a line ending,
so a Windows client's clipboard has to be converted BOTH ways — an unconverted CRLF pasted into
a terminal submits every line twice."
  (if (find #\Return text)
      (with-output-to-string (out)
        (let ((n (length text)))
          (dotimes (i n)
            (let ((c (char text i)))
              (cond ((char/= c #\Return) (write-char c out))
                    ((and (< (1+ i) n) (char= (char text (1+ i)) #\Newline)))  ; CRLF: drop the CR
                    (t (write-char #\Newline out)))))))                        ; lone CR: becomes LF
      text))

(defun latin1-bytes (text)
  "TEXT as Latin-1 bytes with LF line endings — the ServerCutText payload.  Characters above
U+00FF become *LATIN1-SUBSTITUTE*; that is the protocol's limit, not a shortcut."
  (let* ((s (clipboard-normalize-newlines text))
         (out (make-array (length s) :element-type '(unsigned-byte 8))))
    (dotimes (i (length s) out)
      (let ((code (char-code (char s i))))
        (setf (aref out i) (if (<= code #xff) code (char-code *latin1-substitute*)))))))

(defun latin1-string (bytes &key (start 0) (end (length bytes)))
  "Latin-1 BYTES as a string, newlines normalized.  Every byte 0-255 is a character, so this
cannot fail — which is the one nice thing about Latin-1."
  (clipboard-normalize-newlines
   (let ((s (make-string (- end start))))
     (dotimes (i (- end start) s)
       (setf (char s i) (code-char (aref bytes (+ start i))))))))

;;; ---- the fallback consumer: paste as keystrokes ----------------------------
;;;
;;; The clipboard above is the right structure and it is useless on its own, because on this
;;; desktop NOTHING READS IT YET.  There is no toolkit-wide "paste" verb: the terminal writes to
;;; a pty, loom's address bar keeps its own edit buffer, a CLIM pane wants a CLIM event — and VNC
;;; has no notion of "the focused text field" that a server could target.
;;;
;;; So, until apps opt in, paste is delivered the one way every app already understands: as
;;; keystrokes.  The text is synthesized into KeyEvents and pushed through the SAME on-key
;;; callback a real keypress from a real client takes, which means it inherits the whole routing
;;; for free — the WM's focused-surface rule, the terminal's pty write, loom's editing state, the
;;; CLIM event queue.  Nothing anywhere needs to know a paste happened.
;;;
;;; This is a FALLBACK, and it is worth being precise about what it cannot do, because an app
;;; that later reads the clipboard properly will do better on all of it:
;;;
;;;   - It types.  An app that transforms keystrokes transforms the paste: a terminal in raw mode
;;;     (vi, less, a TUI) reads pasted text as commands, exactly as if the user had typed it, and
;;;     a shell's own bracketed-paste protection cannot help because from the pty's side this IS
;;;     typing.  Pasting into a shell prompt or a text field works; pasting into vi's normal mode
;;;     does what those characters mean in vi.
;;;   - It carries only what a keysym carries.  Latin-1 printables, LF as Return, TAB.  Anything
;;;     else — a control character, an emoji — is dropped rather than typed as garbage.
;;;   - It is one-way.  Nothing here copies OUT of an app; that needs the app to declare the
;;;     selection (CLIPBOARD-OWN), which is the opt-in this exists to avoid blocking on.
;;;   - It is not atomic.  Keys arrive one at a time on a real event path, so an app that
;;;     re-renders per keystroke re-renders per character, and a focus change halfway through a
;;;     paste splits it between two windows.

(defvar *key-injector* nil
  "Function (DOWN-P KEYSYM) that delivers a key event to whatever currently has focus.
SERVE installs its own :ON-KEY callback here, so an injected key takes the identical path a
client's keypress takes.  NIL = nothing to type into (no server running).")

(defparameter *paste-key-delay* 0.004
  "Seconds between injected keys.  Not politeness: a pty has a finite buffer and an app that
repaints per keystroke has a finite appetite, and a thousand characters delivered in one burst
is how you find both limits at once.  ~250 chars/s types a URL in under a fifth of a second.")

(defparameter *paste-max-chars* 4096
  "Longest paste that will be typed.  Key injection is O(n) event dispatches through the whole
desktop; a megabyte of clipboard would type for over an hour.  Truncated, and said so.")

(defun paste-keysyms (text)
  "TEXT as the X keysyms a client would have sent to type it.  Pure — no injection, so a test
can check the translation without a desktop.  Characters with no keysym are dropped."
  (let ((out '()))
    (loop for c across (clipboard-normalize-newlines text)
          for code = (char-code c)
          do (cond ((char= c #\Newline) (push #xff0d out))     ; Return
                   ((char= c #\Tab) (push #xff09 out))         ; Tab
                   ;; Latin-1 printables are their own keysym (X11 keysymdef: keysym = codepoint
                   ;; for U+0020..U+00FF, with the C1 range unassigned).
                   ((or (<= #x20 code #x7e) (<= #xa0 code #xff)) (push code out))))
    (nreverse out)))

(defvar *paste-lock* (%clip-make-lock)
  "Serializes pastes.  Two pastes at once would interleave character by character into the same
focused window, which is worse than either paste alone.")

(defun %type-keys (inject keys delay)
  (dolist (k keys)
    ;; down THEN up, always both: an app that latches on key-down and clears on key-up (loom
    ;; latches shift this way) is left holding a key forever by a down without its up.
    (handler-case (progn (funcall inject t k) (funcall inject nil k))
      (serious-condition (e)
        (ignore-errors (format *error-output* "~&glass clipboard: key injection failed: ~a~%" e))
        (return)))
    (when (and delay (plusp delay)) (sleep delay))))

(defun paste-text-as-keys (text &key (injector *key-injector*) (delay *paste-key-delay*))
  "Type TEXT into whatever has focus, synchronously.  Returns the number of keys sent.

The caller is normally PASTE-TEXT, which runs this off the calling thread; call it directly only
when you want to block until the text has been typed (a test does)."
  (let ((inject (or injector *key-injector*)))
    (unless inject (return-from paste-text-as-keys 0))
    (let* ((keys (paste-keysyms text))
           (n (length keys)))
      (when (> n *paste-max-chars*)
        (format *error-output* "~&glass clipboard: paste truncated, ~d of ~d keys~%"
                *paste-max-chars* n)
        (setf keys (subseq keys 0 *paste-max-chars*) n *paste-max-chars*))
      #+sb-thread (sb-thread:with-mutex (*paste-lock*) (%type-keys inject keys delay))
      #-sb-thread (%type-keys inject keys delay)
      n)))

(defun paste-text (text &key (injector *key-injector*) (delay *paste-key-delay*) wait)
  "Type TEXT into the focused window.  Asynchronous by default: the caller is usually an RFB
reader thread, which must go back to reading client messages immediately — a paste that typed on
that thread would freeze the pointer and the keyboard for its whole duration.  WAIT t types on
the calling thread instead."
  (cond
    ((or wait #-sb-thread t) (paste-text-as-keys text :injector injector :delay delay))
    (t #+sb-thread
       (progn (sb-thread:make-thread
               (lambda () (ignore-errors (paste-text-as-keys text :injector injector :delay delay)))
               :name "glass-paste")
              t))))

(defun clipboard-paste (&key (clipboard (session-clipboard)) wait)
  "Paste the session selection into the focused window, by typing it.  This is the fallback
consumer described above — the explicit, documented one, so that an app which later reads the
clipboard properly simply stops going through here.

Returns NIL if there is no selection or nothing to type into."
  (let ((text (clipboard-text clipboard)))
    (when (and text (plusp (length text)))
      (paste-text text :wait wait))))
