;;;; packages.lisp — glass

(defpackage #:glass
  (:use #:cl)
  (:documentation
   "glass — a framebuffer and a from-scratch VNC/RFB server in pure Common Lisp.
    To glass is to see at a distance; a VNC server does exactly that — it exports a
    framebuffer so a remote client can view and drive it.  Draw into an in-memory
    FRAMEBUFFER with simple primitives, then SERVE it over RFB (RFC 6143) to any
    VNC client.  Clean-room, no FFI; sb-bsd-sockets is the only platform seam.
    Meant to give modus a display; grown and tested on SBCL first.")
  (:export
   ;; framebuffer
   #:make-framebuffer #:framebuffer #:framebuffer-p
   #:fb-width #:fb-height #:fb-pixels #:fb-resize #:with-fb-locked #:fb-generation #:fb-touch
   #:fb-clip #:with-fb-clip #:fb-frameno #:fb-damage #:fb-copy #:fb-mark-frame #:fb-take-frame #:fb-take-copy
   #:fb-put #:fb-get #:fb-fill #:fb-rect #:fb-hline #:fb-vline #:fb-frame #:fb-blit #:fb-move-rect
   #:rgb #:+black+ #:+white+ #:+red+ #:+green+ #:+blue+
   ;; text (the :glass/text system; scribe-backed)
   #:fb-text #:text-width #:load-font #:default-font
   ;; server: (serve fb port &key on-key on-pointer on-resize name once wake)
   #:serve #:serve-one #:*desktop-name* #:tcp-listen #:make-wake #:wake-signal
   ;; VNC authentication (from-scratch DES); *vnc-password* nil = open, string = required
   #:*vnc-password* #:*legacy-vnc-auth* #:vnc-auth-response #:vnc-auth-verify
   ;; clipboard (the :glass/clipboard system) — one session selection, many transports
   #:make-clipboard #:clipboard #:clipboard-p #:clipboard-own #:clipboard-set #:clipboard-text
   #:clipboard-disown #:clipboard-clear #:clipboard-owner #:clipboard-owner-name
   #:clipboard-serial #:clipboard-stamp #:clipboard-report
   #:clipboard-listen #:clipboard-unlisten
   #:session-clipboard #:*session-clipboard*
   #:latin1-bytes #:latin1-string #:clipboard-normalize-newlines
   #:*latin1-substitute* #:*max-cut-text*
   ;; paste's fallback consumer: type the selection into whatever has focus
   #:clipboard-paste #:paste-text #:paste-text-as-keys #:paste-keysyms
   #:*key-injector* #:*paste-key-delay* #:*paste-max-chars* #:*paste-chord*
   ;; RFB cut text (the transport half, in :glass)
   #:send-cut-text #:read-client-cut-text
   ;; audio (the :glass/audio system; reed-backed) — one session mix, many listeners
   #:make-mixer #:mixer #:mixer-p #:mixer-start #:mixer-stop #:mixer-tick #:mixer-report
   #:mixer-rate #:mixer-frame-samples #:mixer-seq #:mixer-level #:mixer-late #:mixer-running
   #:mixer-add-source #:mixer-remove-source #:mixer-sources #:mixer-play #:audio-tone
   #:mixer-source #:mixer-source-p #:src-id #:src-name #:src-gain #:src-frames
   #:mixer-subscribe #:mixer-unsubscribe #:sink #:sink-p #:sink-next-frame #:sink-source
   #:sink-rate #:sink-frames #:sink-drops #:sink-underruns #:sink-gain
   #:*mixer-rate* #:*mixer-frame-ms* #:session-mixer #:*session-mixer* #:mixer-add-file
   ;; the mix over a socket (the :glass/audio-stream system) — server and listener
   #:start-audio-stream #:stop-audio-stream #:audio-stream #:audio-stream-p #:audio-stream-report
   #:audio-stream-port #:audio-stream-mixer #:*audio-stream-port* #:start-session-audio
   #:make-audio-tap #:audio-tap #:audio-tap-p #:tap-next-frame #:tap-source #:tap-stop
   #:tap-report #:audio-tap-connected #:audio-tap-rate #:audio-tap-frames #:audio-tap-drops
   #:audio-tap-underruns #:audio-tap-reconnects
   ;; the voice (the :glass/speech system; quill-backed) — one speaker on the session mix
   #:speak #:hush #:speaking-p #:make-speaker #:session-speaker #:stop-speaker #:speech-report
   #:speaker #:speaker-p #:*session-speaker* #:*speech-voice* #:*speech-gain* #:*speech-gap-ms*
   ;; standing perf counters (read a snapshot over the control socket)
   #:*perf-on* #:perf-reset #:perf-report #:perf-record-send #:perf-record-composite
   #:*send-lag* #:*send-queue*))
