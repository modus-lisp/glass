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
   #:serve #:serve-one #:*desktop-name* #:tcp-listen #:close-listener #:make-wake #:wake-signal
   ;; transports (src/socket.lisp) — a port anybody on the box can reach, or a socket file
   ;; only its owner can open.  Siblings: everything above them is a stream protocol.
   #:listener #:tcp-listener #:unix-listener #:open-listener #:unix-listen
   #:listener-kind #:listener-endpoint #:listener-open-p #:listener-socket #:listener-path
   #:listener-port #:listener-address #:listener-mode #:listener-peer-policy #:listener-refused
   #:listener-accept #:accept-stream
   #:runtime-dir #:socket-path #:*runtime-dir* #:*socket-file-mode* #:*socket-dir-mode*
   #:clear-stale-socket #:unix-socket-live-p
   #:peer-credentials #:peer-allowed-p #:peer-name #:socket-fd #:*peer-policy*
   #:open-connection #:parse-endpoint #:endpoint-string #:socket-unsent-bytes
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
   ;; audio (the :glass/audio system; reed-backed) — one bus of sources, a mix per listener
   #:make-mixer #:mixer #:mixer-p #:mixer-start #:mixer-stop #:mixer-tick #:mixer-report
   #:mixer-rate #:mixer-frame-samples #:mixer-seq #:mixer-level #:mixer-late #:mixer-running
   #:mixer-add-source #:mixer-remove-source #:mixer-sources #:mixer-play #:audio-tone
   #:mixer-source #:mixer-source-p #:src-id #:src-name #:src-gain #:src-frames #:src-audience
   #:mixer-subscribe #:mixer-unsubscribe #:sink #:sink-p #:sink-next-frame #:sink-source
   #:sink-rate #:sink-frames #:sink-drops #:sink-underruns #:sink-gain #:sink-mix #:sink-mixer
   #:sink-unsubscribe
   ;; one listener's composite of those sources — what a SEAT gets one of
   #:mix #:mix-p #:make-mix #:remove-mix #:as-mix #:mix-name #:mix-bus #:mix-seq #:mix-level
   #:mix-sinks #:mix-source-gain #:mix-mute #:mix-unmute #:mix-hears-p #:mix-report
   #:mixer-default-mix #:mixer-default #:mixer-mixes #:mixer-find-source #:mixer-capacity
   #:*mixer-rate* #:*mixer-frame-ms* #:session-mixer #:*session-mixer* #:mixer-add-file
   ;; the mix over a socket (the :glass/audio-stream system) — server and listener
   #:start-audio-stream #:stop-audio-stream #:audio-stream #:audio-stream-p #:audio-stream-report
   #:audio-stream-port #:audio-stream-mixer #:audio-stream-mix #:audio-stream-running
   #:*audio-stream-port*
   #:start-session-audio #:seat-audio-port #:seat-mic-port #:*session-audio-stream*
   #:*audio-port-offset* #:*mic-port-offset*
   #:make-audio-tap #:audio-tap #:audio-tap-p #:tap-next-frame #:tap-source #:tap-stop
   #:tap-report #:audio-tap-connected #:audio-tap-rate #:audio-tap-frames #:audio-tap-drops
   #:audio-tap-underruns #:audio-tap-reconnects
   ;; a peer's microphone, inbound (the :glass/mic-stream system) — receiver and sender
   #:start-mic-stream #:stop-mic-stream #:start-session-mic #:mic-stream #:mic-stream-p
   #:mic-stream-port #:mic-stream-report #:*mic-stream-port* #:*mic-rate* #:*mic-live-seconds*
   #:*mic-gap-frames* #:session-mic #:*session-mic-stream* #:stream-mic #:mic-stream-current
   #:mic #:mic-p #:mic-next-frame #:mic-source #:mic-live-p #:mic-report #:mic-name #:mic-rate
   #:mic-frames #:mic-received #:mic-drops #:mic-underruns
   #:make-mic-sender #:mic-send #:mic-feed #:mic-sender-stop #:mic-sender-report
   #:mic-sender #:mic-sender-p #:mic-sender-connected #:mic-sender-sent #:mic-sender-dropped
   #:mic-sender-offered #:mic-sender-reconnects
   ;; one person's audio (the :glass/headset system) — the audio half of a SEAT
   #:headset #:make-headset #:stop-headset #:headset-report #:headset-listen
   #:headset-stop-listening #:headset-dictate #:headset-stop-dictating
   #:headset-name #:headset-mix #:headset-mixer #:headset-mic #:headset-ears #:headset-audio
   #:headset-dictation #:headset-injector #:headset-audio-port #:headset-mic-port
   #:headset-primary-p
   ;; the voice (the :glass/speech system; chord-backed) — one speaker on the session mix
   #:speak #:hush #:speaking-p #:make-speaker #:session-speaker #:stop-speaker #:speech-report
   #:speaker #:speaker-p #:*session-speaker* #:*speech-voice* #:*speech-gain* #:*speech-gap-ms*
   ;; the ear (the :glass/hearing system; stave-backed) — one sink on the same mix
   #:start-listening #:stop-listening #:listening-p #:make-ears #:ears #:ears-p
   #:ear-mix #:ear-mic-stream #:ear-rec #:ear-listening-to
   #:hearing-text #:hearing-partial #:hearing-heard #:hearing-clear #:hearing-level
   #:hearing-ready-p #:hearing-listen #:hearing-unlisten
   #:hearing-report #:*session-ears* #:*hearing-models* #:*hearing-rate* #:*hearing-threshold*
   #:*hearing-gap-seconds* #:*hearing-max-seconds* #:*hearing-preroll-seconds*
   #:*hearing-prefer-mic*
   ;; dictation (the :glass/dictation system) — the ear as a keyboard: finished utterances typed
   ;; into the focused window through the same *KEY-INJECTOR* a clipboard paste goes through
   #:start-dictation #:stop-dictation #:dictating-p #:dictation-text #:dictation-report
   #:*dictating* #:*dictation-tail-seconds* #:*dictation-typed* #:*dictation-muted*
   #:*dictation-last* #:*session-dictation*
   #:dictation #:dict-name #:dict-ear #:dict-injector #:dict-on #:dict-typed #:dict-muted
   #:dict-last
   ;; the box's identity and the terminals it trusts (the :glass/nostr system; cl-nostr-backed) —
   ;; the enrolment store, login tokens, the DM command surface, and the admission service a
   ;; transport asks instead of keeping a second copy of the answer
   #:*box-secret* #:box-identity-p #:box-pubkey #:box-npub #:unix-now
   ;; a SEAT's own npub — which PLACE this is, kept apart from who may sit in it
   #:seat-identity #:seat-identity-for #:seat-identity-known #:seat-identity-name
   #:seat-identity-pubkey #:seat-identity-npub #:seat-identity-secret
   #:list-seat-identities #:forget-seat-identity #:*seat-key-file* #:*seat-keys*
   #:*login-ttl* #:mint-login-token #:verify-login-token #:login-token-status
   #:*nostr-allow* #:normalize-pubkey #:refresh-nostr-allow #:allowed-pubkey-p
   #:*enrolment-file* #:*enrolment-ttl* #:*enrolments* #:load-enrolments #:save-enrolments
   #:sync-enrolments #:enrol-device #:device-enrolled-p #:list-enrolments #:enrolment-count
   #:revoke-enrolments #:describe-enrolments #:describe-revoke
   #:admit-peer #:parse-nostr-command #:nostr-command-reply #:*login-url-base*
   #:*admission-port* #:*admission-port-offset* #:seat-admission-port #:*admission-host*
   #:*admission-timeout* #:admission-serve #:start-admission-service #:stop-admission-service
   #:admission-service #:admission-service-p #:admission-service-port #:admission-service-report
   #:admission-service-running #:*session-admission-service*
   #:admission-request #:admission-ping #:admission-admit #:admission-allowed-p
   #:admission-devices #:admission-revoke #:admission-mint
   #:*nostr-relays* #:*nostr-command-max-age* #:start-nostr-bot #:stop-nostr-bot
   #:nostr-bot #:nostr-bot-p #:nostr-bot-report #:nostr-bot-pubkey #:nostr-bot-npub
   #:nostr-bot-received #:nostr-bot-answered #:nostr-bot-denied #:nostr-bot-ignored
   #:*session-nostr-bot* #:start-session-nostr
   ;; standing perf counters (read a snapshot over the control socket)
   #:*perf-on* #:perf-reset #:perf-report #:perf-record-send #:perf-record-composite
   #:*send-lag* #:*send-queue*))
