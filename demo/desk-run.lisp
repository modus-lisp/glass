;;;; demo/desk-run.lisp — serve the desk.  Run by demo/desk.sh: from the saved core on modus,
;;;; or after loading on SBCL.
;;;;   env DESK_PORT     the RFB port (default 5910)
;;;;       DESK_ADDRESS  the interface to bind (default 127.0.0.1: this machine only)
;;;;       DESK_SIZE     WxH (default 1024x700)
;;;;       DESK_MEDIA    the folder Media plays from (default ~/Music/, else ~/Videos/)

(unless (find-package "GLASS.DESK")
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
  (load (merge-pathnames "desk-load.lisp" (or *load-truename* *load-pathname*
                                              (uiop:getenv "DESK_DEMO_DIR")))))

(defun cl-user::desk-env (name default) (let ((v (uiop:getenv name))) (if (and v (plusp (length v))) v default)))

(let* ((port (parse-integer (cl-user::desk-env "DESK_PORT" "5910")))
       (address (cl-user::desk-env "DESK_ADDRESS" "127.0.0.1"))
       (size (cl-user::desk-env "DESK_SIZE" "1024x700"))
       (x (position #\x size))
       (width (parse-integer size :end x))
       (height (parse-integer size :start (1+ x)))
       (home (namestring (user-homedir-pathname)))
       (media (cl-user::desk-env "DESK_MEDIA"
                                 (if (probe-file (concatenate 'string home "Music/"))
                                     (concatenate 'string home "Music/")
                                     (concatenate 'string home "Videos/")))))
  (setf glass.desk:*media-root* media)
  (format t "~&desk: ~dx~d on ~a:~d -- point a VNC viewer there~%desk: media from ~a~%"
          width height address port media)
  (finish-output)
  (glass.desk:serve-desk port :width width :height height :address address
                              :apps (if (and (boundp 'cl-user::*desk-media*) cl-user::*desk-media*)
                                        glass.desk:*standard-apps*
                                        (remove "Media" glass.desk:*standard-apps*
                                                :key #'first :test #'string=))))
