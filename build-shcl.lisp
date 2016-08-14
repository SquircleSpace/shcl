(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(let ((here (truename ".")))
  (push here asdf:*central-registry*))
(handler-case
    (progn
      (asdf:compile-system :shcl)
      (asdf:load-system :shcl)
      (funcall (intern "OBSERVE-DUMP" (find-package "SHCL.UTILITY")))
      (sb-ext:save-lisp-and-die "shcl" :toplevel (intern "MAIN" (find-package "SHCL")) :executable t :save-runtime-options t :purify t))
  (error (c) (format *error-output* "Fatal error: ~A~%" c)))
