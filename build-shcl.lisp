(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(let ((here (truename ".")))
  (push here asdf:*central-registry*))
(asdf:load-system :shcl)
(sb-ext:save-lisp-and-die "shcl" :toplevel (lambda ()) :executable t :save-runtime-options t :purify t)
