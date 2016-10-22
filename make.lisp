(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(let ((here (truename ".")))
  (push here asdf:*central-registry*))

(handler-bind
    ((error
      (lambda (c)
        (format *error-output* "Fatal error: ~A~%" c)
        (uiop:quit 1))))
  #-ecl
  (progn
    (asdf:compile-system :shcl)
    (asdf:load-system :shcl)
    (funcall (intern "OBSERVE-DUMP" (find-package "SHCL/UTILITY")))
    #+sbcl (sb-ext:save-lisp-and-die "shcl" :toplevel (intern "MAIN" (find-package "SHCL/MAIN")) :executable t :save-runtime-options t :purify t))
  #+ecl
  (progn
    (asdf:register-immutable-system :asdf)
    (asdf:register-immutable-system :uiop)

    (defmethod asdf:output-files ((op asdf:monolithic-lib-op) (sys (eql (asdf:find-system "shcl"))))
      (values (list "libshcl.a") t))

    (asdf:operate
     'asdf:monolithic-lib-op "shcl"))
  (uiop:quit 0))
