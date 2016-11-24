(defpackage :shcl/shell/directory
  (:use :common-lisp :shcl/core/builtin :shcl/core/environment :shcl/core/working-directory))
(in-package :shcl/shell/directory)

(define-builtin (builtin-cd "cd") (args)
  ;; Cut off command name
  (setf args (fset:less-first args))
  (when (zerop (fset:size args))
    (let ((home (env "HOME")))
      (when (zerop (length home))
        (format *error-output* "cd: Could not locate home")
        (return-from builtin-cd 1))

      (fset:push-last args home)))

  (cd (fset:last args))
  0)

(define-builtin pushd (args)
  (fset:pop-first args)
  (unless (equal 1 (fset:size args))
    (format *error-output* "Anything but 1 arg pushd is not implemented~%")
    (return-from pushd 1))

  (push-working-directory (fset:last args))
  0)

(define-builtin popd (args)
  (fset:pop-first args)
  (unless (equal 0 (fset:size args))
    (format *error-output* "popd takes no arguments~%")
    (return-from popd 1))

  (pop-working-directory)
  0)
