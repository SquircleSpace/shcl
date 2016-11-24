(defpackage :shcl/shell/lisp-repl
  (:use :common-lisp :shcl/core/builtin)
  (:export #:return-to-shell #:shell-help))
(in-package :shcl/shell/lisp-repl)

(defpackage :shcl-user
  (:use :common-lisp :shcl/shell/lisp-repl))

(defun return-to-shell ()
  (throw 'return-to-shell t))

(defun shell-help ()
  (format
   *standard-output*
   "Welcome to SHCL REPL!  The following special commands are recognized.
:help to see this
:shell to return to SHCL
"))

(define-builtin shcl-repl (args)
  (declare (ignore args))
  (catch 'return-to-shell
    (let ((eof-value '#:eof-value)
          (*package* (find-package :shcl-user)))
      (labels
          ((print-list (list)
             (dolist (value list)
               (format *standard-output* "~A~%" value))
             (finish-output *standard-output*))
           (our-read ()
             (let ((form (read *standard-input* nil eof-value)))
               (when (eq form eof-value)
                 (format *standard-output* "~%")
                 (return-to-shell))

               (case form
                 (:shell
                  (return-to-shell))
                 (:help
                  (shell-help)
                  '(values))
                 (otherwise
                  form))))
           (rep ()
             (fresh-line *standard-output*)
             (format *standard-output* "shcl (lisp)> ")
             (finish-output *standard-output*)
             (print-list (multiple-value-list (eval (our-read))))))
        (loop
           (restart-case (rep)
             (restart-lisp-repl ()
               (fresh-line *standard-output*))
             (exit-lisp-repl ()
               (return-to-shell)))))))
  0)
