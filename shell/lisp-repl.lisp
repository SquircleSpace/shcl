(defpackage :shcl/shell/lisp-repl
  (:use :common-lisp :shcl/core/builtin :shcl/core/utility)
  (:export #:return-to-shell #:shell-help))
(in-package :shcl/shell/lisp-repl)

(optimization-settings)

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

(defun shcl-repl-read (stdin)
  (let* ((eof-value '#:eof-value)
         (form (read stdin nil eof-value)))
    (when (eq form eof-value)
      (debug-log status "SHCL-REPL EOF")
      (format *standard-output* "~%")
      (return-to-shell))

    (debug-log status "SHCL-REPL READ ~A" form)
    form))

(defun print-list (list stdout)
  (dolist (value list)
    (format stdout "~A~%" value))
  (finish-output stdout))

(define-builtin shcl-repl (args)
  (declare (ignore args))
  (catch 'return-to-shell
    (let ((*package* (find-package :shcl-user)))
      (loop
         (restart-case
             (progn
               (fresh-line *standard-output*)
               (format *standard-output* "shcl (lisp)> ")
               (finish-output *standard-output*)
               (let ((form (shcl-repl-read *standard-input*)))
                 (case form
                   (:shell
                    (return-to-shell))
                   (:help
                    (shell-help)
                    (values))
                   (otherwise
                    (print-list (multiple-value-list (eval form))
                                *standard-output*)))))
           (restart-lisp-repl ()
             (fresh-line *standard-output*))
           (exit-lisp-repl ()
             (return-to-shell))))))
  0)
