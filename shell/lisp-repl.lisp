(defpackage :shcl/shell/lisp-repl
  (:use :common-lisp :shcl/core/builtin :shcl/core/utility)
  (:import-from :shcl/shell/prompt #:make-editline-stream)
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

(defparameter *fresh-prompt* t)

(defun repl-prompt ()
  (let ((result (if *fresh-prompt* "shcl (lisp)> " "> ")))
    (setf *fresh-prompt* nil)
    result))

(define-builtin shcl-repl (args)
  (declare (ignore args))
  (catch 'return-to-shell
    (let ((*package* (find-package :shcl-user))
          (*fresh-prompt* t)
          (stdin (make-editline-stream 'repl-prompt)))
      (loop
         (restart-case
             (progn
               (fresh-line *standard-output*)
               (finish-output *standard-output*)
               (let ((form (shcl-repl-read stdin)))
                 (setf *fresh-prompt* t)
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
