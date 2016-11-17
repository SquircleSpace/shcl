(defpackage :shcl/builtin
  (:use :common-lisp :shcl/utility #:shcl/fd-table)
  (:import-from :fset)
  (:import-from :alexandria)
  (:shadow #:dump-logs)
  (:export #:define-builtin #:lookup-builtin))
(in-package :shcl/builtin)

(optimization-settings)

(defparameter *builtin-table* (fset:empty-map)
  "A map from builtin name (string) to handler functions.")

(defmacro define-builtin (name (args) &body body)
  "Define a new shell builtin.

`name' should either be a symbol or a list of the
form (`function-name' `builtin-name') where `function-name' is a
symbol and `builtin-name' is a string.  If `name' is simply a symbol,
then the builtin name is the downcased symbol name."
  (when (symbolp name)
    (setf name (list name (string-downcase (symbol-name name)))))
  (destructuring-bind (function-sym string-form) name
    (multiple-value-bind (body-forms declarations doc-string) (alexandria:parse-body body :documentation t)
      `(progn
         (defun ,function-sym (,args)
           ,@(when doc-string (list doc-string))
           ,@declarations
           (with-fd-streams ()
             ,@body-forms))
         (setf *builtin-table* (fset:with *builtin-table* ,string-form ',function-sym))))))

(defun lookup-builtin (name)
  "Attempt to find the function which corresponds to the builtin with
the provided string name.

Returns nil if there is no builtin by the given name."
  (fset:lookup *builtin-table* name))

(define-builtin dump-logs (args)
  (declare (ignore args))
  (shcl/utility:dump-logs)
  0)
