(defpackage :shcl/core/builtin
  (:use :common-lisp :shcl/core/utility)
  (:import-from :fset)
  (:import-from :alexandria)
  (:import-from :shcl/core/fd-table #:with-fd-streams)
  (:shadow #:dump-logs)
  (:export #:define-builtin #:lookup-builtin))
(in-package :shcl/core/builtin)

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
  (when (find #\/ name)
    (warn "Builtin name ~W is not callable in shell commands due to #\/ character" name))
  (destructuring-bind (function-sym string-form) name
    (multiple-value-bind (body-forms declarations doc-string) (alexandria:parse-body body :documentation t)
      `(progn
         (defun ,function-sym (,args)
           ,@(when doc-string (list doc-string))
           ,@declarations
           (with-fd-streams ()
             ,@body-forms))
         (setf *builtin-table* (fset:with *builtin-table* ,string-form ',function-sym))
         ',function-sym))))

(defun lookup-builtin (name)
  "Attempt to find the function which corresponds to the builtin with
the provided string name.

Returns nil if there is no builtin by the given name."
  (fset:lookup *builtin-table* name))
