(in-package :shcl.builtin)

(optimization-settings)

(defparameter *builtin-table* (fset:empty-map))

(defmacro define-builtin (name (args) &body body)
  (when (symbolp name)
    (setf name (list name (string-downcase (symbol-name name)))))
  (destructuring-bind (function-sym string-form) name
    `(progn
       (defun ,function-sym (,args)
         ,@body)
       (setf *builtin-table* (fset:with *builtin-table* ,string-form ',function-sym)))))

(defun lookup-builtin (name)
  (fset:lookup *builtin-table* name))
