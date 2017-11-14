(defpackage :shcl/core/builtin
  (:use :common-lisp :shcl/core/utility)
  (:import-from :fset)
  (:import-from :alexandria)
  (:import-from :shcl/core/fd-table #:with-fd-streams)
  (:import-from :shcl/core/exit-info #:exit-info #:make-exit-info)
  (:shadow #:dump-logs)
  (:export #:define-builtin #:lookup-builtin #:builtin-argument-error #:wrap-errors))
(in-package :shcl/core/builtin)

(optimization-settings)

(defparameter *builtin-table* (fset:empty-map)
  "A map from builtin name (string) to handler functions.")

(define-condition builtin-argument-error (error)
  ((name
    :writer set-builtin-argument-error-name
    :reader builtin-argument-error-name
    :initarg :name
    :initform nil)
   (message
    :initarg :message
    :initform nil
    :reader builtin-argument-error-message)
   (error
    :initarg :error
    :initform nil
    :reader builtin-argument-error-error))
  (:report
   (lambda (c s)
     (format s "~A: ~A"
             (builtin-argument-error-name c)
             (or (builtin-argument-error-message c)
                 (builtin-argument-error-error c)
                 "Unknown error")))))

(defun wrap-error (error)
  (unless (typep error 'builtin-argument-error)
    (error 'builtin-argument-error :error error)))

(defmacro wrap-errors (&body body)
  `(handler-bind
       ((error 'wrap-error))
     ,@body))

(defconstant +builtin-argument-error-return-code+ 2)

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
      (let ((e (gensym "E"))
            (result (gensym "RESULT")))
        `(progn
           (defun ,function-sym (,args)
             ,@(when doc-string (list doc-string))
             ,@declarations
             (let ((,result
                    (block ,function-sym
                      (handler-case
                          (with-fd-streams ()
                            ,@body-forms)
                        (builtin-argument-error (,e)
                          (unless (builtin-argument-error-name ,e)
                            (set-builtin-argument-error-name ,string-form ,e))
                          (format *error-output* "~A~%" ,e)
                          (return-from ,function-sym +builtin-argument-error-return-code+))))))
               (etypecase ,result
                 ((integer 0 255) ;; 8 bits
                  (make-exit-info :exit-status ,result))
                 (exit-info
                  ,result))))
           (setf *builtin-table* (fset:with *builtin-table* ,string-form ',function-sym))
           ',function-sym)))))

(defun lookup-builtin (name)
  "Attempt to find the function which corresponds to the builtin with
the provided string name.

Returns nil if there is no builtin by the given name."
  (fset:lookup *builtin-table* name))

(define-builtin (colon ":") (args)
  (declare (ignore args))
  0)
