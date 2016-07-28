(in-package :shcl.environment)

(optimization-settings)

(defun deconstruct-environment-binding (binding)
  (let (index)
    (loop :for i :below (length binding) :do
       (when (equal (aref binding i) #\=)
         (setf index i)
         (return)))
    (values (subseq binding 0 index) (subseq binding (1+ index)))))

(defun environment-to-map ()
  (let ((map (fset:empty-map "")))
    (do-iterator (binding (environment-iterator))
      (multiple-value-bind (key value) (deconstruct-environment-binding binding)
        (setf map (fset:with map key value))))
    map))

(defun reset-environment ()
  (setf *environment* (environment-to-map)))

(defparameter *environment* (environment-to-map))
(on-revival reset-environment)

(defmacro with-environment ((environment) &body body)
  `(let ((*environment* ,environment))
     ,@body))

(defun lookup-with-default (collection key default)
  (multiple-value-bind (value found) (fset:lookup collection key)
    (if found
        value
        default)))

(define-setf-expander lookup-with-default (collection key default &environment env)
  (multiple-value-bind (temp-vars temp-vals new-value-vars setter getter)
      (get-setf-expansion collection env)
    (let ((value (gensym "VALUE"))
          (key-sym (gensym "KEY"))
          (default-sym (gensym "DEFAULT")))
      (values
       (list* key-sym default-sym temp-vars)
       (list* key default temp-vals)
       `(,value)
       `(let ((,(first new-value-vars) (fset:with ,getter ,key-sym ,value)))
          ,setter
          ,value)
       `(lookup-with-default ,getter ,key-sym ,default-sym)))))

(defun env (key &optional (default ""))
  (lookup-with-default *environment* key default))

(defun %set-env (key value default)
  (declare (ignore default))
  (setf *environment* (fset:with *environment* key value))
  value)

(defun set-env (key value)
  (%set-env key value nil))

(defsetf env (key &optional default) (value)
  `(%set-env ,key ,value ,default))

(defmacro define-environment-accessor (name &optional (default ""))
  `(define-symbol-macro ,(intern (concatenate 'string "$" (string-upcase name))) (env ,name ,default)))

(define-once-global %ifs-default% (format nil "~C~C~C" #\space #\tab #\linefeed))
(define-environment-accessor "IFS" %ifs-default%)
(define-environment-accessor "PATH")
(define-environment-accessor "PWD")
(define-environment-accessor "OLDPWD")
