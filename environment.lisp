(in-package :shcl.environment)

(optimization-settings)

(define-once-global +env-default+ "")

(defclass environment-binding ()
  ((value
    :initarg :value
    :initform +env-default+
    :accessor environment-binding-value
    :type string)
   (exported-p
    :initarg :exported
    :initform nil
    :accessor environment-binding-exported-p
    :type boolean)))

(define-once-global +env-default-binding+ (make-instance 'environment-binding))

(defun deconstruct-environment-binding (binding)
  (let (index)
    (loop :for i :below (length binding) :do
       (when (equal (aref binding i) #\=)
         (setf index i)
         (return)))
    (values (subseq binding 0 index) (subseq binding (1+ index)))))

(defun environment-to-map ()
  (let ((map (fset:empty-map +env-default-binding+)))
    (do-iterator (binding (environment-iterator))
      (multiple-value-bind (key value) (deconstruct-environment-binding binding)
        (setf map (fset:with map key (make-instance 'environment-binding
                                                    :value value
                                                    :exported t)))))
    map))

(defparameter *environment* (environment-to-map))
(on-revival reset-environment)
(on-dump clear-environment)

(defun reset-environment ()
  (setf *environment* (environment-to-map)))

(defun clear-environment ()
  (setf *environment* (fset:empty-map)))

(defun linearized-exported-environment (&optional (environment *environment*))
  (let ((result (fset:empty-seq)))
    (fset:do-map (key value environment)
      (when (environment-binding-exported-p value)
        (fset:push-last result (concatenate 'string key "=" (environment-binding-value value)))))
    result))

(defmacro with-environment-scope ((&optional (environment '*environment*)) &body body)
  `(let ((*environment* ,environment))
     ,@body))

(defun lookup-with-default (collection key default)
  (multiple-value-bind (value found) (fset:lookup collection key)
    (if found
        (values value t)
        (values default nil))))

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

(defun env (key &optional (default +env-default+))
  (multiple-value-bind (entry found) (fset:lookup *environment* key)
    (if found
        (values (environment-binding-value entry) t)
        (values default nil))))

(defun exported-p (key)
  (let ((entry (fset:lookup *environment* key)))
    (when entry
      (environment-binding-exported-p entry))))

(defun %set-env (key value default)
  (declare (ignore default))
  (setf *environment* (fset:with *environment* key
                                 (make-instance 'environment-binding
                                                :value value
                                                :exported (exported-p key))))
  value)

(defun set-env (key value)
  (%set-env key value nil))

(defsetf env (key &optional default) (value)
  `(%set-env ,key ,value ,default))

(defun unset-env (key)
  (setf *environment* (fset:less *environment* key)))

(defun export-variable (key)
  (setf (fset:lookup *environment* key) (make-instance 'environment-binding
                                                       :value (env key)
                                                       :exported t)))

(defun unexport-variable (key)
  (setf (fset:lookup *environment* key) (make-instance 'environment-binding
                                                       :value (env key)
                                                       :exported nil)))

(defmacro define-environment-accessor (name &optional (default ""))
  `(define-symbol-macro ,(intern (concatenate 'string "$" (string-upcase name))) (env ,name ,default)))

(define-once-global %ifs-default% (format nil "~C~C~C" #\space #\tab #\linefeed))
(define-environment-accessor "IFS" %ifs-default%)
(define-environment-accessor "PATH")
(define-environment-accessor "PWD")
(define-environment-accessor "OLDPWD")
