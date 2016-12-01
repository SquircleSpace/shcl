(defpackage :shcl/core/environment
  (:use :common-lisp :shcl/core/utility :shcl/core/posix
        :shcl/core/shell-environment)
  (:import-from :fset)
  (:export
   #:*environment* #:linearized-exported-environment #:with-environment-scope
   #:env #:export-variable #:unexport-variable #:clear-environment #:exported-p
   #:unset-env #:colon-list-iterator
   #:$ifs #:$path #:$pwd #:$oldpwd))
(in-package :shcl/core/environment)

(optimization-settings)

(defparameter *env-default* ""
  "The value of unset environment variables.")

(defclass environment-binding ()
  ((value
    :initarg :value
    :initform *env-default*
    :accessor environment-binding-value
    :type string)
   (exported-p
    :initarg :exported
    :initform nil
    :accessor environment-binding-exported-p
    :type boolean))
  (:documentation
   "Everything there is to know about an environment variable."))

(defparameter *env-default-binding* (make-instance 'environment-binding)
  "The default state for an environment binding.")

(defun deconstruct-environment-binding (binding)
  "Parse a string describing an environment binding.

Returns two values: the variable name and the value."
  (let (index)
    (loop :for i :below (length binding) :do
       (when (equal (aref binding i) #\=)
         (setf index i)
         (return)))
    (unless index
      (error "Invalid environment binding string: ~A" binding))
    (values (subseq binding 0 index) (subseq binding (1+ index)))))

(defun environment-to-map ()
  "Translate the posix environment of the current process into a map
suitable for storing in `*environment*'."
  (let ((map (fset:empty-map *env-default-binding*)))
    (do-iterator (binding (environment-iterator))
      (multiple-value-bind (key value) (deconstruct-environment-binding binding)
        (setf map (fset:with map key (make-instance 'environment-binding
                                                    :value value
                                                    :exported t)))))
    map))

(defparameter *environment* (environment-to-map)
  "The current posix environment.")
(on-revival reset-environment)
(on-dump clear-environment)
(preserve-special-variable '*environment*)

(defun reset-environment ()
  "Set `*environment*' based on the current posix environment."
  (setf *environment* (environment-to-map)))

(defun clear-environment ()
  "Empty out `*environment*'."
  (setf *environment* (fset:empty-map)))

(defun linearized-exported-environment (&optional (environment *environment*))
  "Produce a sequence containing all the exported environment binding
strings."
  (let ((result (fset:empty-seq)))
    (fset:do-map (key value environment)
      (when (environment-binding-exported-p value)
        (fset:push-last result (concatenate 'string key "=" (environment-binding-value value)))))
    result))

(defmacro with-environment-scope ((&optional (environment '*environment*)) &body body)
  "Bind `*environment*' to the given value and evaluate the given
forms."
  `(let ((*environment* ,environment))
     ,@body))

(defun lookup-with-default (collection key default)
  "Similar to `fset:lookup', but returns the given default value if
the given key is absent.

The second return value is t iff the key was found in the collection."
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

(defun env (key &optional (default *env-default*))
  "Look up the given variable in the current environment."
  (multiple-value-bind (entry found) (fset:lookup *environment* key)
    (if found
        (values (environment-binding-value entry) t)
        (values default nil))))

(defun exported-p (key)
  "Returns t iff the variable named by `key' should be exported."
  (let ((entry (fset:lookup *environment* key)))
    (when entry
      (environment-binding-exported-p entry))))

(defun %set-env (key value default)
  ;; We only take in a default so that (setf env) can pass it to us
  ;; (and thus mark the default as "used") to supress warnings.
  (declare (ignore default))
  (setf *environment* (fset:with *environment* key
                                 (make-instance 'environment-binding
                                                :value value
                                                :exported (exported-p key))))
  value)

(defun set-env (key value)
  "Change the environment by changing the value associated with a
given key."
  (%set-env key value nil))

(defsetf env (key &optional default) (value)
  `(%set-env ,key ,value ,default))

(defun unset-env (key)
  "Remove the variable with the given name from the environment."
  (setf *environment* (fset:less *environment* key)))

(defun export-variable (key)
  "Make the variable with the given name exported."
  (setf (fset:lookup *environment* key) (make-instance 'environment-binding
                                                       :value (env key)
                                                       :exported t)))

(defun unexport-variable (key)
  "Make the variable with the given name not exported."
  (setf (fset:lookup *environment* key) (make-instance 'environment-binding
                                                       :value (env key)
                                                       :exported nil)))

(defun colon-list-iterator (string)
  (let ((part (make-string-output-stream))
        (iterator (vector-iterator string)))
    (make-iterator ()
      (do-iterator (c iterator)
        (case c
          (#\:
           (emit (get-output-stream-string part)))
          (otherwise
           (write-char c part))))
      (let ((last-part (get-output-stream-string part)))
        (unless (zerop (length last-part))
          (emit last-part))
        (stop)))))

(defmacro define-environment-accessor (name &optional (default '*env-default*))
  "Define a symbol macro that accesses the given environment
variable."
  `(define-symbol-macro ,(intern (concatenate 'string "$" (string-upcase name))) (env ,name ,default)))

(define-once-global %ifs-default% (format nil "~C~C~C" #\space #\tab #\linefeed)
  (:documentation
   "The value of $IFS dictated by the posix standard."))

(define-environment-accessor "IFS" %ifs-default%)
(define-environment-accessor "PATH")
(define-environment-accessor "PWD")
(define-environment-accessor "OLDPWD")
