;; Copyright 2017 Bradley Jensen
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defpackage :shcl/core/environment
  (:use
   :common-lisp :shcl/core/utility :shcl/core/posix :shcl/core/shell-environment
   :shcl/core/iterator)
  (:import-from :fset)
  (:export
   #:*environment* #:linearized-exported-environment #:with-environment-scope
   #:env #:export-variable #:unexport-variable #:clear-environment #:exported-p
   #:unset-env #:colon-list-iterator
   #:$ifs #:$path #:$cdpath #:$pwd #:$oldpwd #:$home))
(in-package :shcl/core/environment)

(optimization-settings)

(defparameter *env-default* ""
  "The value of unset environment variables.")

(defclass environment-binding ()
  ((value
    :initarg :value
    :initform *env-default*
    :accessor environment-binding-value
    :type string
    :documentation
    "The string value which this environment variable is bound to.")
   (exported-p
    :initarg :exported
    :initform nil
    :accessor environment-binding-exported-p
    :type boolean
    :documentation
    "A boolean indicating whether this binding should be shared with
spawned processes."))
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
  "The brains of `set-env' and `(setf env)'."
  (declare (ignore default))
  ;; We only take in a default so that (setf env) can pass it to us
  ;; (and thus mark the default as "used") to supress warnings.
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
  "Set an environment variable."
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
  "This function interprets `string' as a #\: delimited list and
returns an iterator which produces the elements of that list."
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

(defmacro define-environment-accessor (sym-and-name &body options)
  "Define a symbol macro that accesses the given environment
variable."
  (when (symbolp sym-and-name)
    (setf sym-and-name (list sym-and-name (subseq (symbol-name sym-and-name) 1))))
  (destructuring-bind (sym name) sym-and-name
    (unless (equal #\$ (aref (symbol-name sym) 0))
      (error "environment accessors must start with $"))
    (let ((default
           (second (or (assoc :default options) '(:default *env-default*))))
          (documentation
           (second (or (assoc :documentation options) '(:documentation t)))))
      (when (eq t documentation)
        (setf documentation (format nil "This symbol macro accesses the $~A shell environment variable.

If that shell variable is unbound, this symbol evaluates to ~S." name default)))
      `(progn
         (define-symbol-macro ,sym (env ,name ,default))
         ,@(when documentation
             `((setf (documentation ',sym 'variable) ,documentation)))
         ',sym))))

(define-once-global %ifs-default% (format nil "~C~C~C" #\space #\tab #\linefeed)
  (:documentation
   "The value of $IFS dictated by the posix standard."))

(define-environment-accessor $ifs
  (:default %ifs-default%))
(define-environment-accessor $path)
(define-environment-accessor $cdpath)
(define-environment-accessor $pwd)
(define-environment-accessor $oldpwd)
(define-environment-accessor $home)
