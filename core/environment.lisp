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
  (:import-from :shcl/core/data #:define-data)
  (:import-from :fset)
  (:export
   ;; High-level access
   #:env #:env-exported-p
   ;; Utility
   #:linearized-exported-environment #:with-environment-scope
   #:colon-list-iterator #:clear-environment #:reset-environment
   #:deconstruct-environment-assignment-string
   ;; Common environment variables
   #:$ifs #:$path #:$cdpath #:$pwd #:$oldpwd #:$home
   ;; Low-level access
   #:environment-binding #:environment-binding-value
   #:environment-binding-exported-p #:default-environment-binding))
(in-package :shcl/core/environment)

(optimization-settings)

(defparameter *env-default* ""
  "The value of unset environment variables.")

(define-data environment-binding ()
  ((value
    :initarg :value
    :initform nil
    :updater environment-binding-value
    :type (or null string)
    :documentation
    "The string value which this environment variable is bound to.")
   (exported-p
    :initarg :exported
    :initform nil
    :updater environment-binding-exported-p
    :type boolean
    :documentation
    "A boolean indicating whether this binding should be shared with
spawned processes."))
  (:documentation
   "Everything there is to know about an environment variable."))

(defparameter *env-default-binding* (make-instance 'environment-binding)
  "The default state for an environment binding.")

(define-condition invalid-environment-variable (error)
  ((name
    :initarg :name
    :reader invalid-environment-variable-name
    :initform (required)))
  (:report (lambda (c s)
             (format s "Variable name ~S is invalid.  It must be a non-empty string and cannot contain #\="
                     (invalid-environment-variable-name c)))))

(defun check-valid-environment-variable-name (variable-name)
  (unless (and (stringp variable-name)
               (plusp (length variable-name))
               (not (find #\= variable-name)))
    (error 'invalid-environment-variable :name variable-name)))

(defun deconstruct-environment-assignment-string (binding)
  "Parse a string describing an environment assignment.

Returns two values: the variable name and the value."
  (let ((index (position #\= binding)))
    (unless index
      (error "Invalid environment binding string: ~A" binding))
    (let ((var (subseq binding 0 index))
          (value (subseq binding (1+ index))))
      (check-valid-environment-variable-name var)
      (values var value))))

(defun environment-to-map ()
  "Translate the posix environment of the current process into a map
suitable for storing in `*environment*'."
  (let ((map (fset:empty-map *env-default-binding*)))
    (do-iterator (binding (environment-iterator))
      (multiple-value-bind (key value) (deconstruct-environment-assignment-string binding)
        (setf map (fset:with map key (make-instance 'environment-binding
                                                    :value value
                                                    :exported t)))))
    map))

(defparameter *environment* (environment-to-map)
  "The current posix environment.")
(on-revival reset-environment)
(on-dump clear-environment)
(preserve-special-variable '*environment*)

(defun default-environment-binding ()
  (fset:map-default *environment*))

(defun reset-environment ()
  "Set `*environment*' based on the current posix environment."
  (setf *environment* (environment-to-map)))

(defun clear-environment ()
  "Empty out `*environment*'."
  (setf *environment* (fset:empty-map *env-default-binding*)))

(defun linearized-exported-environment (&optional (environment *environment*))
  "Produce a sequence containing all the exported environment binding
strings."
  (let ((result (fset:empty-seq)))
    (fset:do-map (key value environment)
      (when (and (environment-binding-exported-p value)
                 (environment-binding-value value))
        (fset:push-last result (concatenate 'string key "=" (environment-binding-value value)))))
    result))

(defmacro with-environment-scope ((&optional (environment '*environment*)) &body body)
  "Bind `*environment*' to the given value and evaluate the given
forms."
  `(let ((*environment* ,environment))
     ,@body))

(defun environment-binding (key)
  (nth-value 0 (fset:lookup *environment* key)))

(defun (setf environment-binding) (value key)
  (check-type value environment-binding)
  (check-valid-environment-variable-name key)
  (if (eq :equal (fset:compare value (default-environment-binding)))
      (setf *environment* (fset:less *environment* key))
      (setf (fset:lookup *environment* key) value)))

(defun env (key &optional (default *env-default*))
  "Look up the given variable in the current environment."
  (let ((entry (environment-binding key)))
    (if (environment-binding-value entry)
        (values (environment-binding-value entry) t)
        (values default nil))))

(defun %set-env (key value default)
  "The brains of `(setf env)'."
  (declare (ignore default))
  ;; We only take in a default so that (setf env) can pass it to us
  ;; (and thus mark the default as "used") to supress warnings.
  (setf (environment-binding-value (environment-binding key)) value)
  value)

(defsetf env (key &optional default) (value)
  "Set an environment variable."
  `(%set-env ,key ,value ,default))

(defun env-exported-p (key)
  (environment-binding-exported-p (environment-binding key)))

(defun (setf env-exported-p) (value key)
  (setf (environment-binding-exported-p (environment-binding key))
        (not (not value)))
  value)

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
