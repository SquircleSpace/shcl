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

(defpackage :shcl/core/shell-environment
  (:use :common-lisp :shcl/core/utility)
  (:import-from :trivial-garbage)
  (:import-from :fset)
  (:export
   #:extend-shell-environment #:preserve-shell-environment
   #:destroy-preserved-shell-environment #:with-restored-shell-environment
   #:preserve-special-variable #:with-subshell))
(in-package :shcl/core/shell-environment)

(optimization-settings)

(defstruct entry
  pickle
  unpickle
  reclaim)

(defparameter *shell-environment-handlers* (make-hash-table))

(defun extend-shell-environment (name pickler unpickler reclaimer)
  "Enroll some state as part of the shell environment.

The shell environment is just a bunch of state.  Changes to shell
environment state must be confined to the subshell where the change
occured.  In a normal shell, this is accomplished by forking the shell
process when creating a subshell.  Forking naturally confines state
changes to the subshell.  SHCL doesn't fork when creating subshells,
so state must be managed manually.  This function gives you a way to
participate in SHCL's mechanism for controling state changes.

`name' is a symbol that uniquely identifies the state you're trying to
preserve.  If you call this function twice with the same name, it will
replace the earlier registration with the second registration.

`pickler' is a function that accepts no arguments.  The return value
is up to you, but it should be an object that represents the state
you're trying to protect in subshell environments.  When preserving
the current shell environment, SHCL will call your pickler function
and store the returned value.

`unpickler' is a function that accepts two arguments: the output of
your pickler function and a continuation function.  Your unpickler
function should establish a dynamic extent where the pickled state has
been restored.  Then, inside that dynamic extent, your unpickler
should call the continuation function.

`reclaimer' is a function that receives one argument: the output of
your pickler function.  When the preserved shell environment is no
longer needed, it must be explicitly destroyed.  When the shell
environment is destroyed, it will iterate through all of the pickled
state and call the appropriate reclaimer functions.  If your state
requires explicit cleanup (for example, closing files), then you
should perform that work in the reclaimer function.  You may safely
assume your preserved state will always be reclaimed exactly once.

For example, if you wanted to preserve a special variable, you might
use this function like so.

    (extend-shell-environment
      '*special-variable* ; name
      (lambda () *special-variable*)
      (lambda (value cont)
        (let ((*special-variable* value))
          (funcall cont)))
      (constantly nil))

Note that `preserve-special-variable' will do all of the above for you
automatically."
  (setf (gethash name *shell-environment-handlers*)
        (make-entry :pickle pickler :unpickle unpickler :reclaim reclaimer)))

(defstruct preserved-environment
  data)

(defun reclaim (key data)
  (funcall (entry-reclaim (gethash key *shell-environment-handlers*)) data))

(defun unpickle (key data continuation)
  (funcall (entry-unpickle (gethash key *shell-environment-handlers*)) data continuation))

(defun %destroy-preserved-shell-environment (shell-environment)
  (labels
      ((handle (key value)
         (reclaim key value)
         (remhash key shell-environment)))
    (maphash #'handle shell-environment)
    (values)))

(define-condition leaked-shell-environment (error)
  ((label
    :initarg :label
    :reader leaked-shell-environment-label
    :initform nil)
   (backtrace
    :initarg :backtrace
    :reader leaked-shell-environment-backtrace
    :initform nil))
  (:report
   (lambda (c s)
     (with-accessors ((label leaked-shell-environment-label)
                      (backtrace leaked-shell-environment-backtrace))
         c
       (format s "A shell environment was leaked.")
       (when label
         (format s "  Label: ~W." label))
       (when backtrace
         (format s "  Backtrace:~%~A" backtrace))))))

(defun preserve-shell-environment (&key label backtrace-p)
  "Capture the current shell environment and return an object
representing all of its state.

The returned object must be destroyed explicitly with
`destroy-preserved-shell-environment'.  Aside from destruction, you
can also re-hydrate the shell environment using
`with-restored-shell-environment'.

The shell environment usually consists of things like file descriptor
bindings, shell variable bindings, and things of that like.  If you
want to include state in the shell environment, use
`extend-shell-environment'."
  (let* ((table (make-hash-table))
         (backtrace (when backtrace-p
                      (with-output-to-string (s)
                        (uiop:print-backtrace :stream s))))
         (result (make-preserved-environment :data table)))
    (labels
        ((panic ()
           (error 'leaked-shell-environment :label label :backtrace backtrace))
         (handle (key value)
           (setf (gethash key table) (funcall (entry-pickle value)))))
      (declare (dynamic-extent #'handle))
      (maphash #'handle *shell-environment-handlers*)
      (trivial-garbage:finalize result #'panic)
      result)))

(defun destroy-preserved-shell-environment (shell-environment)
  "Reclaim all the resources associated with the given preserved shell environment.

It is safe to destroy a preserved shell environment multiple times.

See `preserve-shell-environment'."
  (%destroy-preserved-shell-environment (preserved-environment-data shell-environment))
  (trivial-garbage:cancel-finalization shell-environment))

(defun call-with-restored-shell-environment (shell-environment continuation)
  (let ((shell-environment (preserved-environment-data shell-environment)))
    (with-hash-table-iterator (iter shell-environment)
      (labels
          ((restore ()
             (multiple-value-bind (found key value) (iter)
               (unless found
                 (return-from restore (funcall continuation)))
               (unpickle key value #'restore))))
        (restore)))))

(defmacro with-restored-shell-environment (shell-environment &body body)
  "Evaluate body in a context where the state for `shell-environment'
is in effect.

See `preserve-shell-environment'."
  `(call-with-restored-shell-environment ,shell-environment (lambda () ,@body)))

(defmacro with-subshell (&body body)
  "Evaluate forms in an isolated shell environment.

Changes to the shell environment will be confied to the dynamic extent
of the body of this form."
  (let ((env (gensym "ENV")))
    `(let ((,env (preserve-shell-environment :label 'with-subshell)))
       (unwind-protect
            (with-restored-shell-environment ,env
              (destroy-preserved-shell-environment ,env)
              ,@body)
         (destroy-preserved-shell-environment ,env)))))

(defparameter *special-variables-to-preserve* (fset:empty-set))

(defun preserve-special-variable (symbol)
  "Include the given symbol's value when preserving the current shell
environment.

See `extend-shell-environment'."
  (setf *special-variables-to-preserve* (fset:with *special-variables-to-preserve* symbol)))

(defun preserve-special-variables ()
  (let ((l (fset:convert 'list *special-variables-to-preserve*)))
    (cons l (mapcar 'symbol-value l))))

(defun restore-special-variables (data continuation)
  (progv (car data) (cdr data)
    (funcall continuation)))

(extend-shell-environment
 'special-variables
 'preserve-special-variables
 'restore-special-variables
 (lambda (x) (declare (ignore x))))
