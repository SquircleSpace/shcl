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
   #:preserve-special-variable))
(in-package :shcl/core/shell-environment)

(optimization-settings)

(defstruct entry
  pickle
  unpickle
  reclaim)

(defparameter *shell-environment-handlers* (make-hash-table))

(defun extend-shell-environment (name pickler unpickler reclaimer)
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

(defun preserve-shell-environment ()
  (let* ((table (make-hash-table))
         (result (make-preserved-environment :data table)))
    (labels
        ((panic ()
           (assert nil nil "A preserved shell environment was leaked"))
         (handle (key value)
           (setf (gethash key table) (funcall (entry-pickle value)))))
      (declare (dynamic-extent #'handle))
      (maphash #'handle *shell-environment-handlers*)
      (trivial-garbage:finalize result #'panic)
      result)))

(defun destroy-preserved-shell-environment (shell-environment)
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
  `(call-with-restored-shell-environment ,shell-environment (lambda () ,@body)))

(defparameter *special-variables-to-preserve* (fset:empty-set))

(defun preserve-special-variable (symbol)
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
