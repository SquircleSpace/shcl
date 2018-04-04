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

(defmacro with-subshell (&body body)
  (let ((env (gensym "ENV")))
    `(let ((,env (preserve-shell-environment :label 'with-subshell)))
       (unwind-protect
            (with-restored-shell-environment ,env
              (destroy-preserved-shell-environment ,env)
              ,@body)
         (destroy-preserved-shell-environment ,env)))))

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
