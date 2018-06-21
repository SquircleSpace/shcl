;; Copyright 2018 Bradley Jensen
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

(defpackage :shcl/test/command
  (:use :common-lisp :prove :shcl/core/utility :shcl/core/command
        :shcl/test/foundation)
  (:import-from :shcl/core/shell-environment #:with-subshell)
  (:import-from :shcl/core/lisp-interpolation #:evaluate-shell-string)
  (:import-from :shcl/core/exit-info #:exit-info-true-p #:exit-info-exit-status
                #:truthy-exit-info)
  (:import-from :fset))
(in-package :shcl/test/command)

(optimization-settings)

(defclass test-command ()
  ())

(defmethod invoke-command ((command test-command) modifier &rest rest)
  (declare (ignore rest))
  (when modifier
    modifier)
  (if (next-method-p)
      (call-next-method)
      (class-name (class-of command))))

(defclass fallback (test-command)
  ((priority
    :reader command-priority
    :initform 1000
    :allocation :class)))

(defclass value-producer (test-command)
  ((priority
    :reader command-priority
    :initform 5
    :allocation :class)
   (value
    :initarg :value
    :initform nil)))

(defmethod invoke-command ((command value-producer) modifier &rest args)
  (declare (ignore args modifier))
  (slot-value command 'value))

(define-test command-namespace
  (let ((*command-namespace* (make-command-namespace :fallback (make-instance 'fallback))))
    (is (invoke-command (lookup-command (gensym)) nil)
        'fallback)
    (is (invoke-command (lookup-command "foobar") nil)
        'fallback)
    (install-command "foobar" (make-instance 'value-producer :value "barfood"))
    (is (invoke-command (lookup-command "foobar") nil)
        "barfood"
        :test 'equal)
    (is-condition
     (install-command "foobar" (make-instance 'value-producer :value "foobar"))
     'warning
     "Redefining commands emits a warning")
    (is-condition
     (install-command "command/with/slash" (make-instance 'value-producer :value "confusing"))
     'confusing-command-name
     "Commands with slashes produce warnings")
    (handler-case
        (progn
          (install-command "other" (make-instance 'value-producer))
          (pass "Correct usage doesn't emit a warning"))
      (redefining-command (e)
        (declare (ignore e))
        (fail "Correct usage generated a warning")))))

(define-test exit
  (ok (exit-info-true-p
       (with-subshell
         (evaluate-shell-string "exit")))
      "Exit with no arguments exits with status 0")
  (is (exit-info-exit-status
       (with-subshell
         (evaluate-shell-string "exit 1")))
      1
      "Exit respects its argument")
  (ok (exit-info-true-p
       (with-subshell
         (with-subshell
           (evaluate-shell-string "exit 1"))
         (truthy-exit-info)))
      "Exit only exits the inner-most subshell"))
