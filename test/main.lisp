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

(defpackage :shcl/test/main
  (:use :common-lisp)
  (:import-from :shcl/test/lexer)
  (:import-from :shcl/test/utility)
  (:import-from :shcl/test/posix)
  (:import-from :shcl/test/lisp-interpolation)
  (:import-from :shcl/test/data)
  (:import-from :shcl/test/iterator)
  (:import-from :shcl/test/command)
  (:import-from :shcl/test/fd-table)
  (:import-from :shcl/test/shell-form)
  (:import-from :shcl/test/foundation
                #:run-test-set #:all-tests #:package-test-set #:symbol-test)
  (:import-from :shcl/core/command)
  (:import-from :shcl/core/shell-environment #:with-subshell)
  (:import-from :prove)
  (:import-from :fset)
  (:export #:run-all-tests))
(in-package :shcl/test/main)

(shcl/core/command:define-builtin -shcl-run-tests (&option package)
  "Run unit tests."
  (with-subshell
    (let ((prove:*enable-colors* nil)
          (prove:*test-result-output* *standard-output*)
          (test-set (fset:empty-set)))
      (cond
        ((zerop (length package))
         (setf test-set (all-tests)))

        (t
         (loop :for package :across package :do
            (fset:unionf test-set (package-test-set package)))))
      (if (run-test-set test-set)
          0
          1))))
