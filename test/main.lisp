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
  (:import-from :shcl/core/utility #:optimization-settings)
  (:import-from :shcl/test/lexer)
  (:import-from :shcl/test/utility)
  (:import-from :shcl/test/posix)
  (:import-from :shcl/test/lisp-interpolation)
  (:import-from :shcl/test/data)
  (:import-from :shcl/test/iterator)
  (:import-from :shcl/test/sequence)
  (:import-from :shcl/test/command)
  (:import-from :shcl/test/shell-lambda)
  (:import-from :shcl/test/fd-table)
  (:import-from :shcl/test/shell-form)
  (:import-from :shcl/test/parser-2)
  (:import-from :shcl/test/lint)
  (:import-from :shcl/test/foundation
                #:run-test-set #:all-tests #:package-test-set #:symbol-test)
  (:import-from :shcl/core/command)
  (:import-from :shcl/core/shell-environment #:with-subshell)
  (:import-from :prove)
  (:import-from :fset))
(in-package :shcl/test/main)

(optimization-settings)

(shcl/core/command:define-builtin -shcl-run-tests (&flag (color "--no-color" "--color")
                                                         &option package)
  "Run unit tests!

-shcl-run-tests [--color | --no-color] [--package <PACKAGE>]

This command runs the unit tests defined in the named test
packages (e.g. SHCL/TEST/SEQUENCE).

If no packages are specified then this command runs all registered
unit tests."
  (with-subshell
    (let ((prove:*enable-colors* (and (not (zerop (length color)))
                                      (equal "--color"
                                             (aref color (1- (length color))))))
          (prove:*test-result-output* *standard-output*)
          (tests
           (cond
             ((zerop (length package))
              (shcl/test/foundation:dependency-ordered-tests))

             (t
              (shcl/core/iterator:concatenate-iterable-collection
               (shcl/core/iterator:mapped-iterator
                (shcl/core/iterator:iterator package)
                (lambda (package)
                  (shcl/core/iterator:iterator (package-test-set package)))))))))
      (if (run-test-set tests)
          0
          1))))
