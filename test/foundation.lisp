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

(defpackage :shcl/test/foundation
  (:use :common-lisp :prove :lisp-namespace)
  (:import-from :fset)
  (:export #:define-test #:run-test-set #:all-tests #:package-test-set
           #:symbol-test))
(in-package :shcl/test/foundation)

(defvar *package-tests* (fset:empty-map (fset:empty-set)))

(define-namespace
    test function nil
    "A namespace for SHCL unit tests.")

(defun ensure-package (package-designator)
  "Convert the argument to a package if it isn't one already."
  (if (typep package-designator 'package)
      package-designator
      (find-package package-designator)))

(defmacro define-test (name &body body)
  "Define a unit test for SHCL functionality.

`name' is a symbol representing the test's unique name.  Use `prove'
test forms inside the body (e.g. `ok')."
  (let ((package *package*))
    `(progn
       (setf (symbol-test ',name) (lambda () (subtest ,(format nil "~A::~A"
                                                               (package-name package)
                                                               (symbol-name name))
                                               ,@body)))
       (fset:adjoinf (fset:lookup *package-tests* (ensure-package ,package)) ',name)
       ',name)))

(defun run-test-set (tests)
  "Run the test functions in the given set."
  (let ((*suite* (make-instance 'suite :plan (fset:size tests))))
    (fset:do-set (test tests)
      (funcall (symbol-test test)))
    (finalize)))

(defun package-test-set (package)
  "Get the set of tests defined in the given package."
  (fset:lookup *package-tests* (ensure-package package)))

(defun all-tests ()
  "Get the set of all tests defined with `define-test'."
  (let ((all-tests (fset:empty-set)))
    (fset:do-map (package tests *package-tests*)
      (declare (ignore package))
      (fset:unionf all-tests tests))
    all-tests))
