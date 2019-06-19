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
  (:import-from :shcl/core/sequence #:do-sequence #:eager-flatmap-sequence)
  (:import-from :shcl/core/utility #:optimization-settings)
  (:import-from :fset)
  (:export #:define-test #:run-test-set #:all-tests #:package-test-set
           #:symbol-test #:dependency-ordered-tests #:link-package-to-system))
(in-package :shcl/test/foundation)

(optimization-settings)

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
  "Run the test functions in the given sequence."
  (let ((*suite* (make-instance 'suite :plan nil)))
    (do-sequence (test tests)
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

(defvar *package-system-linkage-map* (fset:empty-map))

(defun link-package-to-system (system-name &optional (this-package *package*))
  "Record that the package named by `this-package' contains tests for
`system-name'.

Presently, this information is only used for
`dependency-ordered-tests'."
  (setf system-name (asdf:component-name (asdf:find-system system-name)))
  (setf this-package (ensure-package this-package))
  (setf (fset:lookup *package-system-linkage-map* this-package)
        system-name)
  (values))

(defun dependency-ordered-tests ()
  "Get an ordered collection of all tests defined with `define-test'.

This returns the same tests that `all-tests' does, but it attempts to
order the tests such that more foundational tests occur earlier than
more derrived tests.  For example, if ASDF system A depends on ASDF
system B, this function will try to put tests for system B before the
tests for system A.

In order for this to work, it must be possible to figure out which
ASDF system a given test is testing.  Since it is useful to define
tests in a seperate file (and often a seperate system) from the system
under test, a function is provided to explicitly link a package
containing tests to the system it tests.  See
`link-package-to-system'.

Tests that are not associated with an ASDF system will be run at the
end in an unspecified order."
  (let ((packages-without-linked-systems (fset:empty-set))
        (packages-with-linked-systems (fset:empty-set))
        (system-dependencies (fset:empty-map (fset:empty-set)))
        (sorted-packages (shcl/core/utility:make-extensible-vector)))
    (fset:do-map (package-designator tests *package-tests*)
      (declare (ignore tests))
      (let ((package (ensure-package package-designator)))
        (if (fset:lookup *package-system-linkage-map* package)
            (fset:adjoinf packages-with-linked-systems package)
            (fset:adjoinf packages-without-linked-systems package))))
    (labels
        ((examined-p (system-name)
           (nth-value 1 (fset:lookup system-dependencies system-name)))

         (examine (system-name)
           (when (examined-p system-name)
             (return-from examine))

           (shcl/core/debug:traverse-dependencies system-name (depender depended)
             (declare (ignore depender))
             (examine depended)
             (fset:adjoinf (fset:lookup system-dependencies system-name) depended)
             (fset:unionf (fset:lookup system-dependencies system-name)
                          (fset:lookup system-dependencies depended))
             nil))

         (package-< (left right)
           (setf left (fset:lookup *package-system-linkage-map* left))
           (setf right (fset:lookup *package-system-linkage-map* right))
           (let ((left-depends (fset:lookup system-dependencies left))
                 (right-depends (fset:lookup system-dependencies right)))
             (cond
               ((fset:member? right left-depends)
                nil)
               ((fset:member? left right-depends)
                t)
               (t
                (string< left right))))))

      (fset:do-set (package packages-with-linked-systems)
        (let ((linked-system (fset:lookup *package-system-linkage-map* package)))
          (examine linked-system))
        (vector-push-extend package sorted-packages))

      (setf sorted-packages (sort sorted-packages #'package-<))

      (fset:do-set (package packages-without-linked-systems)
        (vector-push-extend package sorted-packages))

      (eager-flatmap-sequence
       sorted-packages
       (lambda (package)
         (fset:lookup *package-tests* package))
       (fset:empty-seq)))))
