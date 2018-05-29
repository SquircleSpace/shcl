(defpackage :shcl/test/foundation
  (:use :common-lisp :prove :lisp-namespace)
  (:import-from :fset)
  (:export #:define-test))
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

(defun run-package-tests (package)
  "Run the tests in the given test package."
  (let ((tests (fset:lookup *package-tests* (ensure-package package))))
    (run-test-set tests)))

(defun run-all-tests ()
  "Run all tests registered with `define-test'."
  (let ((all-tests (fset:empty-set)))
    (fset:do-map (package tests *package-tests*)
      (declare (ignore package))
      (fset:unionf all-tests tests))
    (run-test-set all-tests)))
