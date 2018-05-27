(defpackage :shcl/test/foundation
  (:use :common-lisp :prove :lisp-namespace)
  (:import-from :fset)
  (:export #:define-test))
(in-package :shcl/test/foundation)

(defvar *package-tests* (fset:empty-map (fset:empty-set)))

(define-namespace test function nil)

(defun ensure-package (package-designator)
  (if (typep package-designator 'package)
      package-designator
      (find-package package-designator)))

(defun install-test (name package)
  (fset:adjoinf (fset:lookup *package-tests* (ensure-package package)) name)
  (values))

(defmacro define-test (name &body body)
  (let ((package *package*))
    `(progn
       (setf (symbol-test ',name) (lambda () (subtest ,(format nil "~A::~A"
                                                               (package-name package)
                                                               (symbol-name name))
                                               ,@body)))
       (install-test ',name ,package)
       ',name)))

(defun run-test-set (tests)
  (let ((*suite* (make-instance 'suite :plan (fset:size tests))))
    (fset:do-set (test tests)
      (funcall (symbol-test test)))
    (finalize)))

(defun run-package-tests (package)
  (let ((tests (fset:lookup *package-tests* (ensure-package package))))
    (run-test-set tests)))

(defun run-all-tests ()
  (let ((all-tests (fset:empty-set)))
    (fset:do-map (package tests *package-tests*)
      (declare (ignore package))
      (fset:unionf all-tests tests))
    (run-test-set all-tests)))
