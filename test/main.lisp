(defpackage :shcl-test/main
  (:use :common-lisp)
  (:import-from :shcl/core/builtin)
  (:import-from :prove)
  (:import-from :fset)
  (:export #:run-tests))
(in-package :shcl-test/main)

(defun run-tests (&key enable-colors (output-stream *standard-output*))
  (let ((prove:*enable-colors* enable-colors)
        (prove:*test-result-output* output-stream))
    (prove:run-test-all)))

(shcl/core/builtin:define-builtin -shcl-run-tests (args)
  (unless (equal 1 (fset:size args))
    (format *error-output* "Invalid number of arguments: ~A~%" (1- (fset:size args)))
    (return-from -shcl-run-tests 2))

  (run-tests)
  0)
