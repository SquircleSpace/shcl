(defpackage :shcl/test/main
  (:use :common-lisp)
  (:import-from :shcl/test/lexer)
  (:import-from :shcl/test/utility)
  (:import-from :shcl/test/posix)
  (:import-from :shcl/test/lisp-interpolation)
  (:import-from :shcl/test/data)
  (:import-from :shcl/test/iterator)
  (:import-from :shcl/test/command)
  (:import-from :shcl/core/command)
  (:import-from :prove)
  (:export #:run-tests))
(in-package :shcl/test/main)

(defun run-tests (&key enable-colors (output-stream *standard-output*))
  (let ((prove:*enable-colors* enable-colors)
        (prove:*test-result-output* output-stream))
    (prove:run-test-all)))

(shcl/core/command:define-builtin -shcl-run-tests ()
  "Run all unit tests."
  (run-tests)
  0)
