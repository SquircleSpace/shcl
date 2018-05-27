(defpackage :shcl/test/main
  (:use :common-lisp)
  (:import-from :shcl/test/lexer)
  (:import-from :shcl/test/utility)
  (:import-from :shcl/test/posix)
  (:import-from :shcl/test/lisp-interpolation)
  (:import-from :shcl/test/data)
  (:import-from :shcl/test/iterator)
  (:import-from :shcl/test/command)
  (:import-from :shcl/test/foundation #:run-all-tests)
  (:import-from :shcl/core/command)
  (:import-from :prove)
  (:export #:run-all-tests))
(in-package :shcl/test/main)

(shcl/core/command:define-builtin -shcl-run-tests ()
  "Run all unit tests."
  (let ((prove:*enable-colors* nil)
        (prove:*test-result-output* *standard-output*))
    (if (run-all-tests)
        0
        1)))
