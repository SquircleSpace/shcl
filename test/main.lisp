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
  (:import-from :shcl/test/foundation
                #:run-test-set #:all-tests #:package-test-set #:symbol-test)
  (:import-from :shcl/core/command)
  (:import-from :prove)
  (:import-from :fset)
  (:export #:run-all-tests))
(in-package :shcl/test/main)

(shcl/core/command:define-builtin -shcl-run-tests (&option package)
  "Run unit tests."
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
        1)))
