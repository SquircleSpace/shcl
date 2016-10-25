(defpackage :shcl-test/main
  (:use :common-lisp :shcl/main :shcl/builtin :prove))
(in-package :shcl-test/main)

(define-builtin testing-assert-equal (args)
  (let ((set (fset:convert 'fset:set (fset:less-first args))))
    (unless (equal 1 (fset:size set))
      (error "Non-equal arguments: ~A" (fset:convert 'list set))))
  (prove:pass "Strings matched")
  0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(deftest main
  (labels
      ((run (s) (is (run-shell-commands-in-string s) 0
                    :test #'equal)))
    (run "testing-assert-equal 0 0")
    (run "FOO=123 ; testing-assert-equal $FOO 123")))
