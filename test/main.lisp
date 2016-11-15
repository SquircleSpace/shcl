(defpackage :shcl-test/main
  (:use :common-lisp :shcl/builtin :shcl/exit-info :prove :shcl/lisp-interpolation))
(in-package :shcl-test/main)

(plan 1)

(define-builtin testing-assert-equal (args)
  (let ((set (fset:convert 'fset:set (fset:less-first args))))
    (if (equal 1 (fset:size set))
        (progn
          (pass "Strings matched")
          0)
        (progn
          (fail (format nil "Strings didn't match ~A" set))
          1))))

(define-builtin testing-assert-unequal (args)
  (let ((set (fset:convert 'fset:set (fset:less-first args))))
    (if (equal (fset:size (fset:less-first args)) (fset:size set))
        (progn
          (pass "Strings didn't match")
          0)
        (progn
          (fail (format nil "Strings match ~A" args))
          1))))

(defun run (s &optional description)
  (ok (exit-info-true-p
       (evaluate-shell-string s))
      (or description (format nil "Command exited with code 0: ~A" s))))

(deftest main
  (run "testing-assert-equal 0 0"
       "Equality comparison works")
  (run "testing-assert-unequal 1 0"
       "Inequality comparison works")

  (run "FOO=123 ; testing-assert-equal $FOO 123"
       "Variable expansion works")

  (run "testing-assert-equal '' \"$(for VAR in ; do
echo $VAR
done)\""
       "For loops over empty lists work")
  (run "testing-assert-equal '1
2
3
' \"$(for VAR in 1 2 3
do
echo $VAR
done)\""
       "For loops over non-empty lists work")
  (run "testing-assert-unequal $(for VAR in 1 2 3
do
echo $VAR
done)"
       "For loops over non-empty lists work"))
