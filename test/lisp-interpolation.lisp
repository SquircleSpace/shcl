(defpackage :shcl-test/lisp-interpolation
  (:use :common-lisp :prove :shcl/lisp-interpolation :shcl/exit-info))
(in-package :shcl-test/lisp-interpolation)

(plan 3)

(defun shell (str)
  (evaluate-shell-string str))

(defun exit-ok (thing)
  (ok (and (typep thing 'exit-info)
           (exit-info-true-p thing))
      "Expecting truthy status"))

(defun exit-fail (thing)
  (ok (and (typep thing 'exit-info)
           (exit-info-false-p thing))
      "Expecting falsey status"))

(deftest check-result
  (exit-ok (shell "true"))
  (exit-fail (shell "false"))
  (exit-ok (check-result () (shell "true")))
  (exit-fail
   (let (signaled)
     (handler-bind
         ((exit-failure
           (lambda (c)
             (setf signaled t)
             (pass "exit-failure signal is expected")
             (continue c))))
       (let ((result (check-result () (shell "false"))))
         (unless signaled
           (fail "exit-failure signal is expected"))
         result)))))

(deftest capture
  (is (capture (:stdout) (shell "echo   \f'o'\"o\"   "))
      (format nil "foo~%")
      :test #'equal)
  (is (capture (:stdout :stderr 3) (shell "echo foo && echo bar >&2 ; echo baz >&3"))
      (format nil "foo~%bar~%baz~%")
      :test #'equal))

(deftest splice
  (let ((lexical-variable "ABC"))
    (is (capture (:stdout) (evaluate-constant-shell-string "echo ,lexical-variable" :readtable *splice-table*))
        (format nil "ABC~%")
        :test #'equal))

  (let ((lexical-vector #("A " "b")))
    (is (capture (:stdout) (evaluate-constant-shell-string "echo ,@lexical-vector" :readtable *splice-table*))
        (format nil "A  b~%")
        :test #'equal))

  (let ((lexical-seq (fset:seq "A " "b")))
    (is (capture (:stdout) (evaluate-constant-shell-string "echo ,@lexical-seq" :readtable *splice-table*))
        (format nil "A  b~%")
        :test #'equal)))
