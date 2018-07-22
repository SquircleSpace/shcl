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

(defpackage :shcl/test/lisp-interpolation
  (:use :common-lisp :prove :shcl/core/utility :shcl/core/lisp-interpolation
        :shcl/core/exit-info :shcl/core/command :shcl/test/foundation)
  (:shadow #:run))
(in-package :shcl/test/lisp-interpolation)

(optimization-settings)

(defun shell (str)
  (evaluate-shell-string str))

(defun exit-ok (thing &optional description)
  (unless (typep thing 'exit-info)
    (fail description)
    (return-from exit-ok))
  (ok (exit-info-true-p thing)
      (or description "Expecting truthy status")))

(defun exit-fail (thing &optional description)
  (unless (typep thing 'exit-info)
    (fail description)
    (return-from exit-fail))
  (ok (exit-info-false-p thing)
      (or description "Expecting falsey status")))

(define-builtin -shcl-assert-equal (&rest args)
  "Exit 0 iff all arguments are the same."
  (let ((set (fset:convert 'fset:set args)))
    (if (equal 1 (fset:size set))
        (progn
          0)
        (progn
          1))))

(define-builtin -shcl-assert-unequal (&rest args)
  "Exit 0 iff all arguments are different."
  (let ((set (fset:convert 'fset:set args)))
    (if (equal (length args) (fset:size set))
        (progn
          0)
        (progn
          1))))

(define-builtin -shcl-assert-stdin-equal (&required str)
  "Exit 0 iff stdin is equal to the provided string.

A single newline character is removed from the end of stdin.  If the
stdin content does not end in a newline character then this command
indicates test failure."
  (check-type str string)
  (let ((stdin-str (make-extensible-vector :element-type 'character)))
    (loop :for char = (read-char *standard-input* nil :eof)
       :until (eq char :eof) :do
       (vector-push-extend char stdin-str))
    (unless (equal #\newline (vector-pop stdin-str))
      (return-from -shcl-assert-stdin-equal 1))
    (if (string= str stdin-str)
        0
        1)))

(defun run (s &optional description)
  (ok (exit-info-true-p
       (evaluate-shell-string s))
      (or description (format nil "Command exited with code 0: ~A" s))))

(define-test check-result
  (exit-ok
   (shell "true")
   "True is true")
  (exit-fail
   (shell "false")
   "False is false")
  (exit-ok
   (check-result (shell "true"))
   "check-result doesn't interfere with true")
  (exit-fail
   (let (signaled)
     (handler-bind
         ((exit-failure
           (lambda (c)
             (setf signaled t)
             (pass "exit-failure signal is expected")
             (continue c))))
       (let ((result (check-result (shell "false"))))
         (unless signaled
           (fail "exit-failure signal is expected"))
         result)))))

(define-test capture
  (is (capture (:streams '(:stdout)) (shell "echo   \f'o'\"o\"   "))
      (format nil "foo~%")
      :test #'equal
      "Capturing works for basic strings")
  (is (capture (:streams '(:stdout :stderr 3)) (shell "echo foo && echo bar >&2 ; echo baz >&3"))
      (format nil "foo~%bar~%baz~%")
      :test #'equal
      "Capturing works for slightly more complex strings"))

(define-test splice
  (let ((lexical-variable "ABC"))
    (is (capture (:streams '(:stdout)) (evaluate-constant-shell-string "echo ,lexical-variable" :readtable *splice-table*))
        (format nil "ABC~%")
        :test #'equal
        "Splicing a lexical string works"))

  (let ((lexical-vector #("A " "b")))
    (is (capture (:streams '(:stdout)) (evaluate-constant-shell-string "echo ,@lexical-vector" :readtable *splice-table*))
        (format nil "A  b~%")
        :test #'equal
        "Splicing a lexical vector works"))

  (let ((lexical-seq (fset:seq "A " "b")))
    (is (capture (:streams '(:stdout)) (evaluate-constant-shell-string "echo ,@lexical-seq" :readtable *splice-table*))
        (format nil "A  b~%")
        :test #'equal
        "Splicing a lexical seq works")))

(define-test test-infrastructure
  (run "-shcl-assert-equal 0 0"
       "Equality comparison works")
  (run "-shcl-assert-unequal 1 0"
       "Inequality comparison works"))

(define-test variables
  (run "FOO=123 ; -shcl-assert-equal \"$FOO\" 123"
       "$variable expansion works")
  (run "FOO=123 ; -shcl-assert-equal \"${FOO}\" 123"
       "${variable} expansion works")
  (run "FOO=123 ; -shcl-assert-equal ${#FOO} 3"
       "${#variable} expansion works"))

(define-test for-loops
  (run "-shcl-assert-equal '' \"$(for VAR in ; do
echo $VAR
done)\""
       "For loops over empty lists work")
  (run "-shcl-assert-equal '1
2
3
' \"$(for VAR in 1 2 3
do
echo $VAR
done)\""
       "For loops over non-empty lists work")
  (run "-shcl-assert-unequal $(for VAR in 1 2 3
do
echo $VAR
done)"
       "For loops over non-empty lists work"))

(define-test if
  (exit-fail
   (shell "if true ; then true ; false ; fi")
   "Last exit code of then block is exit code of if")

  (exit-ok
   (shell "if true ; then true ; true ; fi")
   "Last exit code of then block is exit code of if")

  (is (capture (:streams '(:stdout)) (shell "if echo condition ; then echo truthy ; else echo falsey ; fi"))
      (format nil "condition~%truthy~%")
      :test #'equal
      "Only the true branch runs")

  (is (capture (:streams '(:stdout)) (shell "if echo condition && false ; then echo truthy ; else echo falsey ; fi"))
      (format nil "condition~%falsey~%")
      :test #'equal
      "Only the else branch runs")

  (is (capture (:streams '(:stdout)) (shell "if echo condition && false ; then echo truthy ; elif echo condition2 ; then echo elif-branch ; fi"))
      (format nil "condition~%condition2~%elif-branch~%")
      :test #'equal
      "Only the elif branch runs"))

(define-test bang
  (exit-fail
   (shell "! true"))
  (exit-ok
   (shell "! false")))

(define-test while
  (let ((count 0))
    (is (capture (:streams '(:stdout)) (evaluate-constant-shell-string "while [ ,count -ne 3 ]; do echo ,(incf count) ; done" :readtable *splice-table*))
        (format nil "1~%2~%3~%")
        :test #'equal
        "Basic while loop test")))

(define-test empty
  (exit-ok
   (shell "")
   "Empty string is truthy")
  (exit-ok
   (shell (format nil "~C" #\newline))
   "Single newline is truthy")
  (exit-ok
   (shell (format nil " ~C ~C " #\newline #\tab))
   "Miscellaneous whitespace is truthy"))

;; We might normally just use wc, but the output of wc can work
;; differely on some operating systems.
(define-builtin -shcl-char-count ()
  (let ((count 0))
    (loop :for char = (read-char *standard-input* nil :eof)
       :until (eq char :eof) :do
       (incf count))
    (format t "~A~%" count)
    0))

(define-test pipeline
  (exit-ok
   (shell "false | false | true")
   "Last exit code wins")

  (exit-fail
   (shell "true | true | false")
   "Last exit code wins")

  (exit-ok
   (shell "echo foob | -shcl-assert-stdin-equal foob")
   "One-stage pipeline works")

  (exit-ok
   (shell "echo foob | -shcl-char-count | -shcl-assert-stdin-equal 5")
   "Two-stage pipeline works")

  (exit-ok
   (shell "{ echo foo >&2; } 2>&1 | -shcl-assert-stdin-equal foo")
   "Nested redirects work"))
