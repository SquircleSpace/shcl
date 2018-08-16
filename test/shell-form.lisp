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

(defpackage :shcl/test/shell-form
  (:use :common-lisp :prove :shcl/core/utility :shcl/core/shell-form
        :shcl/test/foundation)
  (:import-from :shcl/core/iterator #:make-iterator #:emit)
  (:import-from :shcl/core/environment #:env)
  (:import-from :shcl/core/fd-table #:with-fd-streams)
  (:import-from :shcl/core/exit-info
                #:exit-info-true-p #:exit-info-false-p #:truthy-exit-info
                #:falsey-exit-info #:exit-info #:exit-info-exit-status)
  (:import-from :fset))
(in-package :shcl/test/shell-form)

(optimization-settings)

(link-package-to-system :shcl/core/shell-form)

(define-test shell-not
  (let (evaluated-p)
    (ok (exit-info-true-p
         (shell-not
           (setf evaluated-p t)
           (falsey-exit-info)))
        "shell-not inverts exit status")
    (ok evaluated-p
        "shell-not evaluates all forms in its body"))
  (ok (exit-info-false-p
       (shell-not
         (truthy-exit-info)))
      "shell-not inverts exit status"))

(define-test shell-and
  (let (evaluated-p)
    (ok (exit-info-false-p
         (shell-and
           (truthy-exit-info)
           (falsey-exit-info)
           (progn
             (setf evaluated-p t)
             (truthy-exit-info))))
        "shell-and returns correct status")
    (ok (not evaluated-p)
        "shell-and short circuted"))
  (let (evaluated-p)
    (ok (exit-info-true-p
         (shell-and
           (truthy-exit-info)
           (progn
             (setf evaluated-p t)
             (truthy-exit-info))
           (truthy-exit-info)))
        "shell-and returns the correct status")
    (ok evaluated-p
        "shell-and didn't short circut prematurely")))

(define-test shell-or
  (let (evaluated-p)
    (ok (exit-info-true-p
         (shell-or
           (falsey-exit-info)
           (truthy-exit-info)
           (progn
             (setf evaluated-p t)
             (truthy-exit-info))))
        "shell-or returns correct status")
    (ok (not evaluated-p)
        "shell-and short circuted"))
  (let (evaluated-p)
    (ok (exit-info-true-p
         (shell-or
           (falsey-exit-info)
           (progn
             (setf evaluated-p t)
             (falsey-exit-info))
           (truthy-exit-info)))
        "shell-or returns the correct status")
    (ok evaluated-p
        "shell-or didn't short circut prematurely")))

(define-test shell-if
  (let (true-path-evaluated
        false-path-evaluated)
    (ok (equal 1
               (exit-info-exit-status
                (shell-if (truthy-exit-info)
                          (progn
                            (setf true-path-evaluated t)
                            (make-instance 'exit-info :exit-status 1))
                          (progn
                            (setf false-path-evaluated t)
                            (make-instance 'exit-info :exit-status 2)))))
        "Correct exit status returned")
    (ok true-path-evaluated
        "Truthy path was executed")
    (ok (not false-path-evaluated)
        "Falsey path was not executed"))
  (let (true-path-evaluated
        false-path-evaluated)
    (ok (equal 2
               (exit-info-exit-status
                (shell-if (falsey-exit-info)
                          (progn
                            (setf true-path-evaluated t)
                            (make-instance 'exit-info :exit-status 1))
                          (progn
                            (setf false-path-evaluated t)
                            (make-instance 'exit-info :exit-status 2)))))
        "Correct exit status returned")
    (ok (not true-path-evaluated)
        "Truthy path was not executed")
    (ok false-path-evaluated
        "Falsey path was not executed"))

  (ok (equal 2
             (exit-info-exit-status
              (shell-if (make-instance 'exit-info :exit-status 2)
                        (make-instance 'exit-info :exit-status 1))))
      "Exit info of condition is emitted when false and no else branch provided"))

(define-test shell-when
  (is (exit-info-exit-status
       (shell-when (make-instance 'exit-info :exit-status 2)
         (make-instance 'exit-info :exit-status 1)))
      2
      "Exit info of condition is emitted when false")

  (is (exit-info-exit-status
       (shell-when (truthy-exit-info)
         (make-instance 'exit-info :exit-status 2)))
      2
      "Exit info of body is emitted when condition is true"))

(define-test shell-unless
  (ok (exit-info-true-p
       (shell-unless (truthy-exit-info)
         (make-instance 'exit-info :exit-status 2)))
      "Exit info of condition is emitted when true")

  (is (exit-info-exit-status
       (shell-unless (make-instance 'exit-info :exit-status 3)
         (make-instance 'exit-info :exit-status 2)))
      2
      "Exit info of body is emitted when condition is false"))

(define-test shell-while
  (let ((status-list (list (truthy-exit-info)
                           (truthy-exit-info)
                           (falsey-exit-info)))
        (execution-count 0)
        status)
    (is (exit-info-exit-status
         (shell-while (setf status (pop status-list))
           (incf execution-count)
           (make-instance 'exit-info :exit-status 2)))
        2
        "The last form of the body is returned")
    (is execution-count 2
        "The body was executed twice")
    (ok (exit-info-false-p status)
        "The false condition was reached"))

  (ok (exit-info-true-p
       (shell-while (make-instance 'exit-info :exit-status 2)
         (fail "Body must not be executed")
         (make-instance 'exit-info :exit-status 3)))
      "While loops return true exit status if body never runs"))

(defun forever (value)
  (make-iterator ()
    (emit value)))

(define-test shell-for
  (let* ((string-list '("a" "b" "c"))
         (string-list-copy string-list)
         (execution-count 0))
    (is (exit-info-exit-status
         (shell-for ("var" string-list)
           (is (env "var") (pop string-list-copy)
               "Iteration works")
           (incf execution-count)
           (make-instance 'exit-info :exit-status 2)))
        2
        "The last form of the body is returned")
    (is execution-count 3
        "The body was executed every time"))

  (ok (exit-info-true-p
       (shell-for ("var" nil)
         (fail "Body must not be executed")
         (make-instance 'exit-info :exit-status 3)))
      "For loops return true exit status if body never runs"))

(defmacro with-continuator (continuator &body body)
  `(funcall ,continuator (lambda () ,@body)))

(define-test shell-break
  (labels
      ((call-with-infinite-while-loop (fn)
         (shell-while (truthy-exit-info)
           (funcall fn)))
       (call-with-infinite-for-loop (fn)
         (shell-for ("var" (forever "value"))
           (funcall fn)))
       (test-exit-info (looper-fn)
         (let ((loop-count 0))
           (is (exit-info-exit-status
                (with-continuator looper-fn
                  (incf loop-count)
                  (when (equal 3 loop-count)
                    (shell-break :exit-info (make-instance 'exit-info :exit-status 2)))
                  (make-instance 'exit-info :exit-status 3)))
               2
               "shell-break exits the loop")
           (is loop-count 3
               "The loop break'd at the right time")))

       (test-count (looper-fn)
         (let ((outer-loop-count 0)
               (middle-loop-count 0)
               (inner-loop-count 0))
           (is (exit-info-exit-status
                (with-continuator looper-fn
                  (with-continuator looper-fn
                    (with-continuator looper-fn
                      (when (equal 3 inner-loop-count)
                        (shell-break :exit-info (make-instance 'exit-info :exit-status 2)))
                      (incf inner-loop-count))
                    (when (equal 3 middle-loop-count)
                      (shell-break :count 2 :exit-info (make-instance 'exit-info :exit-status 3)))
                    (incf middle-loop-count))
                  (fail "Control should never reach this point")
                  (shell-break)))
               3
               "shell-break respects count argument")
           (ok (and (equal inner-loop-count 3)
                    (equal middle-loop-count 3)
                    (equal outer-loop-count 0))
               "The loop break'd at the right time"))))

    (subtest "Breaking out of while loops"
      (test-exit-info #'call-with-infinite-while-loop)
      (test-count #'call-with-infinite-while-loop))

    (subtest "Breaking out of for loops"
      (test-exit-info #'call-with-infinite-for-loop)
      (test-count #'call-with-infinite-for-loop))))

(define-test shell-pipeline
  (ok
   (exit-info-true-p
    (shell-pipeline
      (with-fd-streams ()
        (format *standard-output* "foo~%")
        (falsey-exit-info))
      (with-fd-streams ()
        (format *standard-output* "~A~%" (length (read-line)))
        (falsey-exit-info))
      (with-fd-streams ()
        (if (equal "3" (read-line))
            (truthy-exit-info)
            (falsey-exit-info)))))
   "Information flowed through the pipeline, last exit status is used")

  (ok
   (exit-info-false-p
    (shell-pipeline
      (with-fd-streams ()
        (format *standard-output* "foobar~%")
        (truthy-exit-info))
      (with-fd-streams ()
        (format *standard-output* "~A~%" (length (read-line)))
        (truthy-exit-info))
      (with-fd-streams ()
        (if (equal "3" (read-line))
            (truthy-exit-info)
            (falsey-exit-info)))))
   "Information flowed through the pipeline, last exit status is used")

  (setf (env "foo") "123")
  (ok
   (exit-info-true-p
    (shell-pipeline
      (truthy-exit-info)
      (prog1
          (if (equal (env "foo") "123")
              (truthy-exit-info)
              (falsey-exit-info))
        (setf (env "foo") "abc"))))
   "Pipelines inherit the shell environment where they are created")
  (is (env "foo")
      "123"
      "Pipelines run in subshells"))
