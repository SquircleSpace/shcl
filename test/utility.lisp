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

(defpackage :shcl/test/utility
  (:use :common-lisp :prove :shcl/core/utility :shcl/test/foundation))
(in-package :shcl/test/utility)

(optimization-settings)

(define-condition test-condition ()
  ())

(defun signals-test ()
  (signal 'test-condition))

(define-hook test-hook)

(define-test hooks
  (define-hook test-hook)
  (add-hook test-hook 'signals-test)
  (is-condition (run-hook test-hook) test-condition)
  (remove-hook test-hook 'signals-test)
  (handler-case (progn (run-hook test-hook) (pass "No signal expected"))
    (test-condition () (fail "Unexpected signal")))
  (add-hook test-hook 'signals-test)
  (define-hook test-hook)
  (handler-case (progn (run-hook test-hook) (pass "No signal expected"))
    (test-condition () (fail "Unexpected signal"))))

(define-test when-let-tests
  (is (when-let ((a (+ 1 2))
                 (b (format nil "asdf"))
                 (c (not 'not))
                 (d (error "This form shouldn't be evaluated")))
        (error "Failure"))
      nil)
  (ok (when-let ((a t)
                 (b t)
                 (c t))
        (is a t)
        (is b t)
        (is c t)
        (and a b c))))

(define-test try-tests
  (is
   (try (progn (throw 'baz 123))
     (bap () 'xyz)
     (baz (value) (is value 123 :test #'equal) 'foobar))
   'foobar))

(define-test threading-macros
  (is-expand (-> 0 (+ 1) (abs) #'-)
             (as-> 0 $ (+ $ 1) (abs $) (funcall #'- $))
             "-> works")
  (is-expand (->> 0 (+ 1) (abs) #'-)
             (as-> 0 $ (+ 1 $) (abs $) (funcall #'- $))
             "->> works")
  (is-expand (as-> 0 v (+ 1 v) (abs v) (funcall #'- v))
             (let* ((v 0)
                    (v (+ 1 v))
                    (v (abs v))
                    (v (funcall #'- v)))
               v)
             "as-> works"))
