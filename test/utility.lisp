(defpackage :shcl/test/utility
  (:use :common-lisp :prove :shcl/core/utility))
(in-package :shcl/test/utility)

(optimization-settings)

(plan 5)

(defparameter *value* 0)
(define-once-global test-global (incf *value*))

(deftest once-global
  (is 1 test-global)
  (is 1 test-global)
  (is 1 *value*))

(define-condition test-condition ()
  ())

(defun signals-test ()
  (signal 'test-condition))

(define-hook test-hook)

(deftest hooks
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

(deftest when-let-tests
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

(deftest try-tests
  (is
   (try (progn (throw 'baz 123))
     (bap () 'xyz)
     (baz (value) (is value 123 :test #'equal) 'foobar))
   'foobar))

(deftest threading-macros
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
