(defpackage :shcl-test/utility
  (:use :common-lisp :prove :shcl/utility))
(in-package :shcl-test/utility)

(defparameter *once-eval-when-value* 0)
(eval-once-when (:compile-toplevel :load-toplevel :execute)
  (incf *once-eval-when-value*))

(defparameter *once-when-fn-value* 0)

;; So we don't get warnings
(defun once-when-in-function ()
  *once-when-fn-value*)

(deftest once-when
  (is 1 *once-eval-when-value*)
  (setf *once-when-fn-value* 0)
  ;; Redefine the function so that we get a new once token
  (eval '(defun once-when-in-function ()
          (eval-once-when (:execute)
           (incf *once-when-fn-value*))
          *once-when-fn-value*))
  (once-when-in-function)
  (is 1 *once-when-fn-value*)
  (once-when-in-function)
  (is 1 *once-when-fn-value*))

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

(deftest iterator-tests
  (let* ((vector #(1 2 3 4 5))
         (list '(a b c d e))
         (seq (fset:seq 'q 'w 'e 'r))
         (vector-iterator (vector-iterator vector))
         (list-iterator (list-iterator list))
         (generic-iterator (iterator seq)))
    (is (coerce (iterator-values vector-iterator) 'list)
        (coerce vector 'list)
        :test #'equal)
    (is (coerce (iterator-values list-iterator) 'list)
        list
        :test #'equal)
    (is (coerce (iterator-values generic-iterator) 'list)
        (fset:convert 'list seq)
        :test #'equal)))

(deftest lookahead-iterator-tests
  (let* ((count 5)
         (iter (make-iterator (:type 'lookahead-iterator)
                 (when (equal 0 count)
                   (stop))
                 (decf count)))
         fork)
    (is 5 count :test #'equal)
    (is 4 (peek-lookahead-iterator iter) :test #'equal)
    (is 4 count :test #'equal)
    (setf fork (fork-lookahead-iterator iter))
    (is 4 (peek-lookahead-iterator fork) :test #'equal)
    (is 4 (peek-lookahead-iterator iter) :test #'equal)
    (is 4 count :test #'equal)
    (is 4 (next iter) :test #'equal)
    (is 3 (next iter) :test #'equal)
    (is 2 (next iter) :test #'equal)
    (is 2 count :test #'equal)
    (is 4 (peek-lookahead-iterator fork) :test #'equal)
    (is 2 count :test #'equal)
    (is 4 (next fork) :test #'equal)
    (is 3 (next fork) :test #'equal)
    (is 2 count :test #'equal)
    (is 1 (next iter) :test #'equal)
    (is 1 count :test #'equal)
    (move-lookahead-to fork iter)
    (is 0 (next iter) :test #'equal)
    (is 0 count :test #'equal)
    (is nil (next iter) :test #'equal)
    (is 0 count :test #'equal)
    (is 0 (next fork) :test #'equal)
    (is 0 count :test #'equal)
    (is nil (next fork) :test #'equal)
    (is 0 count :test #'equal)))
