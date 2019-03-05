;; Copyright 2019 Bradley Jensen
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

(defpackage :shcl/test/parser-2
  (:use :common-lisp :shcl/core/parser-2 :shcl/test/foundation :prove)
  (:import-from :shcl/core/advice #:define-advice)
  (:import-from :shcl/core/utility #:optimization-settings)
  (:import-from :fset)
  (:import-from :shcl/core/sequence #:head #:tail #:empty-p #:attach #:empty-of #:walk #:attachf))
(in-package :shcl/test/parser-2)

(optimization-settings)

(link-package-to-system :shcl/core/parser-2)

(defun compare-equal-p (left right)
  (eq :equal (fset:compare left right)))

(define-test parser-throw
  (block nil
    (catch shcl/core/parser-2::+parser-error+
      (parser-throw)
      (fail "parser-throw failed to throw")
      (return))
    (pass "parser-throw actually threw!"))

  (let ((shcl/core/parser-2::*parser-errors* (fset:seq 1 2 3)))
    (catch shcl/core/parser-2::+parser-error+
      (parser-throw 4))
    (is shcl/core/parser-2::*parser-errors* (fset:seq 1 2 3 4)
        "parser-throw adds the thrown error to *parser-errors*"
        :test 'compare-equal-p)
    (catch shcl/core/parser-2::+parser-error+
      (parser-throw))
    (is shcl/core/parser-2::*parser-errors* (fset:seq 1 2 3 4)
        "parser-throw doesn't touch *parser-errors* if its not given a value"
        :test 'compare-equal-p)))

(defmacro parse-with-sequence-capture-sequence (sequence &body body)
  ;; We don't use the returned sequence tip because we want to know
  ;; what the tip was even when a parse error occurred.
  (let ((last-sequence-value (gensym "LAST-SEQUENCE-VALUE")))
    `(let (,last-sequence-value)
       (handler-case
           (parse-with-sequence ,sequence
             (unwind-protect
                  (progn ,@body)
               (setf ,last-sequence-value shcl/core/parser-2::*parser-sequence*)))
         (parse-failure ()))
       ,last-sequence-value)))

(define-test parser-lookahead
  (is (parse-with-sequence-capture-sequence '(1 2 3 4)
        (parser-lookahead
          (parse-object-of-type '(eql 5))))
      '(2 3 4)
      "parser-lookahead mutates sequence on parse error")
  (is (parse-with-sequence-capture-sequence '(1 2 3 4)
        (parser-lookahead
          (parse-object-of-type '(eql 1))))
      '(1 2 3 4)
      "parser-lookahead doesn't mutate sequence on parse success"))

(define-test parser-try
  (is (parse-with-sequence-capture-sequence '(1 2 3 4)
        (parser-try
          (parse-object-of-type '(eql 1))))
      '(2 3 4)
      "parser-try mutates sequence on parse success")
  (is (parse-with-sequence-capture-sequence '(1 2 3 4)
        (parser-try
          (parse-object-of-type '(eql 5))))
      '(1 2 3 4)
      "parser-try doesn't mutate sequence on parse failure"))

(define-test parser-choice
  (parse-with-sequence '(1 2 3 4)
    (let (evaluated-p)
      (is (multiple-value-list
           (parser-choice
             (progn
               (parser-throw)
               (fail "parser-throw is expected to cause the next alternative to be evaluated"))
             (progn
               (parser-throw)
               (fail "parser-throw is expected to cause the next alternative to be evaluated"))
             (progn
               (values 1 2 3))
             (progn
               (setf evaluated-p t)
               (values 4 5 6))))
          '(1 2 3)
          "parser-choice returns the first passing alternative")
      (ok (not evaluated-p)
          "parser-choice stops evaluating alternatives when one passes"))

    (is-error (parse-with-sequence '(1 2 3 4)
                (parser-choice
                  (progn
                    (parse-object-of-type '(integer 0))
                    (parser-throw))
                  (values 5 6 7 8)))
              'parse-failure
              "parser-choice re-throws errors after input has been consumed")

    (is-error (parse-with-sequence '(1 2 3 4)
                (parser-choice
                  (parser-throw)))
              'parse-failure
              "parser-choice passes on failures in its final form")

    (parse-with-sequence '(1 2 3 4)
      (is (multiple-value-list
           (parser-choice
             (values 5 6 7 8)))
          '(5 6 7 8)
          "parser-choice returns the value of its final form"))

    (is-error (parse-with-sequence '(1 2 3 4)
                (parser-choice))
              'parse-failure
              "parser-choice with no alternatives is a parse error")))

(define-test parse-eof
  (handler-case
      (parse-with-sequence '(1 2 3 4)
        (parse-eof)
        (fail "parse-eof is supposed to throw"))
    (parse-failure (e)
      (is-type (parse-failure-error-object e)
               'expected-eof
               "parse-eof throws an expected-eof error")))

  (is (parse-with-sequence-capture-sequence '(1 2 3 4)
        (parse-eof))
      '(1 2 3 4)
      "parse-eof doesn't consume sequence")

  (is (parse-with-sequence nil
        (parse-eof))
      :eof
      "parse-eof returns :eof"))

(define-test parse-object-of-type
  (handler-case
      (parse-with-sequence '(1 2 3 4)
        (parse-object-of-type 'string)
        (fail "parse-object-of-type is supposed to throw"))
    (parse-failure (e)
      (is-type (parse-failure-error-object e)
               'type-mismatch
               "parse-object-of-type throws a type-mismatch error")))

  (is (parse-with-sequence-capture-sequence '(1 2 3 4)
        (parse-object-of-type 'string))
      '(2 3 4)
      "parse-object-of-type consumes sequence even when it fails")

  (is (parse-with-sequence-capture-sequence '(1 2 3 4)
        (parse-object-of-type 'integer))
      '(2 3 4)
      "parse-object-of-type consumes sequence when it succeeds")

  (is (parse-with-sequence '(1 2 3 4)
        (parse-object-of-type '(eql 1)))
      1
      "parse-object-of-type produces expected value"))

(define-test parser-repeat-times
  (is (parse-with-sequence '(3 4 5 "abc")
        (prog1
            (parser-repeat-times ((fset:seq 1 2))
              (parser-try
                (parse-object-of-type '(integer 0))))
          (is (parse-object-of-type 'string)
              "abc"
              "parser-repeat-times left the sequence in the expected state")))
      (fset:seq 1 2 3 4 5)
      "parser-repeat-times attached parsed values onto the output sequence"
      :test 'compare-equal-p)

  (is-error (parse-with-sequence '(1 2 3 "abc" 4 5 6)
              (parser-repeat-times (nil 4)
                (parser-try
                  (parse-object-of-type '(integer 0)))))
            'parse-failure
            "parser-repeat-times enforces the minimum")

  (is (parse-with-sequence '(1 2 3 4 5 6 7)
        (parser-repeat-times ((fset:empty-set) nil 4)
          (parse-object-of-type 'fixnum)))
      (fset:set 3 4 2 1) ;; seT not seQ.  Order doesn't matter
      "parser-repeat-times enforces the maximum"
      :test 'compare-equal-p)

  (is-error (parse-with-sequence '(1 2 3 "abc" 4 5 6)
              (parser-repeat-times (nil 3)
                (parse-object-of-type '(integer 0))))
            'parse-failure
            "parser-repeat-times fails if the parser fails after consuming input"))

(define-test parser-repeat-until
  (is (parse-with-sequence '(1 2 3 4 5 6)
        (prog1
            (parser-repeat-until ((fset:seq 1 2) (parser-try (parse-object-of-type '(eql 5))))
              (parse-object-of-type 'number))
          (is (parse-object-of-type 'real)
              6
              "parser-repeat-until left the sequence in the expected state")))
      (fset:seq 1 2 1 2 3 4)
      "parser-repeat-times returns the expected value"
      :test 'compare-equal-p)

  (let ((fuse 4)
        (runs 0))
    (is-error (parse-with-sequence '(1 2 3 4 5 6 7 8 9)
                (parser-repeat-until (nil (progn
                                            (decf fuse)
                                            (if (zerop fuse)
                                                (parse-object-of-type 'string)
                                                (parser-throw))))
                  (incf runs)
                  (parse-object-of-type 't)))
              'parse-failure
              "consuming input and failing in end-parser-form results in a parse failure")
    (is runs 3
        "parser-repeat-until ran the expected number of times"))

  (is-error (parse-with-sequence '(1 2 3)
              (parser-repeat-until (nil (parser-throw))
                (parser-throw)))
            'parse-failure
            "throwing in the body is a parse error"))

(define-test parse-with-sequence
  (let* (error-1
         error-2
         (returned-values
           (multiple-value-list
            (parse-with-sequence '(1 2 3 4)
              (parse-object-of-type '(integer 0))
              (setf (parser-var 'sym) 'abc)
              (parser-choice
                  (progn
                    (setf (parser-var 'other-sym) 'def)
                    (parser-throw (setf error-1 (make-instance 'expected-eof :got 5))))
                nil)
              (parser-choice
                  (parser-throw (setf error-2 (make-instance 'type-mismatch :got 6 :expected-type 'string)))
                nil)
              (parse-object-of-type '(eql 2))
              10))))
    (is returned-values
        (list 10 '(3 4) (fset:map ('sym 'abc)) (fset:seq error-1 error-2))
        "parse-with-sequence returns expected values"
        :test 'compare-equal-p))

  (is-error (parse-with-sequence '(1 2)
              (parse-object-of-type 'string))
            'parse-failure
            "Parse errors become signaled errors"))

(define-test parser-var
  (is-error (parser-var 'sym) 'error
            "Attempting to access a parser-var outside of a parse is an error")
  (parse-with-sequence '(1 2 3 4)
    (is shcl/core/parser-2::*parser-vars*
        (fset:empty-map)
        "parser-var state is empty at the start of a parse"
        :test 'compare-equal-p)
    (is (multiple-value-list (parser-var 'sym))
        '(nil nil)
        "parser-var returns expected values for unbound var")
    (setf (parser-var 'sym) 456)
    (is (multiple-value-list (parser-var 'sym))
        '(456 t)
        "parser-var returns expected values for bound var")
    (parser-choice
      (progn (setf (parser-var 'sym) 123)
             (is (multiple-value-list (parser-var 'sym))
                 '(123 t)
                 "parser-var returns expected values for bound var")
             (parser-throw))
      nil)
    (is (multiple-value-list (parser-var 'sym))
        '(456 t)
        "parser-var state backtracks when the parser backtracks")))

(define-test unset-parser-var
  (parse-with-sequence '(1 2 3 4)
    (is (multiple-value-list (unset-parser-var 'sym))
        nil
        "parser-var returns no values")
    (setf (parser-var 'sym) 456)
    (parser-choice
      (progn (unset-parser-var 'sym)
             (is (multiple-value-list (parser-var 'sym))
                 '(nil nil)
                 "parser-var returns expected values for unbound var")
             (parser-throw))
      nil)
    (is (multiple-value-list (parser-var 'sym))
        '(456 t)
        "parser-var state backtracks when the parser backtracks")))

(define-terminal string parse-string-test)

(deftype other-type ()
  'fixnum)

(define-terminal other-type)

(define-terminal symbol parse-symbol-test)

(define-advice parse-symbol-test :around other-options ()
  (parser-choice
    (call-next-method)
    (parser-try
      (let ((object (parse-object-of-type 'string)))
        (if (equal "abc" object)
            object
            (parser-throw))))))

(define-test define-terminal
  (parse-with-sequence '("foo" "bar" 1 "abc" def)
    (is (parse-string-test)
        "foo"
        "Simple parse of a terminal")

    (is (parser-choice
          (parse-other-type)
          123)
        123
        "terminals don't consume input on parse failure")

    (parse-string-test)

    (is (parse-other-type)
        1
        "Simple parse of a terminal")

    (is (parse-symbol-test)
        "abc"
        "Around methods on terminals work")

    (is (parse-symbol-test)
        'def
        "Around methods on terminals work")))

(define-nonterminal root-test
  (mid-test (leaf-test (list (parse-leaf-test))) &optional other-type (other-type-2 (list (parse-other-type))))
  (other-type (other-type-2 (parse-other-type)))
  parse-symbol-test)

(define-nonterminal root-or-nil-test
  (mid-test (leaf-test (list (parse-leaf-test))) &optional other-type (other-type-2 (list (parse-other-type))))
  (other-type (other-type-2 (parse-other-type)))
  parse-symbol-test
  nil)

(define-nonterminal mid-test
  (string-test)
  parse-leaf-test)

(define-nonterminal leaf-test
  ((slot (parser-try (parse-object-of-type '(integer 3))))))

(define-test define-nonterminal
  (is (parse-with-sequence '("string" 4 10)
        (parse-root-test))
      (make-instance 'root-test
                     'mid-test (make-instance 'mid-test 'string-test "string")
                     'leaf-test (list (make-instance 'leaf-test 'slot 4))
                     'other-type 10
                     'other-type-2 nil)
      "Parsing a nonterminal with no backtracking works"
      :test 'compare-equal-p)

  (is (parse-with-sequence '(2 4)
        (parse-root-test))
      (make-instance 'root-test
                     'other-type 2
                     'other-type-2 4)
      "Parsing a nonterminal with backtracking works"
      :test 'compare-equal-p)

  (is (parse-with-sequence '(#(vector))
        (parse-root-or-nil-test))
      nil
      "Parsing a nonterminal to nil works"
      :test 'compare-equal-p))
