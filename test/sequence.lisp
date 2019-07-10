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

(defpackage :shcl/test/sequence
  (:use :common-lisp :shcl/core/utility :shcl/core/sequence :prove
        :shcl/test/foundation)
  (:import-from :fset))
(in-package :shcl/test/sequence)

(optimization-settings)

(link-package-to-system :shcl/core/sequence)

(defparameter *test-sequence*
  '(10 5 15 0 1 2 3 100 101 -3 1 2 3))

(define-test do-while-popf
  (let ((sequence *test-sequence*)
        evaluated-p)
    (do-while-popf (var sequence (setf evaluated-p t))
      (declare (ignore var))
      (return))
    (ok (not evaluated-p)
        "do-while-popf doesn't evaluate result form when early returning")
    (is sequence (cdr *test-sequence*)
        "do-while-popf mutates the given place"))
  (let (sequence)
    (is (do-while-popf (var sequence (* 1 2 3)) (declare (ignore var)))
        6
        "do-while-popf evaluates result form on normal completion")
    (ok (null sequence)
        "do-while-popf left the sequence in the expected state"))
  (let ((sequence *test-sequence*)
        (sequence-check *test-sequence*))
    (do-while-popf (value sequence (pass "do-while-popf produced expected values"))
      (unless (eq value (pop sequence-check))
        (return (fail "do-while-popf produced an unexpected value")))
      (unless (eq sequence sequence-check)
        (return (fail "do-while-popf left the sequence in an unexpected state"))))))

(define-test pour-from
  (is (fset:compare (pour-from *test-sequence* (fset:empty-seq))
                    (fset:convert 'fset:seq *test-sequence*))
      :equal
      "Pouring into a seq works")
  (is (fset:compare (pour-from *test-sequence* (fset:empty-set))
                    (fset:convert 'fset:set *test-sequence*))
      :equal
      "Pouring into a set works")
  (ok (equal (reverse (pour-from *test-sequence* nil))
             *test-sequence*)
      "Pouring into a list works")
  (is (fset:compare (pour-from (fset:empty-map) (fset:empty-bag))
                    (fset:empty-bag))
      :equal
      "Pouring an empty sequence works"))

(define-test concatenate-sequences
  (is (fset:compare
       (pour-from (concatenate-sequences '(1 2 3) (fset:empty-map) (fset:bag 3.5)
                                         (fset:seq 4 5 6) (immutable-list 7 8 9))
                  (fset:empty-seq))
       (fset:seq 1 2 3 3.5 4 5 6 7 8 9))
      :equal
      "Concatenating some-sequences works")
  (is (pour-from (concatenate-sequences nil nil nil (immutable-list)) nil)
      nil
      "Empty concatenated sequences work"))

(define-test eager-map
  (ok (equal (reverse (eager-map *test-sequence* (lambda (val) (* val 2)) nil))
             (mapcar (lambda (val) (* val 2)) *test-sequence*))
      "eager-map works on non-empty sequences")
  (ok (null (eager-map nil (lambda (val) (* val 2)) nil))
      "eager-map works on empty sequences"))

(define-test lazy-map
  (let* (values
         (functions (list
                     (lambda () (push 3 values) 3)
                     (lambda () (push 2 values) 2)
                     (lambda () (push 1 values) 1)))
         (evals (lazy-map functions #'funcall))
         (evals-original evals))
    (ok (null values)
        "lazy-map is lazy")
    (ok (not (empty-p evals))
        "lazy-map knows that its not empty")
    (is (popf evals) 3
        "lazy-map returns the expected value")
    (ok (equal values '(3))
        "lazy-map is lazy")
    (is (popf evals) 2
        "lazy-map returns the expected value")
    (ok (equal values '(2 3))
        "lazy-map is lazy")
    (is (popf evals) 1
        "lazy-map returns the expected value")
    (ok (equal values '(1 2 3))
        "lazy-map is lazy")
    (ok (empty-p evals)
        "lazy-map knows that its empty")
    (do-while-popf (value evals-original)
      (declare (ignore value)))
    (ok (equal values '(1 2 3))
        "lazy-map evaluates things only once")))

(define-test lazy-filter
  (let* (values
         (input '(3 2 1 0))
         (evals (lazy-filter input (lambda (val) (push val values) (evenp val))))
         (evals-original evals))
    (ok (null values)
        "lazy-filter is lazy")
    (ok (not (empty-p evals))
        "lazy-filter knows that its not empty")
    (is values '(2 3)
        "lazy-filter is lazy")
    (is (popf evals) 2
        "lazy-filter returns the expected value")
    (is values '(2 3)
        "lazy-filter is lazy")
    (is (popf evals) 0
        "lazy-filter returns the expected value")
    (ok (equal values '(0 1 2 3))
        "lazy-filter is lazy")
    (ok (empty-p evals)
        "lazy-filter knows that its empty")
    (do-while-popf (value evals-original)
      (declare (ignore value)))
    (ok (equal values '(0 1 2 3))
        "lazy-filter evaluates things only once")))

(define-test eager-filter
  (is (reverse (eager-filter *test-sequence* #'evenp nil))
      (remove-if-not #'evenp *test-sequence*)
      "eager-filter works on non-empty sequences")
  (ok (null (eager-filter nil #'evenp nil))
      "eager-filter works on empty sequences"))

(define-test flatten-sequence
  (is (fset:compare
       (pour-from (flatten-sequence (fset:set (fset:seq 1 2 3) (fset:seq 4 5 6))) (fset:empty-seq))
       (fset:seq 1 2 3 4 5 6))
      :equal
      "Flattening sequences works"))

(defun test-sequence (sequence-type sequence-order)
  (let ((sequence (empty-for-type sequence-type)))
    (is-type sequence sequence-type
             "empty-for-type returns the desired type")
    (ok (empty-p sequence)
        "empty-for-type returns an empty sequence")
    (setf sequence (empty-of (attach sequence (car *test-sequence*))))
    (is-type sequence sequence-type
             "empty-of returns the expected type")
    (ok (empty-p sequence)
        "empty-of returns an empty sequence")
    (attachf sequence (car *test-sequence*))
    (ok (not (empty-p sequence))
        "attaching a value makes the sequence non-empty")
    (popf sequence)
    (ok (empty-p sequence)
        "popping off head makes the sequence empty again")
    (setf sequence (pour-from *test-sequence* sequence))
    (ok (not (empty-p sequence))
        "Pouring in test sequence makes the sequence non-empty")
    (ecase sequence-order
      (:fifo
       (is (reverse (pour-from sequence nil)) *test-sequence*
           "Sequence is FIFO-like"))
      (:lifo
       (is (reverse (pour-from sequence nil)) (reverse *test-sequence*)
           "Sequence is LIFO-like"))
      (:sorted-least-to-greatest
       (is (reverse (pour-from sequence nil)) (sort (copy-list *test-sequence*) #'<)
           "Sequence is ordered least to greatest"))
      (:sorted-greatest-to-least
       (is (reverse (pour-from sequence nil)) (sort (copy-list *test-sequence*) #'>)
           "Sequence is ordered greatest to least"))
      (:unordered
       (let ((output (sort (pour-from sequence nil) #'<))
             (expected (sort (copy-list *test-sequence*) #'<)))
         (is output expected
             "Sequence preserved values")))
      (:lossy
       (diag "Not checking contents of sequence -- sequence is lossy")))
    (isnt (tail sequence) sequence
          "Tail of non-empty sequence returns a different object")
    (setf sequence (empty-of sequence))
    (pour-from (cdr *test-sequence*) sequence)
    (isnt (attach sequence (car *test-sequence*)) sequence)
    (is-type (tail (empty-of sequence)) sequence-type
             "Tail of empty sequence is of the expected type")))

(defun list-to-alist (list)
  (let ((accumulator 0))
    (mapcar (lambda (key) (cons key (incf accumulator))) list)))

(define-test basic-sequence-types
  (subtest "List"
    (test-sequence 'list :lifo))
  (subtest "Immutable list"
    (test-sequence 'immutable-list :lifo))
  (subtest "fset:seq"
    (test-sequence 'fset:seq :fifo))
  (subtest "fset:set"
    (test-sequence 'fset:set :lossy))
  (subtest "fset:bag"
    (test-sequence 'fset:bag :sorted-least-to-greatest))
  (subtest "fset:map"
    (let ((*test-sequence* (list-to-alist *test-sequence*)))
      (test-sequence 'fset:map :lossy))))

(defun naturals ()
  (let ((accumulator 0))
    (labels
        ((generator ()
           (lazy-sequence
             (declare (wrap-with cache-impure))
             (immutable-cons (incf accumulator) (generator)))))
      (generator))))

(define-test sequence-starts-with-p
  (ok (sequence-starts-with-p (naturals) '(1 2 3))
      "The natural numbers start with 1 2 3")
  (ok (not (sequence-starts-with-p (naturals) '(1 2 3 a b c)))
      "The naturals are not as easy as A, B, C")
  (let ((evaluation-count 0))
    (labels
        ((generator ()
           (lazy-sequence
             (declare (wrap-with cache-impure))
             (cons (incf evaluation-count) (generator)))))
      (sequence-starts-with-p (generator) '(1 2 3 a b c)))
    (is evaluation-count
        4
        "sequence-starts-with-p early returns when a mismatch is encountered"))

  (ok (sequence-starts-with-p
       (eager-map '("foo" "bar" "baz") 'string-upcase (fset:empty-seq))
       '("FOO" "BAR")
       :test 'equal)
      "the :test argument works"))

(defconstant +lazy-sequence-test-thread-count+ 5)
(defconstant +lazy-sequence-test-stopping-point+ 1000)

(define-test cache-impure
  (let* ((evaluated-count 0)
         (fn (cache-impure (lambda ()
                             (incf evaluated-count)
                             123456))))
    (is evaluated-count
        0
        "cache-impure doesn't call the cached function instantly")
    (is (funcall fn)
        123456
        "cache-impure returns a funcallable object that calls the given function")
    (is evaluated-count
        1
        "function was actually called")
    (funcall fn)
    (is evaluated-count
        1
        "cache-impure only calls the wrapped function once"))

  (let ((sequence (naturals))
        (test-pass t)
        (result-lock (bordeaux-threads:make-lock))
        threads)
    (labels
        ((thread ()
           (let ((expected 0)
                 (sequence sequence))
             (do-while-popf (value sequence)
               (when (equal expected +lazy-sequence-test-stopping-point+)
                 (return))
               (unless (equal (incf expected) value)
                 (bordeaux-threads:with-lock-held (result-lock)
                   (setf test-pass nil))
                 (return))))))
      (dotimes (thread-num +lazy-sequence-test-thread-count+)
        (push (bordeaux-threads:make-thread #'thread) threads))
      (dolist (thread threads)
        (bordeaux-threads:join-thread thread))
      (ok test-pass
          "Lazy sequences with cache-impure appear thread-safe"))))

(define-test wrap-with
  (labels
      ((local-fn (wrapee)
         (lambda () (cons 'local-fn (funcall wrapee))))
       (local-fn-2 (wrapee)
         (lambda () (cons 'local-fn-2 (funcall wrapee)))))
    (macrolet
        ((local-macro (wrapee)
           `(lambda () (cons 'local-macro (funcall ,wrapee)))))

      (is (walkable-to-list
           (lazy-sequence
             (declare (wrap-with local-fn))
             (list 123)))
          '(local-fn 123)
          "Basic wrapping works")

      (is (walkable-to-list
           (lazy-sequence
             (declare (wrap-with local-fn)
                      (optimize (compilation-speed 0)))
             (list 123)))
          '(local-fn 123)
          "wrap-with declarations can live in the same declaration as other delcarations")

      (is (walkable-to-list
           (lazy-sequence
             (declare (optimize (compilation-speed 0))
                      (wrap-with local-fn)
                      (optimize (space 0)))
             (declare (wrap-with local-fn))
             (declare (optimize (speed 0)))
             (list 123)))
          '(local-fn local-fn 123)
          "More complex declaration orderings work")

      (is (walkable-to-list
           (lazy-sequence
             (declare (wrap-with local-macro))
             (list 123)))
          '(local-macro 123)
          "Wrapping with macros works")

      (is (walkable-to-list
           (lazy-sequence
             (declare (wrap-with local-fn-2))
             (declare (wrap-with local-macro)
                      (wrap-with local-fn))
             (declare (wrap-with local-fn-2))
             (list 123)))
          '(local-fn-2 local-macro local-fn local-fn-2 123)
          "Wrapping order is as expected"))))

(define-test lazy-sequence
  (let ((sequence (lazy-sequence '(1 2 3))))
    (is (head sequence) 1
        "Lazy sequence's head acts like the underlying sequence's head")
    (is (tail sequence) '(2 3)
        "Lazy sequence's tail acts like the underlying sequence's tail")
    (ok (not (empty-p sequence))
        "Lazy sequence's empty-p acts like the underlying sequence's")
    (is (attach sequence 0) '(0 1 2 3)
        "Lazy sequence's attach acts like the underlying sequence's")
    (is (empty-of sequence) nil
        "Lazy sequence's empty-of returns an empty sequence of the underlying type"))

  (let* (evaluated-p
         (sequence (lazy-sequence (list (setf evaluated-p t)))))
    (ok (not evaluated-p)
        "Creating a lazy sequence doesn't immediately evaluate the sequence")
    (walk sequence)
    (ok (not evaluated-p)
        "Attempting to walk a lazy sequence doesn't evaluate it")
    (ok (head sequence)
        "Lazy sequence still acts like the underlying sequence")
    (ok evaluated-p
        "Lazy sequences are evaluated when necessary"))

  (let* ((eval-count 0)
         (sequence (lazy-sequence
                     (list (incf eval-count)))))
    (head sequence)
    (is (head sequence)
        2
        "Lazy sequences evaluate every time they are called on")))

(define-test shcl/core/sequence::vector-walkable
  (let ((sequence *test-sequence*)
        (walkable (walk (coerce *test-sequence* 'vector))))
    (do-while-popf (value walkable (pass "Walking a non-empty vector-walkable worked correctly"))
      (unless (eq (pop sequence) value)
        (return (fail "walking a vector-walkable produced an unexpected value")))))
  (ok (empty-p (walk #()))
      "Empty vector-walkable are empty")
  (ok (empty-p #())
      "Empty vectors are empty")
  (is (multiple-value-list (head (walk #())))
      '(nil nil)
      "head on an empty vector-walkable returns expected values")
  (is (multiple-value-list (head #()))
      '(nil nil)
      "head on an empty vector returns expected values")
  (ok (not (empty-p (walk #(1))))
      "Non-empty vectors-walkable are not empty")
  (ok (not (empty-p #(1)))
      "Non-empty vectors are not empty")
  (is (multiple-value-list (head (walk #(1))))
      '(1 t)
      "head on a non-empty vector-walkable returns expected values")
  (is (multiple-value-list (head #(1)))
      '(1 t)
      "head on a non-empty vector returns expected values")
  (is (pour-from (tail #(1 2 3)) nil)
      '(3 2)
      "Tail on a non-empty vector works")
  (ok (empty-p (tail #()))
      "Tail on an empty vector works")
  (is (multiple-value-list (head (tail #())))
      '(nil nil)
      "Tail on an empty vector works"))

(define-test sort-sequence
  (let* ((sequence *test-sequence*)
         (key (lambda (val) (- (+ val 10))))
         (expected (sort (copy-list sequence) #'< :key key)))
    (is (walkable-to-list (sort-sequence sequence #'< :key key))
        expected
        "Sorting a list works")
    (is (walkable-to-list (sort-sequence (coerce sequence 'vector) #'< :key key))
        expected
        "Sorting a vector works")
    (is (walkable-to-list (sort-sequence nil #'<))
        nil
        "Sorting an empty sequence works")))

(defun compare-sequence-predicate-functions (baseline alternate)
  (labels
      ((match (comment &rest args)
         (is (apply alternate args)
             (apply baseline args)
             comment)))
    (match "Baseline output matches"
      'zerop '(1 2 3 4 0 5 6 7 0 0 10))
    (match "Empty sequence matches"
      'zerop nil)
    (match "Non-matching output matches"
      (constantly nil) *test-sequence*)
    (match "start overshoot matches"
      (constantly t) *test-sequence* :start (length *test-sequence*))
    (match "0-length interval matches"
      (constantly t) *test-sequence* :start 1 :end 1)
    (let ((sequence '(-5 18 1000 300 -20 132)))
      (match "Key works"
        (lambda (obj) (equal 1 obj)) sequence :key (lambda (obj) (/ obj 1000))))
    (let ((sequence '(-5 32 0 197 0.0 20 0 0 100)))
      (match "Start works"
        'zerop sequence :start 2)
      (match "Start works"
        'zerop sequence :start 3)
      (match "Start / end works"
        'zerop sequence :start 3 :end 4)
      (match "Start / end works"
        'zerop sequence :start 3 :end 5)
      (match "End works"
        'zerop sequence :end 2)
      (match "End works"
        'zerop sequence :end 3))

    (is-error
     (funcall alternate (constantly t) *test-sequence* :start #(1 2 3))
     'type-error
     "bogus start errors")
    (is-error
     (funcall alternate (constantly t) *test-sequence* :start -1)
     'type-error
     "bogus start errors")
    (is-error
     (funcall alternate (constantly t) *test-sequence* :end #(1 2 3))
     'type-error
     "bogus end errors")
    (is-error
     (funcall alternate (constantly t) *test-sequence* :end -1)
     'type-error
     "bogus end errors")
    (is-error
     (funcall alternate (constantly t) *test-sequence* :start 3 :end 2)
     'error
     "bogus start/end interval errors")))

(defun test-sequence-finder (fn)
  (let* ((sequence '(0 1 2 3 4 5 6 7 8))
         (expected-tail (cdddr sequence)))
    (assert expected-tail)
    (is (nth-value 1 (funcall fn (constantly t) sequence :start 3))
        expected-tail
        "The located tail is returned")
    (ok (null (nth-value 1 (funcall fn (constantly nil) sequence)))
        "Failure to find returns nil tail")))

(define-test sequence-find-if
  (compare-sequence-predicate-functions 'find-if 'sequence-find-if)

  (test-sequence-finder 'sequence-find-if))

(defun predicate-inverter (fn)
  (lambda (predicate &rest args)
    (apply fn (lambda (item) (not (funcall predicate item))) args)))

(define-test sequence-find-if-not
  (compare-sequence-predicate-functions
   (predicate-inverter 'find-if-not)
   (predicate-inverter 'sequence-find-if-not))

  (test-sequence-finder (predicate-inverter 'sequence-find-if-not)))

(defun compare-sequence-item-functions (baseline alternate)
  (labels
      ((match (comment &rest args)
         (is (apply alternate args)
             (apply baseline args)
             comment)))
    (let ((sequence '(1 2 3 0.0 4 0 5 6 0.0 10 0)))
      (match "Baseline behavior matches"
        0 sequence)
      (match "Non-matching behavior matches"
        'abc sequence)
      (match "Test parameter is respected"
        0 sequence :test '=)
      (match "Test-not parameter is respected"
        0 sequence :test-not (lambda (f s) (not (= f s))))

      (is-error
       (funcall alternate 0 sequence :test 'equalp :test-not 'equal)
       'error
       "Providing both test and test-not is an error"))))

(define-test sequence-find
  (compare-sequence-item-functions 'find 'sequence-find))

(define-test eager-sequence-remove-if
  (labels
      ((eager-sequence-remove-if-wrapper (predicate sequence &rest keyword-args)
         (nreverse (apply 'eager-sequence-remove-if predicate sequence nil keyword-args))))
    (compare-sequence-predicate-functions 'remove-if #'eager-sequence-remove-if-wrapper)))

(define-test eager-sequence-remove-if-not
  (labels
      ((eager-sequence-remove-if-not-wrapper (predicate sequence &rest keyword-args)
         (nreverse (apply 'eager-sequence-remove-if-not predicate sequence nil keyword-args))))
    (compare-sequence-predicate-functions
     (predicate-inverter 'remove-if-not)
     (predicate-inverter #'eager-sequence-remove-if-not-wrapper))))

(define-test eager-sequence-remove
  (labels
      ((eager-sequence-remove-wrapper (item sequence &rest keyword-args)
         (nreverse (apply 'eager-sequence-remove item sequence nil keyword-args))))
    (compare-sequence-item-functions 'remove #'eager-sequence-remove-wrapper)))

(define-test sequence-count-if
  (compare-sequence-predicate-functions 'count-if 'sequence-count-if))

(define-test sequence-count-if-not
  (compare-sequence-predicate-functions
   (predicate-inverter 'count-if-not)
   (predicate-inverter 'sequence-count-if-not)))

(define-test sequence-count
  (compare-sequence-item-functions 'count 'sequence-count))

(define-test sequence-nth-tail
  (let ((sequence '(1 2 3 4 5 6 7)))
    (is (multiple-value-list
         (sequence-nth-tail sequence 0))
        (list sequence 0)
        "nth = 0 does the right thing")
    (is (multiple-value-list
         (sequence-nth-tail sequence 3))
        (list (cdddr sequence) 0)
        "nth = 3 does the right thing")
    (is (multiple-value-list
         (sequence-nth-tail sequence (length sequence)))
        (list nil 0)
        "nth = length does the right thing")
    (is (multiple-value-list
         (sequence-nth-tail sequence (+ 20 (length sequence))))
        (list nil 20)
        "nth = length + 20 does the right thing")
    (is (multiple-value-list
         (sequence-nth-tail nil 20))
        (list nil 20)
        "Empty sequences work")))
