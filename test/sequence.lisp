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

(defmacro do-walk ((var walkable &optional result) &body body)
  (let ((walk (gensym "WALK"))
        (value (gensym "VALUE"))
        (valid-p (gensym "VALID-P")))
    `(let ((,walk (walk ,walkable)))
       (loop
         (multiple-value-bind (,value ,valid-p) (head ,walk)
           (unless ,valid-p
             (return ,result))
           (setf ,walk (tail ,walk))
           (let ((,var ,value))
             ,@body))))))

(defun pour-from (source sink)
  (do-walk (value source)
    (attachf sink value))
  sink)

(defparameter *test-sequence*
  '(10 5 15 0 1 2 3 100 101 -3 1 2 3))

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
             (immutable-cons (incf accumulator) (generator)))))
      (generator))))

(defconstant +lazy-sequence-test-thread-count+ 5)
(defconstant +lazy-sequence-test-stopping-point+ 1000)

(define-test lazy-sequence
  (let ((sequence (naturals))
        (test-pass t)
        (result-lock (bordeaux-threads:make-lock))
        threads)
    (labels
        ((thread ()
           (let ((expected 0))
             (do-walk (value sequence)
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
          "Lazy sequences appear thread-safe")))

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
        "Lazy sequences are evaluated when necessary")))
