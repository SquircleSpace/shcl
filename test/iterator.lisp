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

(defpackage :shcl/test/iterator
  (:use :common-lisp :prove :shcl/core/iterator :shcl/test/foundation)
  (:shadowing-import-from :shcl/core/iterator #:skip)
  (:import-from :shcl/core/utility #:optimization-settings))
(in-package :shcl/test/iterator)

(optimization-settings)

(link-package-to-system :shcl/core/iterator)

(defun empty-sequence (type)
  (funcall (builder-for-type type)))

(defconstant +simple-sequence-length+ 5)

(defun simple-sequence (type)
  (let ((builder (builder-for-type type)))
    (dotimes (i +simple-sequence-length+)
      (funcall builder i))
    (funcall builder)))

(defgeneric sequence-type-for-iterator-type (iterator-type))

(defmethod sequence-type-for-iterator-type ((type (eql 'list-iterator)))
  'list)

(defmethod sequence-type-for-iterator-type ((type (eql 'vector-iterator)))
  'vector)

(defmethod sequence-type-for-iterator-type ((type (eql 'seq-iterator)))
  'fset:seq)

(defmethod sequence-type-for-iterator-type ((type (eql 'set-iterator)))
  'fset:set)

(defgeneric empty-iterator (type))

(defmethod empty-iterator (type)
  (iterator (empty-sequence (sequence-type-for-iterator-type type))))

(defmethod empty-iterator ((type (eql 'forkable-wrapper-iterator)))
  (forkable-wrapper-iterator (iterator nil)))

(defmethod empty-iterator ((type (eql 'computed-iterator)))
  (make-computed-iterator
    (stop)))

(defgeneric simple-iterator (type))

(defmethod simple-iterator (type)
  (iterator (simple-sequence (sequence-type-for-iterator-type type))))

(defmethod simple-iterator ((type (eql 'forkable-wrapper-iterator)))
  (forkable-wrapper-iterator (iterator (simple-sequence 'list))))

(defmethod simple-iterator ((type (eql 'computed-iterator)))
  (let ((iter (simple-iterator 'list-iterator)))
    (make-computed-iterator
      (do-iterator (value iter)
        (emit value))
      (stop))))

(defun test-iterator-generic-function (class)
  (let ((sequence-type (sequence-type-for-iterator-type class)))
    (is-type (iterator (empty-sequence sequence-type)) class
             "Iterator GF returns the right type")
    (is-type (iterator (simple-sequence sequence-type)) class
             "Iterator GF returns the right type")))

(defun test-forward-iterator (class)
  (is (iterable-values (empty-iterator class) 'list)
      nil
      "Iteration of empty sequences works")
  (is (iterable-values (simple-iterator class) 'list)
      (simple-sequence 'list)
      "Iteration of non-empty sequences works"))

(defun test-backward-iterator (class)
  (declare (ignore class)))

(defun test-bidirectional-iterator (class)
  (declare (ignore class)))

(defun test-forkable-iterator (class)
  (labels
      ((test (a-iter)
         (let ((b-iter (fork a-iter)))
           (isnt b-iter a-iter :test 'eq
                 "Forks are different objects")
           ;; A forkable iterator that can't move is really hard to
           ;; test.  So, let's see if we can move it.
           (when (typep a-iter 'forward-iterator)
             (is (next a-iter) (next b-iter)
                 "Forks have independent forward state")
             (is (next a-iter) (next b-iter)
                 "Forks have independent forward state"))
           (when (typep a-iter 'backward-iterator)
             (is (previous a-iter) (previous b-iter)
                 "Forks have independent backward state")
             (is (previous a-iter) (previous b-iter)
                 "Forks have independent backward state")))))
    (test (empty-iterator class))
    (test (simple-iterator class))))

(defclass secret-class (bidirectional-skip-iterator)
  ())

(defun test-skip-method-exists (class)
  ;; We can't really verify that the implementation is effecient, but
  ;; we can verify that there's some method other than the fallback
  ;; one.
  (let* ((sequence (simple-iterator class))
         (base-methods (compute-applicable-methods #'skip (list (make-instance 'secret-class) 10)))
         (class-methods (compute-applicable-methods #'skip (list (iterator sequence) 10)))
         (extra-methods (set-difference class-methods base-methods :test 'eq)))
    (ok extra-methods
        "At least one method for skip is defined")))

(defun test-forward-skip-iterator (class)
  (let ((empty (empty-iterator class)))
    ;; Check if errors happen when skipping on an empty sequence
    (skip empty 100)
    (is (nth-value 1 (next empty)) nil
        "Yep, still empty"))

  (let ((iter (simple-iterator class)))
    (skip iter 0)
    (is (next iter) 0
        "Skipping 0 works")

    (skip iter 1)
    (is (next iter) 2
        "Skipping one works"))

  (let ((iter (simple-iterator class)))
    (assert (>= +simple-sequence-length+ 5))
    (skip iter 4)
    (is (next iter) 4
        "Skipping several works"))

  (let ((iter (simple-iterator class)))
    (skip iter (* 2 +simple-sequence-length+))
    (is (nth-value 1 (next iter)) nil
        "Skipping way too many works")))

(defun test-backward-skip-iterator (class)
  (declare (ignore class)))

(defun test-bidirectional-skip-iterator (class)
  (declare (ignore class)))

(defun auto-test-iterator (class)
  (let ((instance (allocate-instance (find-class class))))
    (if (ignore-errors (sequence-type-for-iterator-type class))
        (subtest "Iterator generic function"
          (test-iterator-generic-function class))
        (diag "Class doesn't seem to have a corresponding sequence type"))

    (if (typep instance 'forward-iterator)
        (subtest "Movement forward"
          (test-forward-iterator class)
          (test-forward-skip-iterator class))
        (diag "Class doesn't seem to support moving forward"))

    (if (typep instance 'backward-iterator)
        (subtest "Movement backward"
          (test-backward-iterator class)
          (test-backward-skip-iterator class))
        (diag "Class doesn't seem to support moving backward"))

    (if (typep instance 'bidirectional-iterator)
        (subtest "Movement both directions"
          (test-bidirectional-iterator class)
          (test-bidirectional-skip-iterator class))
        (diag "Class doesn't seem to support moving both forward and backward"))

    (if (typep instance 'forkable-iterator)
        (subtest "Forking"
          (test-forkable-iterator class))
        (diag "Class doesn't seem to support forking"))

    (if (or (typep instance 'forward-skip-iterator)
            (typep instance 'backward-skip-iterator))
        (subtest "Skipping"
          (test-skip-method-exists class))
        (diag "Class doesn't seem to support skipping"))))

(define-test list-iterator
  (auto-test-iterator 'list-iterator))

(define-test vector-iterator
  (auto-test-iterator 'vector-iterator))

(define-test seq-iterator
  (auto-test-iterator 'seq-iterator))

(define-test set-iterator
  (auto-test-iterator 'set-iterator))

(define-test forkable-wrapper-iterator
  (auto-test-iterator 'forkable-wrapper-iterator)
  (let* ((invocations 0)
         (iter-a (forkable-wrapper-iterator (make-computed-iterator (emit (incf invocations)))))
         (iter-b (fork iter-a)))
    (is invocations 0
        "Wrapper iterator hasn't been called yet")

    ;; Let's just try to mix up the state a bit...
    (next (fork iter-a))
    (is invocations 1
        "Wrapper iterator has been called once")

    (is (forkable-wrapper-iterator-position-token iter-a)
        (forkable-wrapper-iterator-position-token iter-b)
        "Forks share a position token")

    (is (next iter-a) 1
        "Forks don't impact each other")
    (is (next iter-b) 1
        "Forks don't impact each other")
    (is (forkable-wrapper-iterator-position-token iter-a)
        (forkable-wrapper-iterator-position-token iter-b)
        :test 'eq
        "Forks share a position token")
    (is invocations 1
        "Wrapper iterator has been called once")

    (is (next iter-a) 2
        "Just making sure...")
    (is invocations 2
        "Wrapper iterator has been called again")
    (isnt (forkable-wrapper-iterator-position-token iter-a)
          (forkable-wrapper-iterator-position-token iter-b)
          :test 'eq
          "Forks no longer share a position token")
    (move-forkable-wrapper-iterator-to iter-a iter-b)
    (is (forkable-wrapper-iterator-position-token iter-a)
        (forkable-wrapper-iterator-position-token iter-b)
        :test 'eq
        "Forks again share a position token")
    (is (next iter-a) 2
        "Position was succesfully moved back")))

(define-test computed-iterator
  (auto-test-iterator 'computed-iterator)
  (let* (stopped
         failed
         (iter (make-computed-iterator
                 (when stopped
                   (setf failed t))
                 (setf stopped t)
                 (stop))))
    ;; Once to prime it...
    (is (nth-value 1 (next iter)) nil
        "Just making sure...")
    ;; Once to try and trigger it...
    (is (nth-value 1 (next iter)) nil
        "Just making sure...")
    (is failed nil
        "lambda must not be called after it calls stop")))
