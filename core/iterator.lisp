;; Copyright 2017 Bradley Jensen
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

(defpackage :shcl/core/iterator
  (:use :common-lisp)
  (:import-from :shcl/core/utility #:make-extensible-vector #:optimization-settings #:required)
  (:import-from :fset)
  (:export
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator #:seq-iterator
   #:do-iterator #:peek-lookahead-iterator #:move-lookahead-to #:map-iterator
   #:filter-iterator #:concatenate-iterables #:concatenate-iterable-collection
   #:concatmap-iterator #:iterator-values #:lookahead-iterator-wrapper
   #:set-iterator #:lookahead-iterator-position-token

   ;; High-level sequence functions
   #:starts-with-p))
(in-package :shcl/core/iterator)

(optimization-settings)

(defclass iterator ()
  ((compute
    :initform (required)
    :initarg :compute
    :documentation
    "A function that returns the next value."))
  (:documentation
   "This represents the most basic sort of iterator.  It can only go
forward.  Unless otherwise specified, assume that iterators are not
thread safe.")
  (:metaclass closer-mop:funcallable-standard-class))

(defgeneric iterate-function (iterator)
  (:documentation
   "The function that should handle iteration for the given
iterator.

This generic function is called when initializing an iterator.  It is
not called every time the iterator advances."))

(defmethod initialize-instance :after ((i iterator) &key)
  (let ((fn (iterate-function i)))
    (closer-mop:set-funcallable-instance-function
     i
     (lambda ()
       (funcall fn i)))))

(defmacro emit (value)
  "Within the body of `make-iterator', cause the iterator to produce a
value.

It is an error for this macro to be used outside of `make-iterator'."
  (declare (ignore value))
  (error "This macro can only be used within the body of `make-iterator'."))

(defmacro stop ()
  "Within the body of `make-iterator', signal that the iterator has no
values left to produce.

It is an error for this macro to be used outside of `make-iterator'."
  (error "This macro can only be used within the body of `make-iterator'."))

(defmacro make-iterator ((&key type) &body body)
  "Create an iterator.

The body of this macro will be executed each time the iterator needs
to produce a new value.  The body is evaluated in the lexical
environment in which the `make-iterator' form appears.  Within the
body, the local macros `stop' and `emit' can be used to indicate end
of sequence or return a value.  After `stop' is evaluated, the
iterator will not be called again.  Both `stop' and `emit' cause a
control transfer out of the body of `make-iterator'."
  (let ((stop-value (gensym "STOP-VALUE"))
        (compute (gensym "COMPUTE"))
        (compute-block (gensym "COMPUTE-BLOCK"))
        (value (gensym "VALUE"))
        (type-sym (gensym "TYPE-SYM")))
    `(let* ((,type-sym ,type))
       (macrolet ((emit (value)
                    (list 'return-from ',compute-block value))
                  (stop ()
                    '(return-from ,compute-block ',stop-value)))
         (labels ((,compute ()
                    (let ((,value (block ,compute-block ,@body)))
                      (cond ((eq ,value ',stop-value)
                             (values nil nil))

                            (t
                             (values ,value t))))))
           (make-instance (or ,type-sym 'iterator) :compute #',compute))))))

(defun iterate-iterator (iter)
  "Advance an iterator of type `iterator'"
  (with-slots (compute) iter
    (when (not (slot-boundp iter 'compute))
      (return-from iterate-iterator (values nil nil)))

    (multiple-value-bind (value more) (funcall compute)
      (unless more
        (slot-makunbound iter 'compute))
      (values value more))))

(defmethod iterate-function ((iter iterator))
  #'iterate-iterator)

(defun next (iter)
  "Advance an iterator"
  (funcall iter))

(defmacro do-iterator ((value-sym iter &key result) &body body)
  "Iterate across the values produced by an iterator"
  (let ((iter-sym (gensym "ITER-SYM"))
        (more-sym (gensym "MORE-SYM"))
        (iter-fun (gensym "ITER-FUN"))
        (internal-value-sym (gensym "INTERNAL-VALUE-SYM")))
    `(let* ((,iter-sym (iterator ,iter))
            (,iter-fun (iterate-function ,iter-sym)))
       (loop
          (multiple-value-bind (,internal-value-sym ,more-sym) (funcall ,iter-fun ,iter-sym)
            (unless ,more-sym
              (return ,result))
            (let ((,value-sym ,internal-value-sym))
              ,@body))))))

(defun map-iterator (iter function)
  "Create a new iterator that applies the given function to each value
produced by the given iterator."
  (make-iterator ()
    (multiple-value-bind (value more) (next iter)
      (unless more
        (stop))
      (emit (funcall function value)))))

(defun filter-iterator (iter function)
  "Create a new iterator that only contains the elements produced by
`iter' where `function' returns non-nil.

For example,
 (iterator-values (filter-iterator (list-iterator '(1 2 3 4) #'oddp)))
will return
 #(1 3)"
  (make-iterator ()
    (loop
       (multiple-value-bind (value more) (next iter)
         (unless more
           (stop))
         (when (funcall function value)
           (emit value))))))

(defun concatenate-iterable-collection (iterable)
  "Concatenate a collection of iterables.

`iterable' is anything that can be iterated using the `iterator'
generic function.  It must contain objects that are also iterable
using `iterator'.

This function returns an iterator that traverses the iterable
containers in `iterable' and emits the values that each iterable
contains.  This function is essentially a generic version of
`concatenate-iterables'.

This allows you to walk across containers of heterogenous types.  For
example, the following is perfectly valid.

    (concatenate-iterable-collection
     (list (vector-iterator #(1 2 3))
           #(4 5 6)
           '(7 8 9)))"
  (concatmap-iterator iterable 'identity))

(defun concatmap-iterator (iterable function)
  "Map `function' onto the elements of `iterable', then concatenate
the iterable sequences that result.

The following forms are semantically equivalent.
    (concatenate-iterable-collection (map-iterator iter fn))
    (concatmap-iterator iter fn)"
  (let ((iter-iter (iterator iterable))
        current-iter)
    (make-iterator ()
      (when current-iter
        (do-iterator (value current-iter)
          (emit value)))
      (do-iterator (inner-value iter-iter)
        (setf current-iter (iterator (funcall function inner-value)))
        (do-iterator (value current-iter)
          (emit value)))
      (stop))))

(defun concatenate-iterables (&rest iterables)
  "Produce an iterator that traverses the values contained in the
given iterables.

This is simply a convenient version of
`concatenate-iterable-collection'"
  (concatenate-iterable-collection iterables))

(define-method-combination concatenate-iterables
    :identity-with-one-argument t
    :documentation
    "A method combination that combines method results using the
`concatenate-iterables' function.

This is like a generic version of the `nconc' method combination.
Instead of returning lists that are then destructively modified, you
may return any type that can be iterated with `iterator'.")

(defun iterator-values (iter)
  "Extract all remaining values from the given iterator and return
them in an array."
  (let ((vector (make-extensible-vector)))
    (do-iterator (value iter :result vector)
      (vector-push-extend value vector))))

(defclass lookahead-iterator (iterator)
  ((buffer
    :initform (cons 'tail 'tail)
    :initarg :buffer
    :type cons))
  (:documentation
   "A `lookahead-iterator' permits peeking at values without consuming
them.  Using the `peek-lookahead-iterator' function, you can see what
the next value will be.  Using `fork-lookahead-iterator', you can peek
arbitrarily far into the future of the sequence.")
  (:metaclass closer-mop:funcallable-standard-class))

(defun lookahead-iterator-position-token (iter)
  "Returns a value representing the position of the iterator.

This is an opaque value that can be compared to other position tokens
with `eq'.  The exact nature of the token is unspecified and subject
to change at any time.  If two lookahead iterators have `eq' position
tokens then they are at the same place in the same stream."
  (with-slots (buffer) iter
    buffer))

(defun fork-lookahead-iterator (iter)
  "Create a `lookahead-iterator' that shares the same future as the
given iterator.

That is, the returned iterator will produce the exact same sequence of
values as the given iterator.  The returned iterator shall be
considered part of the same \"family\" as the given iterator."
  (check-type iter lookahead-iterator)
  (with-slots (compute buffer) iter
    (make-instance (class-of iter) :compute compute :buffer buffer)))

(defun iterate-lookahead-iterator (iter)
  "Produce the next value for an iterator of type `lookahead-iterator'"
  (with-slots (buffer compute) iter
    (when (eq (cdr buffer) 'eof)
      (return-from iterate-lookahead-iterator (values nil nil)))

    (cond ((eq (cdr buffer) 'tail)
           ;; Buffer is empty.  Add something to the back
           (multiple-value-bind (value more) (funcall compute)
             (cond (more
                    (let ((new-tail (cons 'tail 'tail)))
                      (setf (car buffer) value
                            (cdr buffer) new-tail
                            buffer (cdr buffer))))

                   (t
                    (setf (cdr buffer) 'eof)))
             (values value more)))

          (t
           (let ((value (car buffer)))
             (setf buffer (cdr buffer))
             (values value t))))))

(defmethod iterate-function ((iter lookahead-iterator))
  (declare (ignore iter))
  #'iterate-lookahead-iterator)

(defun peek-lookahead-iterator (iter)
  "Produce (but don't consume) the next value that the given
`lookahead-iterator' will return."
  (next (fork-lookahead-iterator iter)))

(defun move-lookahead-to (iter-to-change model-iter)
  "Change the future of a `lookahead-iterator' to match that of a
different `lookahead-iterator'.

After this function returns, the first argument (`iter-to-change')
shall produce the exact same sequence of values as the second
argument (`model-iter').  The given iterators must be in the same
family.  The `fork-lookahead-iterator' function can be used to produce
a new iterator in a given family."
  (with-slots ((b-compute compute)
               (b-buffer buffer))
      iter-to-change
    (with-slots ((a-compute compute)
                 (a-buffer buffer))
        model-iter
      (unless (eq a-compute b-compute)
        (error "These iterators aren't in the same family"))
      (setf b-buffer a-buffer)))
  (values))

(defun lookahead-iterator-wrapper (iter)
  "Produce a `lookahead-iterator' that emits the same values as
`iter'."
  (make-iterator (:type 'lookahead-iterator)
    (multiple-value-bind (value more) (next iter)
      (unless more
        (stop))
      (emit value))))

(defun vector-iterator (vector)
  "Produce an iterator that traverses a vector."
  (let ((index 0))
    (make-iterator ()
      (when (>= index (length vector))
        (stop))
      (let ((value (aref vector index)))
        (incf index)
        (emit value)))))

(defun list-iterator (list)
  "Produce an iterator that traverses a list."
  (let ((cons list))
    (make-iterator ()
      (when (eq nil cons)
        (stop))
      (let ((value (car cons)))
        (setf cons (cdr cons))
        (emit value)))))

(defun seq-iterator (seq)
  "Produce an iterator that traverses an `fset:seq'"
  (make-iterator ()
    (when (equal 0 (fset:size seq))
      (stop))
    (let ((element (fset:first seq)))
      (setf seq (fset:less-first seq))
      (emit element))))

(defun set-iterator (set)
  "Produce an iterator that traverses an `fset:set'"
  (make-iterator ()
    (when (equal 0 (fset:size set))
      (stop))
    (let ((element (fset:least set)))
      (setf set (fset:less set element))
      (emit element))))

(defgeneric iterator (thing)
  (:documentation
   "Produce an iterator that traverses the given thing."))

(defmethod iterator ((list list))
  (list-iterator list))

(defmethod iterator ((vector vector))
  (vector-iterator vector))

(defmethod iterator ((seq fset:seq))
  (seq-iterator seq))

(defmethod iterator ((seq fset:set))
  (set-iterator seq))

(defmethod iterator ((iter iterator))
  iter)

(defun starts-with-p (iterable iterable-prefix &key (test 'eql))
  "Returns non-nil if the first elements of `iterable' match the
first elements of `iterable-prefix'.

`iterable' and `iterable-prefix' may be any type that can be passed to
the `iterator' generic function.  When non-equal elements are
found (as determined by the function provided in the `test'
parameter), this function returns nil.  If `iterable' has fewer
elements than `iterable-prefix', this function returns nil."
  (let ((prefix (iterator iterable-prefix))
        (seq (iterator iterable)))
    (labels
        ((next-expected ()
           (multiple-value-bind (value valid-p) (next prefix)
             (unless valid-p
               (return-from starts-with-p t))
             value))
         (next-found ()
           (multiple-value-bind (value valid-p) (next seq)
             (unless valid-p
               (return-from starts-with-p nil))
             value)))
      (loop :for expected = (next-expected)
         :for found = (next-found) :do
         (unless (funcall test found expected)
           (return-from starts-with-p nil)))
      (assert nil nil "This function doesn't return normally"))))
