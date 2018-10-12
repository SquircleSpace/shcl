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
  (:import-from :shcl/core/data #:trivially-clonable #:clone)
  (:import-from :fset)
  (:export
   ;; Generic functions
   #:iterator #:next #:previous #:skip #:fork

   ;; Useful type tags for subclasses
   #:forkable-iterator #:fork-with-clone #:forward-iterator
   #:backward-iterator #:bidirectional-iterator
   #:forward-skip-iterator #:backward-skip-iterator
   #:bidirectional-skip-iterator

   ;; Fundamental iterator classes
   #:list-iterator #:vector-iterator #:seq-iterator #:set-iterator
   #:computed-iterator

   ;; Higher order iterators
   #:iterator-transformation #:mapped-iterator #:filtered-iterator
   #:concatenated-iterator #:forkable-wrapper-iterator

   ;; Sequence operations
   #:do-sequence #:sequence-starts-with-p #:builder-for-type #:sequence-map

   ;; Other
   #:emit #:stop #:move-forkable-wrapper-iterator-to
   #:forkable-wrapper-iterator-position-token
   #:concatmapped-iterator #:make-computed-iterator
   #:do-iterator :concatenate-iterables
   #:concatenate-iterable-collection
   #:iterable-values))
(in-package :shcl/core/iterator)

(optimization-settings)

(defgeneric iterator (object)
  (:documentation
   "Produce an iterator that traverses the given sequence.

This may return the given object unmodified if it is already an
iterator."))

(defgeneric next (iterator)
  (:documentation
   "Advance an iterator forward.

If a class supports this method, it should inherit from the
`forward-iterator' class.

This function returns two values: the value produced by the iterator
and a boolean.  If the iterator has no values left to produce, it
should return nil for the second return value.  The first return value
should be ignored in that case.  If the iterator successfully produced
a value it should return a non-nil second value."))

(defgeneric previous (iterator)
  (:documentation
   "Move an iterator backward.

If a class supports this method, it should inherit from the
`backward-iterator' class.

Like `next', this function returns two values.  However, instead of
advancing the iterator forward through the sequence, it should move
the iterator backward through the sequence."))

(defgeneric skip (iterator count)
  (:documentation
   "Move an iterator quickly, ignoring intermediate values.

If a class supports an efficient implementation of this method, it
should inherit from one of the following classes.
- `forward-skip-iterator'
- `backward-skip-iterator'
- `bidirectional-skip-iterator'

A method is provided that simply calls `next' and `previous'
repeatedly."))

(defmethod skip (iterator (count integer))
  (dotimes (i (abs count))
    (multiple-value-bind (value eof-p)
        (if (minusp count)
            (previous iterator)
            (next iterator))
      (declare (ignore value))
      (unless eof-p
        (return))))
  (values))

(defgeneric fork (iterator)
  (:documentation
   "Create an iterator that traverses the same sequence as
`iterator'.

If a class supports forking, it should inherit from
`forkable-iterator'.

The returned iterator has independent state.  That is, the returned
iterator can be advanced independently of `iterator'.  However, they
share the same sequence.  So, advancing the original iterator will not
modify the output of the new iterator.

When applicable, you are encouraged to implement a `clone' method and
then call `clone' from `fork'.  You can also just inherit from
`fork-with-clone'.  You should only do this if `clone' will achieve
the semantics described above.  Its possible that an iterator might be
clonable without being forkable."))

(defclass iterator ()
  ()
  (:documentation
   "A base class to represent iterators.

On its own, this class means nothing.  You should probably inherit
from `forward-iterator' or `backward-iterator' instead."))

(defmethod iterator ((iterator iterator))
  iterator)

(defclass forkable-iterator (iterator)
  ()
  (:documentation
   "A base class to represent iterators that can be forked with `fork'.

If a class inherits from `forkable-iterator' then it must have a
method for the `fork' generic function.

This class only exists to describe the capabilities of its subclasses.
It is intended to allow you to specialize methods on iterators with
specific capabilities."))

(defclass fork-with-clone (forkable-iterator)
  ()
  (:documentation
   "A mix-in class that provides a `fork' method which calls
`clone'."))

(defmethod fork ((iterator fork-with-clone))
  (clone iterator))

(defclass forward-iterator (iterator)
  ()
  (:documentation
   "A base class to represent iterators that can be advanced with
`next'.

If a class inherits from `forward-iterator' then it must have a method
for the `next' generic function.

This class only exists to describe the capabilities of its subclasses.
It is intended to allow you to specialize methods on iterators with
specific capabilities."))

(defclass backward-iterator (iterator)
  ()
  (:documentation
   "A base class to represent iterators that can be moved with
`previous'.

If a class inherits from `backward-iterator' then it must have a method
for the `previous' generic function.

This class only exists to describe the capabilities of its subclasses.
It is intended to allow you to specialize methods on iterators with
specific capabilities."))

(defclass bidirectional-iterator (forward-iterator backward-iterator)
  ()
  (:documentation
   "A base class to represent iterators that can be moved with
`next' and `previous'.

If a class inherits from `bidirectional-iterator' then it must have a method
for the `next' and `previous' generic functions.

This class only exists to describe the capabilities of its subclasses.
It is intended to allow you to specialize methods on iterators with
specific capabilities."))

(defclass forward-skip-iterator (forward-iterator)
  ()
  (:documentation
   "A base class to represent iterators that can be moved forward with
`skip'.

If a class inherits from `forward-skip-iterator' then it must have a
method for the `skip' generic function that handles non-negative
integer movements.

This class only exists to describe the capabilities of its subclasses.
It is intended to allow you to specialize methods on iterators with
specific capabilities."))

(defclass backward-skip-iterator (backward-iterator)
  ()
  (:documentation
   "A base class to represent iterators that can be moved backward
with `skip'.

If a class inherits from `backward-skip-iterator' then it must have a
method for the `skip' generic function that handles negative integer
movements.

This class only exists to describe the capabilities of its subclasses.
It is intended to allow you to specialize methods on iterators with
specific capabilities."))

(defclass bidirectional-skip-iterator (forward-skip-iterator backward-skip-iterator bidirectional-iterator)
  ()
  (:documentation
   "A base class to represent iterators that can be moved forward or
backward with `skip'.

If a class inherits from `backward-skip-iterator' then it must have a
method for the `skip' generic function that handles integer movements.

This class only exists to describe the capabilities of its subclasses.
It is intended to allow you to specialize methods on iterators with
specific capabilities."))

(defmacro do-iterator ((value-sym iterator &key result) &body body)
  "Iterate across the values produced by an iterator.

This macro repeatedly calls `next' on `iterator' until it indicates
end-of-file by returning nil for its second value.  Each produced
value is bound to `value-sym' and then `body' is evaluated."
  (let ((iter-sym (gensym "ITER-SYM"))
        (more-sym (gensym "MORE-SYM"))
        (internal-value-sym (gensym "INTERNAL-VALUE-SYM")))
    `(let ((,iter-sym ,iterator))
       (loop
          (multiple-value-bind (,internal-value-sym ,more-sym) (next ,iter-sym)
            (unless ,more-sym
              (return ,result))
            (let ((,value-sym ,internal-value-sym))
              ,@body))))))

(defmacro do-sequence ((value-sym iterable &key result) &body body)
  "Iterate across the values in an iterable sequence.

This macro produces an iterator by calling `iterator' on `iterable'.
Then, it uses `do-iterator' to traverse the sequence."
  `(do-iterator (,value-sym (iterator ,iterable) :result ,result)
     ,@body))

(defclass list-iterator (forward-iterator fork-with-clone trivially-clonable)
  ((list
    :initarg :list
    :initform nil
    :type list))
  (:documentation
   "This class represents an iterator that can traverse a list."))

(defmethod iterator ((list list))
  (list-iterator list))

(defun list-iterator (list)
  "Produce a `list-iterator' for the given list."
  (make-instance 'list-iterator :list list))

(defmethod print-object ((iter list-iterator) stream)
  (print-unreadable-object (iter stream :type t)
    (format stream "(~W~{ ~W~})" 'list
            (slot-value iter 'list))))

(defmethod next ((iterator list-iterator))
  (with-slots (list) iterator
    (if list
        (values (pop list) t)
        (values nil nil))))

(defclass vector-iterator (bidirectional-skip-iterator fork-with-clone trivially-clonable)
  ((sequence
    :initarg :vector
    :initform #()
    :type vector)
   (next-offset
    :initarg :first-offset
    :initform 0
    :type (integer 0)))
  (:documentation
   "This class represents an iterator that can traverse a vector."))

(defmethod iterator ((vector vector))
  (vector-iterator vector))

(defun vector-iterator (vector)
  "Produce a `vector-iterator' for the given vector."
  (make-instance 'vector-iterator :vector vector))

(defmethod print-object ((iter vector-iterator) stream)
  (print-unreadable-object (iter stream :type t)
    (format stream "(~W" 'vector)
    (do-sequence (value (fork iter))
      (format stream " ~W" value))
    (format stream ")")))

(defmacro define-random-access-methods (class-name access-fn-name size-fn-name)
  (let ((iterator (gensym "ITERATOR"))
        (sequence (gensym "SEQUENCE"))
        (length (gensym "LENGTH"))
        (old-offset (gensym "OLD-OFFSET"))
        (offset (gensym "OFFSET"))
        (count (gensym "COUNT")))
    `(progn
       (defmethod next ((,iterator ,class-name))
         (let* ((,sequence (slot-value ,iterator 'sequence))
                (,length (,size-fn-name ,sequence))
                (,old-offset (slot-value ,iterator 'next-offset)))
           (setf (slot-value ,iterator 'next-offset) (min (1+ ,old-offset) ,length))
           (if (>= ,old-offset ,length)
               (values nil nil)
               (values (,access-fn-name ,sequence ,old-offset) t))))

       (defmethod previous ((,iterator ,class-name))
         (let ((,offset (slot-value ,iterator 'next-offset)))
           (cond
             ((plusp ,offset)
              (decf ,offset)
              (setf (slot-value ,iterator 'next-offset) ,offset)
              (values (,access-fn-name (slot-value ,iterator 'sequence) ,offset) t))
             (t
              (assert (zerop ,offset))
              (values nil nil)))))

       (defmethod skip ((,iterator ,class-name) (,count integer))
         (setf (slot-value ,iterator 'next-offset)
               (max 0 (min (+ (slot-value ,iterator 'next-offset) ,count)
                           (,size-fn-name (slot-value ,iterator 'sequence)))))
         (values)))))

(define-random-access-methods vector-iterator aref length)

(defclass seq-iterator (bidirectional-skip-iterator fork-with-clone trivially-clonable)
  ((sequence
    :initarg :seq
    :initform (fset:empty-seq)
    :type fset:seq)
   (next-offset
    :initarg :first-offset
    :initform 0
    :type (integer 0)))
  (:documentation
   "This class represents an iterator that can traverse a `fset:seq'."))

(defmethod iterator ((seq fset:seq))
  (seq-iterator seq))

(defun seq-iterator (seq)
  "Produce an iterator that traverses the given `fset:seq'."
  (make-instance 'seq-iterator :seq seq))

(defmethod print-object ((iter seq-iterator) stream)
  (print-unreadable-object (iter stream :type t)
    (format stream "(~W" 'fset:seq)
    (do-sequence (value (fork iter))
      (format stream " ~W" value))
    (format stream ")")))

(define-random-access-methods seq-iterator fset:lookup fset:size)

(defclass set-iterator (bidirectional-skip-iterator fork-with-clone trivially-clonable)
  ((sequence
    :initarg :set
    :initform (fset:empty-set)
    :type fset:set)
   (next-offset
    :initarg :first-offset
    :initform 0
    :type (integer 0)))
  (:documentation
   "This class represents an iterator that can traverse a `fset:set'."))

(defmethod iterator ((set fset:set))
  (set-iterator set))

(defun set-iterator (set)
  "Produce an iterator that traverses the given `fset:set'."
  (make-instance 'set-iterator :set set))

(defmethod print-object ((iter set-iterator) stream)
  (print-unreadable-object (iter stream :type t)
    (format stream "(~W" 'fset:set)
    (do-sequence (value (fork iter))
      (format stream " ~W" value))
    (format stream ")")))

(define-random-access-methods set-iterator fset:at-rank fset:size)

(defclass computed-iterator (forward-iterator)
  ((function
    :initarg :function
    :initform (required)
    :type (or symbol function)))
  (:documentation
   "This class represents an iterator that simply calls a function.

The `next' method for this class will simply call the function stored
in the `function' slot.  If the function indicates it has no more
values (i.e. returns a nil second return value), then the `function'
will not be called again."))

(defun dead-computation ()
  (values nil nil))

(defmethod next ((iterator computed-iterator))
  (with-slots (function) iterator
    (multiple-value-bind (value valid-p) (funcall function)
      (unless valid-p
        (setf function 'dead-computation)
        (return-from next (values nil nil)))
      (values value t))))

(defun computed-iterator (function)
  "Produce an iterator that simply calls the given function to produce
values."
  (make-instance 'computed-iterator :function function))

(defmethod print-object ((iter computed-iterator) stream)
  (print-unreadable-object (iter stream :type t)
    (format stream "~W" (slot-value iter 'function))))

(defmacro make-computed-iterator (&body body)
  "Create an iterator.

The body of this macro will be executed each time the iterator needs
to produce a new value.  The body is evaluated in the lexical
environment in which the `make-computed-iterator' form appears.
Within the body, the local macros `stop' and `emit' can be used to
indicate end of sequence or return a value.  After `stop' is
evaluated, the iterator will not be called again.  Both `stop' and
`emit' cause a control transfer out of the body of
`make-computed-iterator'."
  (let ((compute (gensym "COMPUTE"))
        (value (gensym "VALUE")))
    `(labels ((,compute ()
                (macrolet
                    ((emit (,value)
                       `(return-from ,',compute (values ,,value t)))
                     (stop ()
                       '(return-from ,compute (values nil nil))))
                  (emit (progn ,@body)))))
           (make-instance 'computed-iterator :function #',compute))))

(defmacro emit (value)
  "Within the body of `make-computed-iterator', cause the iterator to produce a
value.

It is an error for this macro to be used outside of `make-computed-iterator'."
  (declare (ignore value))
  (error "This macro can only be used within the body of `make-computed-iterator'."))

(defmacro stop ()
  "Within the body of `make-computed-iterator', signal that the iterator has no
values left to produce.

It is an error for this macro to be used outside of `make-computed-iterator'."
  (error "This macro can only be used within the body of `make-computed-iterator'."))

(defclass iterator-transformation (forward-iterator)
  ((underlying-iterator
    :initarg :underlying-iterator
    :initform (required)
    :type forward-iterator))
  (:documentation
   "This class represents an iterator that consumes and transforms the
values of another iterator.

The `next' method for this class simply calls `next' on its underlying
iterator.  You should rely on this in any `next' methods you write for
classes which inherit from `iterator-transformation'.  This allows
transformation subclasses to be layered via inheritance.  For example,
you could create a iterator that maps and then filters by creating a
class that inherits from `mapped-iterator' and `filtered-iterator'.
Naturally, layering in this way has limits, and its often better to
achieve the same result using composition rather than inheritance."))

(defmethod next ((iter iterator-transformation))
  (next (slot-value iter 'underlying-iterator)))

(defclass mapped-iterator (iterator-transformation)
  ((map-function
    :initarg :map-function
    :initform (required)
    :type (or symbol function)))
  (:documentation
   "This class applies a function to every value produced by another
iterator.

This is the iterator equivalent of `mapcar'."))

(defun mapped-iterator (iterator function)
  "Create a new iterator that applies the given function to each value
produced by the given iterator."
  (make-instance 'mapped-iterator :underlying-iterator iterator :map-function function))

(defmethod next ((iterator mapped-iterator))
  (multiple-value-bind (value valid-p) (call-next-method)
    (unless valid-p
      (return-from next
        (values nil nil)))

    (values (funcall (slot-value iterator 'map-function) value) t)))

(defmethod print-object ((iter mapped-iterator) stream)
  (print-unreadable-object (iter stream :type t)
    (format stream "~W ~W" (slot-value iter 'map-function)
            (slot-value iter 'underlying-iterator))))

(defclass filtered-iterator (iterator-transformation)
  ((filter-function
    :initarg :filter-function
    :initform (required)
    :type (or symbol function)))
  (:documentation
   "This class filters traverses an iterator and removes elements that
don't satisfy some predicate.

This is the iterator equivalent of `remove-if-not'."))

(defmethod next ((iterator filtered-iterator))
  (let ((function (slot-value iterator 'filter-function)))
    (loop
       (multiple-value-bind (value valid-p) (call-next-method)
         (unless valid-p
           (return-from next
             (values nil nil)))

         (when (funcall function value)
           (return-from next (values value t)))))))

(defun filtered-iterator (iter function)
  "Create a new iterator that only contains the elements produced by
`iter' where `function' returns non-nil.

For example,
 (iterable-values (filtered-iterator (list-iterator '(1 2 3 4) #'oddp)))
will return
 #(1 3)"
  (make-instance 'filtered-iterator :underlying-iterator iter
                 :filter-function function))

(defclass concatenated-iterator (iterator-transformation)
  ((current-iterator
    :initform nil))
  (:documentation
   "This class traverses the sequences returned by an iterator.

This class can be used to traverse a homogeneous collection of
sequences.  For example,
    (let* ((sequences (list #(1 2 3) '(4 5 6)))
           (sequences-iter (list-iterator sequences))
           (iter (make-instance 'concatenated-iterator :underlying-iterator sequences-iter)))
      (iterable-values iter 'list))
    ; => (1 2 3 4 5 6)

See also `concatenate-iterable-collection'."))

(defmethod next ((iterator concatenated-iterator))
  (with-slots (current-iterator) iterator
    (loop
       (cond
         ((null current-iterator)
          (multiple-value-bind (next-iterator valid-p) (call-next-method)
            (unless valid-p
              (return-from next
                (values nil nil)))
            (setf current-iterator (iterator next-iterator))))

         (t
          (multiple-value-bind (next-value valid-p) (next current-iterator)
            (if valid-p
                (return-from next
                  (values next-value t))
                (setf current-iterator nil))))))))

(defmethod print-object ((iter concatenated-iterator) stream)
  (print-unreadable-object (iter stream :type t)
    (when (slot-value iter 'current-iterator)
      (format stream "~W " (slot-value iter 'current-iterator)))
    (format stream "~W" (slot-value iter 'underlying-iterator))))

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
  (make-instance 'concatenated-iterator :underlying-iterator (iterator iterable)))

(defun concatmapped-iterator (iterable function)
  "Map `function' onto the elements of `iterable', then concatenate
the iterable sequences that result.

The following forms are semantically equivalent.
    (concatenate-iterable-collection (mapped-iterator (iterator iter) fn))
    (concatmapped-iterator iter fn)"
  (concatenate-iterable-collection (mapped-iterator (iterator iterable) function)))

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

(defgeneric builder-for-type (type)
  (:documentation
   "Return a function which can be repeatedly invoked to build a
sequence of the given type.

The returned function should accept one argument: the next item to add
to the sequence.  The returned function should return the constructed
sequence.  If the builder function is invoked without arguments, it
should return the current value of the sequence without modifying
it."))

(defmethod builder-for-type ((type (eql 'vector)))
  (let ((result (make-extensible-vector)))
    (lambda (&optional (item nil item-p))
      (when item-p
        (vector-push-extend item result))
      result)))

(defmethod builder-for-type ((type (eql 'list)))
  (let (result
        last-cell)
    (lambda (&optional (item nil item-p))
      (when item-p
        (cond
          ((null last-cell)
           (setf result (cons item nil))
           (setf last-cell result))

          (t
           (setf (cdr last-cell) (cons item nil)))))
      result)))

(defmethod builder-for-type ((type (eql 'fset:seq)))
  (let ((result (fset:empty-seq)))
    (lambda (&optional (item nil item-p))
      (when item-p
        (fset:push-last result item))
      result)))

(defmethod builder-for-type ((type (eql 'fset:set)))
  (let ((result (fset:empty-set)))
    (lambda (&optional (item nil item-p))
      (when item-p
        (fset:adjoinf result item))
      result)))

(defun iterable-values (iter &optional (output-type 'vector))
  "Extract all remaining values from the given iterable and return
them in a sequence of the given type.

This function uses `builder-for-type' to aquire a builder function for
the given `output-type'.  It then iterates over `iter' and repeatedly
invokes the builder on the values produced by `iter'.  At the end, it
returns the sequence produced by the builder.

Although this function is intended to extract values from an iterator,
you can actually use it to convert from any iterable type to any
buildable type.  If the `iter' already conforms to `output-type' then
`iter' is returned unmodified."
  (when (typep iter output-type)
    (return-from iterable-values iter))
  (let* ((builder (builder-for-type output-type))
         (result (funcall builder)))
    (do-sequence (value iter :result result)
      (setf result (funcall builder value)))))

(defclass forkable-wrapper-iterator (forward-iterator fork-with-clone trivially-clonable)
  ((buffer
    :initform (cons 'tail 'tail)
    :type cons)
   (underlying-iterator
    :initform (required)
    :initarg :underlying-iterator
    :type forward-iterator))
  (:documentation
   "This class wraps another iterator and supports being forked.

If you have an iterator that isn't forkable (or has unknown
forkability), you can use this class to create an iterator that
definitely supports `fork'."))

(defmethod print-object ((iterator forkable-wrapper-iterator) stream)
  (print-unreadable-object (iterator stream :type t)
    (with-slots (buffer underlying-iterator) iterator
      (format stream "~W ~W" buffer underlying-iterator))))

(defun forkable-wrapper-iterator-position-token (iter)
  "Returns a value representing the position of the iterator.

This is an opaque value that can be compared to other position tokens
with `eq'.  The exact nature of the token is unspecified and subject
to change at any time.  If two lookahead iterators have `eq' position
tokens then they are at the same place in the same stream."
  (with-slots (buffer) iter
    buffer))

(defmethod next ((iterator forkable-wrapper-iterator))
  (with-slots (buffer underlying-iterator) iterator
    (when (eq (cdr buffer) 'eof)
      (return-from next (values nil nil)))

    (cond ((eq (cdr buffer) 'tail)
           ;; Buffer is empty.  Add something to the back
           (multiple-value-bind (value more) (next underlying-iterator)
             (cond (more
                    (let ((new-tail (cons 'tail 'tail)))
                      (setf (car buffer) value
                            (cdr buffer) new-tail
                            buffer (cdr buffer))))

                   (t
                    (setf (cdr buffer) 'eof)))
             (values value (not (not more)))))

          (t
           (let ((value (car buffer)))
             (setf buffer (cdr buffer))
             (values value t))))))

(defun move-forkable-wrapper-iterator-to (iter-to-change model-iter)
  "Change the future of a `forkable-wrapper-iterator' to match that of
a different `forkable-wrapper-iterator'.

After this function returns, the first argument (`iter-to-change')
shall produce the exact same sequence of values as the second
argument (`model-iter').  The given iterators must be in the same
family.  The `fork' function can be used to produce a new iterator in
a given family."
  (with-slots ((b-underlying-iterator underlying-iterator)
               (b-buffer buffer))
      iter-to-change
    (with-slots ((a-underlying-iterator underlying-iterator)
                 (a-buffer buffer))
        model-iter
      (unless (eq a-underlying-iterator b-underlying-iterator)
        (error "These iterators aren't in the same family"))
      (setf b-buffer a-buffer)))
  (values))

(defun forkable-wrapper-iterator (iter)
  "Produce a `forkable-wrapper-iterator' that emits the same values as
`iter'."
  (make-instance 'forkable-wrapper-iterator :underlying-iterator iter))

(defun sequence-starts-with-p (iterable iterable-prefix &key (test 'eql))
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
               (return-from sequence-starts-with-p t))
             value))
         (next-found ()
           (multiple-value-bind (value valid-p) (next seq)
             (unless valid-p
               (return-from sequence-starts-with-p nil))
             value)))
      (loop :for expected = (next-expected)
         :for found = (next-found) :do
         (unless (funcall test found expected)
           (return-from sequence-starts-with-p nil)))
      (assert nil nil "This function doesn't return normally"))))

(defun sequence-map (sequence function &optional (output-type 'iterator))
  "Apply `function' to the elements of `sequence' and return a
sequence containing the results.

This is the iterator version of `map'.  This function is just a
trivial combination of `iterable-values' and `mapped-iterator'."
  (iterable-values (mapped-iterator (iterator sequence) function) output-type))

(defun sequence-filter (sequence function &optional (output-type 'iterator))
  "Produce a sequence that contains elements of `sequence' for which
`function' returned a non-nil value.

This is the iterator version of `remove-if-not' (except you get to
choose the output format).  This function is just a trivial
combination of `iterable-values' and `filtered-iterator'."
  (iterable-values (filtered-iterator (iterator sequence) function) output-type))
