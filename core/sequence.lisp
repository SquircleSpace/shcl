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

(defpackage :shcl/core/sequence
  (:use :common-lisp)
  (:import-from :shcl/core/utility #:required #:optimization-settings)
  (:import-from :shcl/core/iterator #:iterator #:next)
  (:import-from :bordeaux-threads #:make-lock #:with-lock-held)
  (:import-from :fset)
  (:export
   #:empty-p #:attach #:empty-of #:empty-for-type #:walk #:head #:tail
   #:attachf #:popf

   #:immutable-cons #:empty-immutable-list #:immutable-list #:immutable-list*
   #:lazy-sequence #:walk-iterator))
(in-package :shcl/core/sequence)

(optimization-settings)

;;; Walkable protocol

(defgeneric head (walkable)
  (:documentation
   "Return the first element of the given walkable.

A walkable type is one that has methods for both `head' and `tail'.
`head' is conceptually equivalent to the `car' function, and `tail' is
conceptually equivalent to the `cdr' function.  Methods on this
generic function must be thread safe.  That is, a walkable may be used
simultaneously from multiple threads.

If the walkable is empty (i.e. has no values left to produce) then
this function should return two nil values.  If the walkable isn't
empty then this function shall return two values: the first element of
the sequence and a non-nil value."))

(defgeneric tail (walkable)
  (:documentation
   "Return a walkable representing everything except the first element
in `walkable'.

A walkable type is one that has methods for both `head' and `tail'.
`head' is conceptually equivalent to the `car' function, and `tail' is
conceptually equivalent to the `cdr' function.  Methods on this
generic function must be thread safe.  That is, a walkable may be used
simultaneously from multiple threads.

If `walkable' is empty then this function will return `walkable'
unmodified."))

(defgeneric empty-p (walkable)
  (:documentation
   "Return non-nil iff the given walkable has elements left.

Methods on this generic function should produce the same result as
    (not (nth-value 1 (head walkable))).
In fact, this generic function has a method that falls back to using
`head' as shown above.  This generic function is provided in case it
is cheaper to check for empty-ness than it is to retrieve the first
element."))

(defmethod empty-p (walkable)
  (not (nth-value 1 (head walkable))))

;;; Sequence protocol

(defgeneric attach (sequence &rest values)
  (:documentation
   "Return a new version of `sequence' that has the values in `values'
included in it.

Methods may choose to attach the given values to the sequence at any
location.  For example, attaching a value to a list will put that
value at the head of the list, but attaching a value to some other
sequence type may put the value at the end of the sequence.

If `values' contains more than one element, this generic function
should behave in a left-associative way.  So, for example, these two
forms should have equivalent results.
    (attach sequence 1 2 3)
    (attach (attach (attach sequence 1) 2) 3)

If no values are provided then this generic function should return
`sequence'.

This generic function is not allowed to modify `sequence'."))

(defgeneric empty-of (sequence)
  (:documentation
   "Return an empty sequence of the same type as `sequence'.

Note: This generic function is intended to be used with the rest of
the sequence protocol.  In particular, this is intended to be used in
conjunction with `attach'.  If you can't write an efficient method for
`attach' then you probably shouldn't write a method for this generic
function, either."))

(defgeneric empty-for-type (type)
  (:documentation
   "Return an empty sequence of the named type.

Note: This generic function is intended to be used with the rest of
the sequence protocol.  In particular, this is intended to be used in
conjunction with `attach'.  If you can't write an efficient method for
`attach' then you probably shouldn't write a method for this generic
function, either."))

(defgeneric walk (sequence)
  (:documentation
   "Return an object that contains all the values contained in
`sequence' and implements the walkable protocol.

See the documentation for `head' and `tail' for more information about
the walkable protocol."))

(define-modify-macro attachf (&rest values) attach
  "Replace the sequence stored in `sequence-place' with one that has
had the given values attached.

This is a trivial place-modification macro wrapper around `attach'.

Assuming the relevant `attach' method is following the rules, this
doesn't actually modify the sequence.  It only modifies the place
where the sequence is stored.")

(defmacro popf (sequence-place &environment env)
  "This macro operates like `pop' for anything that is walkable."
  ;; The naive expansion that just calls setf directly would evaluate
  ;; sequence-place multiple times.
  (multiple-value-bind
        (vars vals store-vars store-form access-form)
      (get-setf-expansion sequence-place env)
    (let ((temporary-bindings
            (loop :for var :in vars :for val :in vals :collect (list var val)))
          (body
            `(multiple-value-prog1 (head ,access-form)
               (multiple-value-bind ,store-vars (tail ,access-form)
                 ,store-form))))
      (if temporary-bindings
          `(let* ,temporary-bindings
             ,body)
          body))))

;;; Immutable lists

(defstruct immutable-list
  "An immutable list is like a normal list except that you can't
  change the spine of the list after you create it."
  ;; Okay, its actually a mutable struct... but we're not going to
  ;; export a symbol that can be used to set the slots.
  head
  tail)

;; This is a bit hacky.  We're not using nil to represent empty list
;; because its nice having a way to represent empty list that is
;; different from a mutable empty list.  If the immutable empty list
;; isa immutable-list then things like method specialization work
;; nicely.
(defstruct (immutable-nil (:include immutable-list))
  "This struct is used to represent the end of an immutable list.")

(defvar *immutable-nil* (make-immutable-nil))

(defun empty-immutable-list ()
  "Return an object representing an empty immutable list.

See `immutable-cons' and `immutable-list'."
  *immutable-nil*)

(defun immutable-cons (head tail)
  "Return a new `immutable-list' with the given head and tail.

This function is analogous to `cons', but it returns an object that
cannot be modified."
  (make-immutable-list :head head :tail tail))

(defun immutable-list (&rest things)
  "Produce an `immutable-list' that contains the given values.

This is a convenience function that uses `immutable-cons' and
`empty-immutable-list' to construct the list."
  (labels
      ((visit (list)
         (if list
             (immutable-cons (car list) (visit (cdr list)))
             (empty-immutable-list))))
    (visit things)))

(defun immutable-list* (&rest things)
  "Produce an `immutable-list' in the same way that `list*' would.

This is the `immutable-list' version of `list*'.  The final element of
`things' is treated as a list that the other elements of `things' are
cons'd onto."
  (labels
      ((visit (sublist)
         (if (cdr sublist)
             (immutable-cons (car sublist) (visit (cdr sublist)))
             (car sublist))))
    (if things
        (visit things)
        (empty-immutable-list))))

(defmethod walk ((immutable-list immutable-list))
  immutable-list)

(defmethod head ((immutable-list immutable-list))
  (values (immutable-list-head immutable-list) t))

(defmethod head ((immutable-nil immutable-nil))
  (values nil nil))

(defmethod tail ((immutable-list immutable-list))
  (immutable-list-tail immutable-list))

(defmethod tail ((immutable-nil immutable-nil))
  immutable-nil)

(defmethod empty-p ((immutable-list immutable-list))
  nil)

(defmethod empty-p ((immutable-nil immutable-nil))
  t)

(defmethod attach ((immutable-list immutable-list) &rest values)
  "Attach the given values to the front of the list."
  (dolist (value values)
    (setf immutable-list (immutable-cons value immutable-list)))
  immutable-list)

(defmethod empty-of ((immutable-list immutable-list))
  (empty-immutable-list))

(defmethod empty-for-type ((sym (eql 'immutable-list)))
  (empty-immutable-list))

;;; Lazy sequences

(defclass lazy-sequence ()
  ((generator
    :initarg :generator
    :initform (required))
   (seq)
   (lock
    :initform (make-lock)))
  (:documentation
   "This class represents a sequence that is produced lazily.

Instances of this class represents a sequence that may be as-of-yet
unevaluated.  This class has methods for the walkable and the sequence
protocols.  The methods simply ensure that the sequence has been
evaluated and then invoke the same function on the concrete
sequence.

This class is typically used with `immutable-list' to produce a lazy
list.  See the `lazy-sequence' macro documentation for an example of
this."))

(defmethod walk ((lazy-sequence lazy-sequence))
  lazy-sequence)

(defun force-lazy-sequence (lazy-sequence)
  (with-slots (generator seq lock) lazy-sequence
    (with-lock-held (lock)
      (when (slot-boundp lazy-sequence 'generator)
        (setf seq (walk (funcall generator)))
        (slot-makunbound lazy-sequence 'generator))
      seq)))

(defmethod head ((lazy-sequence lazy-sequence))
  (let ((seq (force-lazy-sequence lazy-sequence)))
    (head seq)))

(defmethod tail ((lazy-sequence lazy-sequence))
  (let ((seq (force-lazy-sequence lazy-sequence)))
    (tail seq)))

(defmethod empty-p ((lazy-sequence lazy-sequence))
  (let ((seq (force-lazy-sequence lazy-sequence)))
    (empty-p seq)))

(defmethod attach ((lazy-sequence lazy-sequence) &rest values)
  (let ((seq (force-lazy-sequence lazy-sequence)))
    (apply 'attach seq values)))

(defmethod empty-of ((lazy-sequence lazy-sequence))
  (let ((seq (force-lazy-sequence lazy-sequence)))
    (empty-of seq)))

(defmacro lazy-sequence (&body body)
  "Create a `lazy-sequence' object that evalutes `body' when it needs
to generate a concrete sequence.

This macro is a trivial wrapper around `make-instance'.

When used in conjunction with `immutable-list', this macro can be used
to create lazy lists.  For example, this code snippet creates a lazy
list containing the natural numbers.

    (labels
        ((generate (value)
           (lazy-sequence
             (immutable-cons value (generate (1+ value))))))
      (generate 0))

If you're trying to generate a lazy list, you may also find
`walk-iterator' helpful."
  `(make-instance 'lazy-sequence :generator (lambda () ,@body)))

;;; Iterators

(defun walk-iterator (iterator)
  "Produce a `lazy-sequence' that contains the values produced by `iterator'.

The given iterator is only invoked when the lazy sequence requires a
fresh value.  That can happen at any time and on any thread.  It is a
very bad idea to walk an iterator that has side effects or shared with
other code.

Since iterators are inherently stateful, there isn't a method on
`walk' for them -- it would be too easy to accidentally create a lazy
sequence with side effects!  However, its actually quite useful to
walk an iterator.  Some things are naturally expressed with iterator.
For example, these forms produce lazy sequences that contain the same
values.

    ;; Functionally
    (labels
        ((generate (value)
           (lazy-sequence
             (immutable-cons value (generate (1+ value))))))
      (generate 0))

    ;; Imperatively
    (let ((value -1))
      (walk-iterator (make-iterator () (emit (incf value)))))

Note that the imperative version has side effects, but they are only
observable by the iterator itself.  This is okay!  The lazy sequence
that this function generates will provide the necessary
synchronization to ensure that the iterator isn't invoked in parallel.
It would still be a very bad idea to share the iterator's internal
state with some other unit of code."
  (lazy-sequence
   (multiple-value-bind (value valid-p) (next iterator)
     (if valid-p
         (immutable-cons value (walk-iterator iterator))
         (empty-immutable-list)))))

;;; Lists

(defmethod walk ((list list))
  list)

(defmethod head ((list list))
  (if list
      (values (car list) t)
      (values nil nil)))

(defmethod tail ((list list))
  (cdr list))

(defmethod empty-p ((list list))
  (null list))

(defmethod attach ((list list) &rest values)
  "Attach the given values to the front of the list."
  (dolist (value values)
    (setf list (cons value list)))
  list)

(defmethod empty-of ((list list))
  nil)

(defmethod empty-for-type ((sym (eql 'list)))
  nil)

;;; vector

(defmethod walk ((vector vector))
  (walk-iterator (iterator vector)))

;;; fset:seq

(defmethod walk ((seq fset:seq))
  seq)

(defmethod head ((seq fset:seq))
  (multiple-value-bind (value valid-p) (fset:first seq)
    (if valid-p
        (values value t)
        (values nil nil))))

(defmethod tail ((seq fset:seq))
  (fset:less-first seq))

(defmethod empty-p ((seq fset:seq))
  (fset:empty? seq))

(defmethod attach ((seq fset:seq) &rest values)
  "Attach the given values to the end of the sequence."
  (dolist (value values)
    (setf seq (fset:with-last seq value)))
  seq)

(defmethod empty-of ((seq fset:seq))
  (fset:empty-seq))

(defmethod empty-for-type ((sym (eql 'fset:seq)))
  (fset:empty-seq))

;;; fset:set

(defmethod walk ((set fset:set))
  set)

(defmethod head ((set fset:set))
  (fset:least set))

(defmethod tail ((set fset:set))
  (multiple-value-bind (value valid-p) (fset:least set)
    (if valid-p
        (fset:less set value)
        set)))

(defmethod empty-p ((set fset:set))
  (fset:empty? set))

(defmethod attach ((set fset:set) &rest values)
  (dolist (value values)
    (setf set (fset:with set value)))
  set)

(defmethod empty-of ((set fset:set))
  (fset:empty-set))

(defmethod empty-for-type ((sym (eql 'fset:set)))
  (fset:empty-set))

;;; fset:map

(defmethod walk ((map fset:map))
  map)

(defmethod head ((map fset:map))
  (multiple-value-bind (key value valid-p) (fset:least map)
    (if valid-p
        (values (cons key value) t)
        (values nil nil))))

(defmethod tail ((map fset:map))
  (multiple-value-bind (key value valid-p) (fset:least map)
    (declare (ignore value))
    (if valid-p
        (fset:less map key)
        map)))

(defmethod empty-p ((map fset:map))
  (fset:empty? map))

(defmethod attach ((map fset:map) &rest values)
  (dolist (pair values)
    (setf map (fset:with map (car pair) (cdr pair))))
  map)

(defmethod empty-of ((map fset:map))
  (fset:empty-map))

(defmethod empty-for-type ((sym (eql 'fset:map)))
  (fset:empty-map))

;;; fset:bag

(defmethod walk ((bag fset:bag))
  bag)

(defmethod head ((bag fset:bag))
  (multiple-value-bind (value count valid-p) (fset:least bag)
    (declare (ignore count))
    (if valid-p
        (values value t)
        (values nil nil))))

(defmethod tail ((bag fset:bag))
  (multiple-value-bind (value count valid-p) (fset:least bag)
    (declare (ignore count))
    (if valid-p
        (fset:less bag value)
        bag)))

(defmethod empty-p ((bag fset:bag))
  (fset:empty? bag))

(defmethod attach ((bag fset:bag) &rest values)
  (dolist (value values)
    (setf bag (fset:with bag value)))
  bag)

(defmethod empty-of ((bag fset:bag))
  (fset:empty-bag))

(defmethod empty-for-type ((sym (eql 'fset:bag)))
  (fset:empty-bag))
