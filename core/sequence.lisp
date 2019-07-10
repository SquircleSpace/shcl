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
  (:import-from :shcl/core/utility
   #:required #:optimization-settings #:make-extensible-vector
   #:document #:define-documentation-type)
  (:import-from :bordeaux-threads #:make-lock #:with-lock-held)
  (:import-from :fset)
  (:import-from :alexandria)
  (:export
   #:empty-p #:attach #:empty-of #:empty-for-type #:walk #:head #:tail
   #:attachf #:popf #:do-while-popf #:lazy-map #:eager-map #:lazy-filter
   #:eager-filter #:pour-from #:concatenate-sequences #:flatten-sequence
   #:eager-flatmap-sequence #:walkable-to-list #:sort-sequence #:do-sequence
   #:sequence-find-if #:sequence-find-if-not #:sequence-find
   #:eager-sequence-remove-if #:eager-sequence-remove-if-not
   #:eager-sequence-remove #:sequence-count-if #:sequence-count-if-not
   #:sequence-count #:sequence-nth-tail

   #:immutable-cons #:empty-immutable-list #:immutable-list #:immutable-list*
   #:lazy-sequence #:sequence-starts-with-p #:wrap-with #:cache-impure))
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

(defmethod head (walkable)
  (let ((walker (walk walkable)))
    (head walker)))

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

(defmethod tail (walkable)
  (let ((walker (walk walkable)))
    (tail walker)))

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
cannot be modified.

`tail' can be any sequence.  It doesn't need to be another
`immutable-list'."
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
    :initform (required)))
  (:documentation
   "This class represents a sequence that is produced lazily.

Instances of this class represents a sequence that hasn't been
evaluated, yet.  The generator function is expected to return a
sequence.  Whenever a sequence function (e.g. `head', `tail',
`attach', etc.) is invoked on a `lazy-sequence', the generator
function is evaluated and the sequence function is invoked on the
result.

This class is typically used with `immutable-list' to produce a lazy
list.  See the `lazy-sequence' macro documentation for an example of
this."))

(defun force-lazy-sequence (lazy-sequence)
  (with-slots (generator) lazy-sequence
    (funcall generator)))

(defmethod walk ((lazy-sequence lazy-sequence))
  lazy-sequence)

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

(defun cache-impure (function)
  "Wrap `function' with a function that caches the result of
`function'.

`function' is assumed to take no arguments.  Synchronization is used
to ensure that `function' is called at most once.  This is meant to be
used with the `wrap-with' declaration in a `lazy-sequence' body.  The
name refers to the fact that the given `function' is assumed to have
stateful effects."
  (let ((lock (make-lock))
        evaluated-p
        values)
    (lambda ()
      (with-lock-held (lock)
        (unless evaluated-p
          (setf values (multiple-value-list (funcall function)))
          (setf evaluated-p t))
        (values-list values)))))

;; Ideally, wrap-with would be documented in the declaration doc-type,
;; but that symbol is owned by the common-lisp package.
;; Implementations are well within their rights to have documentation
;; methods that know how to read/write the declaration doc-type.  It
;; would be sad if we overwrote those methods, here.
(define-documentation-type lazy-sequence)
(document wrap-with lazy-sequence
  "Wrap the body of a `lazy-sequence' using a wrapping function.

This declaration is only valid at the start of a `lazy-sequence'
macro body.  See the `lazy-sequence' macro for more information.")

(defmacro lazy-sequence (&whole whole &body body)
  "Create a `lazy-sequence' object that evalutes `body' when it needs
to generate a concrete sequence.

When used in conjunction with `immutable-list', this macro can be used
to create lazy lists.  For example, this code snippet creates a lazy
list containing the natural numbers.

    (labels
        ((generate (value)
           (lazy-sequence
             (immutable-cons value (generate (1+ value))))))
      (generate 0))

By default, the returned sequence will evaluate `body' every time a
sequence function is called on it.  You must ensure that the body is
idempotent, thread-safe, and relatively cheap to evaluate.

Since caching is often desirable, this macro will recognize the
`wrap-with' declaration.  When you use the `wrap-with' declaration,
the body of the macro will be turned into a 0-argument lambda that is
passed into the function named by the `wrap-with' declaration.  The
wrapping function must return a funcallable object.  That object will
be called whenever the `lazy-sequence' needs to produce its value.
When combined with a wrapping function like `cache-impure', you'll get
a `lazy-sequence' that evalutes `body' at most once.

If `wrap-with' appears multiple times, each wrapping function will be
called in the reverse of the order that they appear.  For example, the
following lazy sequences will have identical behaviors.

    (lazy-sequence
      (declare (wrap-with first-wrapper)
               (wrap-with second-wrapper))
      (body-forms))
    (let ((fn (first-wrapper (second-wrapper (lambda () (body-forms))))))
      (lazy-sequence
        (funcall fn)))

Note that you can also name a macro instead of a function."
  (multiple-value-bind (body declarations) (alexandria:parse-body body :whole whole)
    (let (wrapping-functions)
      (labels
          ((consume-declaration (declaration)
             (destructuring-bind (declare-sym &rest decl-clauses) declaration
               (assert (eq declare-sym 'declare))
               (unless (find 'wrap-with decl-clauses :key (lambda (entry) (and (consp entry) (car entry))))
                 (return-from consume-declaration declaration))

               (let ((result-clauses (make-extensible-vector)))
                 (dolist (clause decl-clauses)
                   (cond
                     ((and (consp clause) (eq (car clause) 'wrap-with))
                      (destructuring-bind (decl-name value) clause
                        (assert (eq decl-name 'wrap-with))
                        (check-type value symbol)
                        (push value wrapping-functions)))
                     (t
                      (vector-push-extend clause result-clauses))))

                 (unless (zerop (length result-clauses))
                   `(declare ,@(coerce result-clauses 'list)))))))

        (setf declarations (mapcar #'consume-declaration declarations))
        (setf declarations (remove nil declarations)))

      (let ((generator `(lambda () ,@declarations ,@body)))
        (dolist (wrapper wrapping-functions)
          (setf generator `(,wrapper ,generator)))
        `(make-instance 'lazy-sequence :generator ,generator)))))

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

(defstruct vector-walkable
  vector
  offset)

(defmethod walk ((vector vector))
  (if (zerop (length vector))
      (make-vector-walkable)
      (make-vector-walkable :vector vector :offset 0)))

(defmethod walk ((vector-walkable vector-walkable))
  vector-walkable)

(defmethod head ((vector vector))
  (if (zerop (length vector))
      (values nil nil)
      (values (aref vector 0) t)))

(defmethod head ((vector-walkable vector-walkable))
  (if (vector-walkable-vector vector-walkable)
      (values (aref (vector-walkable-vector vector-walkable)
                    (vector-walkable-offset vector-walkable))
              t)
      (values nil nil)))

(defmethod tail ((vector-walkable vector-walkable))
  (unless (vector-walkable-vector vector-walkable)
    (return-from tail vector-walkable))
  (if (>= (1+ (vector-walkable-offset vector-walkable))
          (length (vector-walkable-vector vector-walkable)))
      (make-vector-walkable)
      (make-vector-walkable :vector (vector-walkable-vector vector-walkable)
                            :offset (1+ (vector-walkable-offset vector-walkable)))))

(defmethod tail ((vector vector))
  (if (>= 1 (length vector))
      (make-vector-walkable)
      (make-vector-walkable :vector vector :offset 1)))

(defmethod empty-p ((vector-walkable vector-walkable))
  (null (vector-walkable-vector vector-walkable)))

(defmethod empty-p ((vector vector))
  (zerop (length vector)))

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

;;; Utilities

(defmacro do-while-popf ((var walkable-place &optional result) &body body)
  "As long as `walkable-place' contains a non-empty walkable, this
macro will pop an element off the walkable, bind it to `var', and evaluate
`body'.

This macro is sort of like the walkable version of `dolist'.  However,
in an effort to avoid retaining the head of the sequence, this macro
is intentionally destructive.  That is, `walkable-place' will get
modified each time this macro evaluates `body'.

This macro repeatedly uses `popf' to remove an element from
`walkable-place' until the second return value is nil.  As a result,
`walkable-place' will be evaluated multiple times."
  (let ((value (gensym "VALUE"))
        (valid-p (gensym "VALID-P")))
    `(loop
       (multiple-value-bind (,value ,valid-p) (popf ,walkable-place)
         (unless ,valid-p
           (return ,result))
         (let ((,var ,value))
           ,@body)))))

(defmacro do-sequence ((var sequence &optional result) &body body)
  "Repeatedly evaluate `body' with `var' bound to the successive
elements in `sequence'.

This macro is the walkable version of `dolist'.

Note that unlike regular cons lists, sequences may be lazy and have an
infinite number of distinct elements.  Thus, it is important to avoid
unnecessarily retaining a pointer to an early part of a sequence while
traversing it with this macro.  See also `do-while-popf'."
  (let ((walker (gensym "WALKER")))
    `(let ((,walker ,sequence))
       (do-while-popf (,var ,walker ,result)
         ,@body))))

(defun sort-sequence (sequence predicate &key key)
  "Return a sequence with the same elements as `sequence' but sorted
in the order determined by `predicate'.

This is a non-mutating, generic version of the standard `sort'.  The
`predicate' and `key' arguments work exactly like in the standard
`sort' function."
  (let ((result (make-extensible-vector)))
    (do-while-popf (element sequence)
      (vector-push-extend element result))
    (sort result predicate :key key)))

(defun sequence-nth-tail (sequence nth)
  "Pop off `nth' elements from `sequence' and return the result.

If `sequence' has at least `nth' elements then the second return value
will be 0.  If `sequence' has fewer than `nth' elements then the
second return value will contain the difference between `nth' and the
length of `sequence'."
  (check-type nth (integer 0))

  (loop :for remaining :above 0 :downfrom nth :do
    (multiple-value-bind (head valid-p) (popf sequence)
      (declare (ignore head))
      (unless valid-p
        (return-from sequence-nth-tail
          (values sequence remaining)))))

  (values sequence 0))

(defun sequence-find-if (predicate sequence &key (start 0) end key)
  "Search through `sequence' for an element that satisfies `predicate'.

This is a generic version of Common Lisp's `find-if'.  Note: unlike
`find-if', this function does not support the from-end keyword
argument.

In addition to returning the item that was found, this function also
returns the sequence starting at the point where the item was found.
If no item was found then this function returns nil for both the first
and second return value."
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))

  (when end
    (cond
      ((< end start)
       (error "end must not be less than start"))
      ((equal end start)
       (return-from sequence-find-if
         (values nil nil)))))

  (multiple-value-bind
        (new-sequence uncompleted-steps)
      (sequence-nth-tail sequence start)
    (unless (zerop uncompleted-steps)
      (return-from sequence-find-if
        (values nil nil)))
    (setf sequence new-sequence))

  (when end
    (decf end start)
    (assert (plusp end) (end) "This should be impossible"))

  (let ((previous-sequence sequence))
    (do-while-popf (item sequence)
      (when (funcall predicate (if key (funcall key item) item))
        (return-from sequence-find-if
          (values item previous-sequence)))
      (when end
        (decf end)
        (when (not (plusp end))
          (return-from sequence-find-if
            (values nil nil))))
      (setf previous-sequence sequence))))

(defun sequence-find-if-not (predicate sequence &rest args &key (start 0) end key)
  "Search through `sequence' for an element that fails to satisfy
`predicate'.

This is a generic version of Common Lisp's `find-if-not'.  Note:
unlike `find-if-not', this function does not support the from-end
keyword argument.

In addition to returning the item that was found, this function also
returns the sequence starting at the point where the item was found.
If no item was found then this function returns nil for both the first
and second return value."
  (declare (ignore start end key))
  (apply 'sequence-find-if (lambda (item) (not (funcall predicate item))) sequence args))

(defun finder-predicate (item test test-not)
  (cond
    ((and (null test)
          (null test-not))
     (lambda (seq-item)
       (eql item seq-item)))
    ((and test test-not)
     (error "Only one of test or test-not should be provided"))
    (test
     (lambda (seq-item)
       (funcall test item seq-item)))
    (test-not
     (lambda (seq-item)
       (not (funcall test-not item seq-item))))
    (t
     (assert nil nil "This should be impossible"))))

(defun sequence-find (item sequence &key test test-not (start 0) end key)
  "Search through `sequence' for an element that matches `item'.

This is a generic version of Common Lisp's `find'.  Note: unlike
`find', this function does not support the from-end keyword argument.

In addition to returning the item that was found, this function also
returns the sequence starting at the point where the item was found.
If no item was found then this function returns nil for both the first
and second return value."
  (sequence-find-if
   (finder-predicate item test test-not)
   sequence :start start :end end :key key))

(defun eager-sequence-remove-if (predicate sequence output-sequence &key (start 0) end count key)
  "Search through `sequence', remove every element that satisfies
`predicate', and attach the remaining elements to `output-sequence'.

This is a generic version of Common Lisp's `remove-if'.  Note: unlike
`remove-if', this function does not support the from-end keyword
argument."
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))
  (check-type count (or null (integer 0)))

  (when end
    (cond
      ((< end start)
       (error "end must not be less than start"))
      ((equal end start)
       (return-from eager-sequence-remove-if
         (pour-from sequence output-sequence)))))

  (dotimes (count start)
    (multiple-value-bind (head valid-p) (popf sequence)
      (unless valid-p
        (return-from eager-sequence-remove-if output-sequence))
      (attachf output-sequence head)))

  (when end
    (decf end start)
    (assert (plusp end) (end) "This should be impossible"))

  (when (and count (minusp count))
    (setf count 0))

  (when (and count (zerop count))
    (return-from eager-sequence-remove-if
      (pour-from sequence output-sequence)))

  (do-while-popf (item sequence)
    (cond
      ((funcall predicate (if key (funcall key item) item))
       (when count
         (decf count)
         (when (zerop count)
           (return-from eager-sequence-remove-if
             (pour-from sequence output-sequence)))))

      (t
       (attachf output-sequence item)))

    (when end
      (decf end)
      (when (zerop end)
        (return-from eager-sequence-remove-if
          (pour-from sequence output-sequence)))))

  output-sequence)

(defun eager-sequence-remove-if-not (predicate sequence output-sequence &rest args &key (start 0) end count key)
  "Search through `sequence', remove every element that fails to
satisfy `predicate', and attach the remaining elements to
`output-sequence'.

This is a generic version of Common Lisp's `remove-if-not'.  Note:
unlike `remove-if-not', this function does not support the from-end
keyword argument."
  (declare (ignore start end key count))
  (apply 'eager-sequence-remove-if
         (lambda (item) (not (funcall predicate item)))
         sequence
         output-sequence
         args))

(defun eager-sequence-remove (item sequence output-sequence &key test test-not (start 0) end count key)
  "Search through `sequence', remove every element that matches
`item', and attach the remaining elements to `output-sequence'.

This is a generic version of Common Lisp's `remove'.  Note: unlike
`remove', this function does not support the from-end keyword
argument."
  (eager-sequence-remove-if
   (finder-predicate item test test-not)
   sequence output-sequence :start start :end end :key key :count count))

(defun sequence-count-if (predicate sequence &key (start 0) end key)
  "Count the number of items in `sequence' that satisfy `predicate'.

This is a generic version of Common Lisp's `count-if'.  Note: unlike
`count-if' this function does not support the from-end keyword argument."
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))

  (when end
    (cond
      ((< end start)
       (error "end must not be less than start"))
      ((equal end start)
       (return-from sequence-count-if
         0))))

  (multiple-value-bind
        (new-sequence uncompleted-steps)
      (sequence-nth-tail sequence start)
    (unless (zerop uncompleted-steps)
      (return-from sequence-count-if
        0))
    (setf sequence new-sequence))

  (when end
    (decf end start)
    (assert (plusp end) (end) "This should be impossible"))

  (let ((count 0))
    (do-while-popf (item sequence)
      (when (funcall predicate (if key (funcall key item) item))
        (incf count))
      (when end
        (decf end)
        (when (not (plusp end))
          (return-from sequence-count-if
            count))))
    count))

(defun sequence-count-if-not (predicate sequence &rest args &key (start 0) end key)
  "Count the number of items in `sequence' that fail to satisfy
`predicate'.

This is a generic version of Common Lisp's `count-if-not'.  Note:
unlike `count-if-not' this function does not support the from-end keyword
argument."
  (declare (ignore start end key))
  (apply 'sequence-count-if
         (lambda (item) (not (funcall predicate item)))
         sequence
         args))

(defun sequence-count (item sequence &key (start 0) end key test test-not)
  "Count the number of items in `sequence' that match `item'.

This is a generic version of Common Lisp's `count'.  Note:
unlike `count' this function does not support the from-end keyword
argument."
  (sequence-count-if
   (finder-predicate item test test-not) sequence
   :start start :end end :key key))

(defun lazy-map (walkable fn)
  "Create a lazy sequence that consists of `fn' applied to each
element in `walkable'.

`fn' is called at most once per element in `walkable'."
  (lazy-sequence
    (declare (wrap-with cache-impure))
    (multiple-value-bind (value valid-p) (head walkable)
      (if valid-p
          (immutable-cons (funcall fn value) (lazy-map (tail walkable) fn))
          (empty-immutable-list)))))

(defun eager-map (walkable fn output-sequence)
  "Create an eagerly-evaluated sequence consisting of `fn' applied to
each element in `walkable'.

The result of the map operation is attached to `output-sequence'.

If `fn' is the identity function then this is semantically equivalent
to `pour-from'."
  (do-while-popf (value walkable)
    (attachf output-sequence (funcall fn value)))
  output-sequence)

(defun lazy-filter (walkable fn)
  "Create a lazy sequence that consists of the elements of `walkable'
for which `fn' returns non-nil.

`fn' is called at most once per element in `walkable'."
  (lazy-sequence
    (declare (wrap-with cache-impure))
    (multiple-value-bind (value valid-p) (head walkable)
      (cond
        ((and valid-p (funcall fn value))
         (immutable-cons value (lazy-filter (tail walkable) fn)))
        (valid-p
         (lazy-filter (tail walkable) fn))
        (t
         (empty-immutable-list))))))

(defun eager-filter (walkable fn output-sequence)
  "Create an eagerly-evaluated sequence consisting of the elements of
`walkable' for which `fn' returns non-nil.

The result of the filter operation is attached to `output-sequence'.

If `fn' always returns non-nil then this is semantically equivalent to
`pour-from'."
  (do-while-popf (value walkable)
    (when (funcall fn value)
      (attachf output-sequence value)))
  output-sequence)

(defun pour-from (source sink)
  "Walk through `source' and attach all the values encountered onto
`sink' and returns the result.

Note: since sequences have inconsistent behaviors regarding where
`attach' operates, this is NOT a generic way to convert one sequence
type to another type.  For example, converting an `fset:seq' to an
`immutable-list' will reverse the order of the elements!"
  (do-while-popf (value source)
    (attachf sink value))
  sink)

(defstruct concatenated
  sequences)

(defmethod walk ((concatenated concatenated))
  concatenated)

(defmethod head ((concatenated concatenated))
  (let ((sequences (concatenated-sequences concatenated)))
    (do-while-popf (sequence sequences)
      (multiple-value-bind (value valid-p) (head sequence)
        (when valid-p
          (return-from head (values value valid-p))))))
  (values nil nil))

(defvar *empty-concatenated-sequence*
  (make-concatenated))

(defmethod tail ((concatenated concatenated))
  (let ((sequences (concatenated-sequences concatenated))
        popped-p)
    (loop
      (multiple-value-bind (sequence valid-p) (popf sequences)
        (unless valid-p
          (return-from tail *empty-concatenated-sequence*))
        (unless popped-p
          (multiple-value-bind (value valid-p) (popf sequence)
            (declare (ignore value))
            (when valid-p
              (setf popped-p t))))
        (unless (empty-p sequence)
          (setf sequences (immutable-cons sequence sequences))
          (return-from tail (make-concatenated :sequences sequences))))))
  *empty-concatenated-sequence*)

(defun concatenate-sequences (&rest sequences)
  "Return a walkable that contains all the elements in the provided
sequences.

This function is just a trivial wrapper around `flatten-sequence'."
  (flatten-sequence sequences))

(define-method-combination concatenate-sequences
  :identity-with-one-argument t
  :documentation
  "A method combination that combines method results using the
`concatenate-sequences' function.

This is like a generic version of the `nconc' method combination.
Instead of returning lists that are then destructively modified, you
may return any walkable sequence..")

(defun flatten-sequence (sequence-of-sequences)
  "Return a walkable that traverses the sequences contained within
`sequence-of-sequences'."
  (make-concatenated :sequences sequence-of-sequences))

(defun eager-flatmap-sequence (walkable fn output-sequence)
  "Apply `fn' to each element in `walkable' and then `attachf' each
element in the returned sequence to `output-sequence'.

After attaching the values to `output-sequence', this function returns
the resulting value of `output-sequence'.

This function is semantically equivalent to the following.
    (pour-from (flatten-sequence (eager-map walkable fn nil)) output-sequence)
However, unlike the above snippet this function will avoid creating
the intermediate sequences returned by `eager-map' and
`flatten-sequence'."
  (do-while-popf (value walkable)
    (let ((inner-sequence (funcall fn value)))
      (do-while-popf (inner-value inner-sequence)
        (attachf output-sequence inner-value))))
  output-sequence)

(defun walkable-to-list (walkable)
  "Convert the given walkable sequence into a list.

Let's face it.  Some things just want lists.  This gives you a proper
list."
  (when (typep walkable 'list)
    (return-from walkable-to-list walkable))
  (let (head
        tail)
    (do-while-popf (value walkable)
      (cond
        (tail
         (setf (cdr tail) (cons value nil))
         (setf tail (cdr tail)))
        (t
         (setf head (cons value nil))
         (setf tail head))))
    head))

(defun sequence-starts-with-p (sequence prefix &key (test 'eql))
  "Returns non-nil if the first elements of `sequence' match the
first elements of `prefix'.

`sequence' and `prefix' may be any types that can be walked using
`head' and `tail'.  When non-equal elements are found (as determined
by the function provided in the `test' parameter), this function
returns nil.  If `sequence' has fewer elements than `prefix', this
function returns nil."
  (loop
    (multiple-value-bind (seq-value seq-valid) (popf sequence)
      (multiple-value-bind (prefix-value prefix-valid) (popf prefix)
        (cond
          ((not prefix-valid)
           (return-from sequence-starts-with-p t))
          ((not seq-valid)
           (return-from sequence-starts-with-p nil))
          ((not (funcall test seq-value prefix-value))
           (return-from sequence-starts-with-p nil)))))))
