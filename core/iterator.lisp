(defpackage :shcl/core/iterator
  (:use :common-lisp)
  (:import-from :shcl/core/utility #:make-extensible-vector)
  (:export
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator #:seq-iterator
   #:do-iterator #:peek-lookahead-iterator #:move-lookahead-to #:map-iterator
   #:filter-iterator #:iterator-values #:lookahead-iterator-wrapper
   #:lookahead-iterator-position-token))
(in-package :shcl/core/iterator)

(defclass iterator ()
  ((compute
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
        (iter-fun (gensym "ITER-FUN")))
    `(let* ((,iter-sym ,iter)
            (,iter-fun (iterate-function ,iter-sym)))
       (loop
          (multiple-value-bind (,value-sym ,more-sym) (funcall ,iter-fun ,iter-sym)
            (unless ,more-sym
              (return ,result))
            ,@body)))))

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

(defun iterator-values (iter)
  "Extract all remaining values from the given iterator and return
them in an array."
  (let ((vector (make-extensible-vector)))
    (do-iterator (value iter :result vector)
      (vector-push-extend value vector))))

(defclass lookahead-iterator ()
  ((compute
    :initform (cons t nil)
    :initarg :compute)
   (buffer
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

(defmethod initialize-instance :after ((iter lookahead-iterator) &key)
  (with-slots (compute buffer) iter
    (unless (consp compute)
      (setf compute (cons t compute)))
    (let ((fn (iterate-function iter)))
      (closer-mop:set-funcallable-instance-function
       iter
       (lambda ()
         (funcall fn iter))))))

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
    (when (and (eq (cdr buffer) 'tail)
               (not (cdr compute)))
      (return-from iterate-lookahead-iterator (values nil nil)))

    (cond ((eq (cdr buffer) 'tail)
           ;; Buffer is empty.  Add something to the back
           (multiple-value-bind (value more) (funcall (cdr compute))
             (cond (more
                    (let ((new-tail (cons 'tail 'tail)))
                      (setf (car buffer) value
                            (cdr buffer) new-tail
                            buffer (cdr buffer))))

                   (t
                    (setf (cdr compute) nil)))
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

(defgeneric iterator (thing)
  (:documentation
   "Produce an iterator that traverses the given thing."))

(defmethod iterator ((list list))
  (list-iterator list))

(defmethod iterator ((vector vector))
  (vector-iterator vector))

(defmethod iterator ((seq fset:seq))
  (seq-iterator seq))
