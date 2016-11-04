(defpackage :shcl/utility
  (:use :common-lisp :alexandria :bordeaux-threads)
  (:import-from :fset)
  (:import-from :closer-mop)
  (:shadow #:when-let #:when-let*)
  (:export
   #:eval-once-when #:define-once-global #:required #:required-argument-missing
   #:optimization-settings #:when-let #:when-let* #:try #:debug-log
   #:logging-enabled-p #:status #:make-extensible-vector
   ;; Hooks
   #:define-hook #:add-hook #:remove-hook #:run-hook #:on-revival
   #:observe-revival #:on-dump #:observe-dump
   ;; Iterators
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator #:seq-iterator
   #:do-iterator #:peek-lookahead-iterator #:move-lookahead-to #:map-iterator
   #:iterator-values #:lookahead-iterator-wrapper))
(in-package :shcl/utility)

(defmacro optimization-settings ()
  "Declaims standard optimization settings.

Put this at the top of every file!"
  `(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0))))

(optimization-settings)

(defmacro eval-once-when ((&rest times) &body body)
  (let ((once-token (gensym "ONCE-TOKEN")))
    `(eval-when (,@times)
       (defvar ,once-token)
       (unless (boundp ',once-token)
         (setf ,once-token (progn ,@body)))
       ,once-token)))

(defmacro define-once-global (name initform &body options)
  "Define a global variable.

The variable is initialized the first time it is accessed and is
initialized at most once.  Redefining the variable with
`define-once-global' will reset the variable to be uninitialized."
  (check-type name symbol)
  (let* ((value (gensym "VALUE"))
         (setter-value (gensym "SETTER-VALUE"))
         (lock (gensym "LOCK"))
         (documentation (second (find :documentation options :key 'car)))
         (no-lock (second (find :no-lock options :key 'car)))
         (read-only (second (find :read-only options :key 'car)))
         (lock-form (if no-lock '(progn) `(with-lock-held (,lock)))))

    (check-type documentation (or null string))
    (when documentation
      (setf documentation (list documentation)))
    (unless (member no-lock '(nil t))
      (error ":no-lock option must have value nil or t"))
    (unless (member read-only '(nil t))
      (error ":read-only option must have value nil or t"))
    `(progn
       ,@(unless no-lock `((defvar ,lock (bordeaux-threads:make-lock ,(symbol-name name)))))
       (defvar ,value)
       (defun ,name ()
         ,@documentation
         (,@lock-form
          (unless (boundp ',value)
            (setf ,value ,initform))
          ,value))
       ,@(unless read-only
           `((defun (setf ,name) (,setter-value)
               (,@lock-form
                (setf ,value ,setter-value)
                ,setter-value))))
       (define-symbol-macro ,name (,name)))))

(defparameter *debug-stream* *error-output*
  "The stream where log lines are sent.")
(defparameter *debug-stream-lock* (make-lock)
  "The lock that protects `*debug-stream*'.")

(defparameter *log-levels*
  (alist-hash-table
   '((error . t)
     (warning . t)
     (status . t)))
  "The hash table that dictates which log levels are enabled and which
are not.")

(defun logging-enabled-p (level)
  "Returns t iff the given log level is enabled."
  (gethash level *log-levels*))

(defmacro debug-log (level message &rest format-args)
  "Emit a log line."
  (let ((level-val (gensym "LEVEL-VAL")))
    `(with-lock-held (*debug-stream-lock*)
       (let ((,level-val ,level))
         (when (logging-enabled-p ,level-val)
           (format *debug-stream* ,message ,@format-args)
           (fresh-line *debug-stream*))))))

(defstruct hook
  (functions (fset:empty-set)))

(defmacro define-hook (name &optional documentation)
  "Create a hook.

A hook is more or less an unordered collection of functions.  When the
hook is run with `run-hook', each function will be called once."
  `(defparameter ,name
     (make-hook)
     ,documentation))

(defun add-hook (hook function-symbol)
  "Add a function a function to a hook."
  (check-type hook hook)
  (check-type function-symbol symbol)
  (setf (hook-functions hook) (fset:with (hook-functions hook) function-symbol))
  hook)

(defun remove-hook (hook function-symbol)
  "Remove a function from a hook."
  (check-type hook hook)
  (check-type function-symbol symbol)
  (setf (hook-functions hook) (fset:less (hook-functions hook) function-symbol))
  hook)

(defun run-hook (hook)
  "Run each function in the provided hook."
  (fset:do-set (fn (hook-functions hook))
    (funcall fn)))

(define-hook *revival-hook*
  "This hook is run when the process starts.")

(defmacro on-revival (function-symbol)
  "When the process starts, call the named function."
  `(add-hook *revival-hook* ',function-symbol))

(defun observe-revival ()
  "The process has started!"
  (run-hook *revival-hook*))

(define-hook *dump-hook*
  "This hook is run when an executable is being prepared.

Note, it is as of yet undetermined whether this hook will run or not
for lisp compilers like ECL.")

(defmacro on-dump (function-symbol)
  "When saving an executable, call the named function."
  `(add-hook *dump-hook* ',function-symbol))

(defun observe-dump ()
  "We're saving an executable!"
  (run-hook *dump-hook*))

(defun %when-let (let-sym bindings body)
  (let ((block (gensym (format nil "WHEN-~A-BLOCK" (symbol-name let-sym)))))
    (labels
        ((transform (binding)
           (when (symbolp binding)
             (setf binding (list binding nil)))
           (destructuring-bind (variable &optional value) binding
             (let ((value-sym (gensym "VALUE")))
               (setf value `(let ((,value-sym ,value))
                              (if ,value-sym
                                  ,value-sym
                                  (return-from ,block))))
               (list variable value)))))
      `(block ,block
         (,let-sym ,(mapcar #'transform bindings)
           ,@body)))))

(defmacro when-let* (bindings &body body)
  "Establish bindings (like `let*' would).  If any variable is bound to
nil, then the whole `when-let*' short circuts and evaluates to nil."
  (%when-let 'let* bindings body))

(defmacro when-let (bindings &body body)
  "Establish bindings (like `let' would).  If any variable is bound to
nil, then the whole `when-let' short circuts and evaulates to nil."
  (%when-let 'let bindings body))

(define-condition required-argument-missing (error)
  ()
  (:documentation
   "A condition for indicating that a required argument was not provided."))

(defmacro required ()
  "This form always signals an error of type `required-argument-missing'."
  `(error 'required-argument-missing))

(defmacro try (exceptionable-form &body clauses)
  "A better version of `catch'.

When you use `catch', you can't distinguish between normal execution and a thrown value.  Sometimes that is desirable.  Other times, you might like to know the difference.  With `signal' and conditions, you have that flexibility.  However, the condition system is fairly heavyweight and thus isn't appropriate for all use cases.  The `try' macro attempts to bring some of the flexibility of `handler-case' to `catch' and tries to emulate the control-flow of a more traditional exception system.

Example:
(try
    (throw 'foobar (values 1 2 3))
  (foobar (x y z) (+ x y z))
  (baz (a) (frobnosticate a)))

It is unspecified what happens if one of the handler clauses throws a
tag named in a different clause."
  ;; We're going to do a slightly insane thing.  We're going to build
  ;; up a series of nested forms that look like this.
  ;; (multiple-value-call #'foo-handler
  ;;   (catch 'foo
  ;;     (return-from no-problem
  ;;       (multiple-value-call #'bar-handler
  ;;         (catch 'bar
  ;;           (return-from no-problem <...>))))))
  ;; If nothing is thrown, then the return-from form will execute.  If
  ;; something is thrown, then we bypass the return-from and call the
  ;; handler instead.  When the handler returns, its wrapping
  ;; return-from form will cause us to exit the try form altogether.
  (let ((no-problem (gensym "NO-PROBLEM"))
        labels-forms
        tag-label-alist)
    (dolist (clause clauses)
      (destructuring-bind (tag lambda-list &body body) clause
        (let ((label (gensym (symbol-name tag))))
          (push (cons tag label) tag-label-alist)
          (push `(,label ,lambda-list ,@body) labels-forms))))
    (setf labels-forms (nreverse labels-forms)
          tag-label-alist (nreverse tag-label-alist))

    (labels
        ((catch-form (tag-alist)
           (if (null tag-alist)
               exceptionable-form
               (let* ((head (car tag-alist))
                      (rest (cdr tag-alist))
                      (tag (car head))
                      (label (cdr head)))
                   `(multiple-value-call #',label
                      (catch ',tag
                        (return-from ,no-problem ,(catch-form rest))))))))
      `(block ,no-problem
         (labels (,@labels-forms)
           ,(catch-form tag-label-alist))))))

(defun make-extensible-vector
    (&key
       (initial-size 0)
       (initial-element nil initial-element-p)
       (initial-contents nil initial-contents-p)
       (element-type t)
       (fill-pointer t))
  "This function provides a quick way to make a single-dimensional,
adjustable array with a fill pointer."
  (cond
    ((and initial-element-p initial-contents-p)
     (error "Can't specify both initial-element and initial-contents"))

    ((not fill-pointer)
     (error "fill-pointer cannot be nil"))

    (initial-element-p
     (make-array initial-size :adjustable t :fill-pointer fill-pointer :initial-element initial-element :element-type element-type))

    (initial-contents-p
     (make-array initial-size :adjustable t :fill-pointer fill-pointer :initial-contents initial-contents :element-type element-type))

    (t
     (make-array initial-size :adjustable t :fill-pointer fill-pointer :element-type element-type))))

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
