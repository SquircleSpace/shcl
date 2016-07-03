(in-package :shcl.utility)

(defmacro optimization-settings ()
  `(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0))))

(optimization-settings)

(defmacro define-once-global (name &body initform)
  (let ((value (gensym "VALUE"))
        (set (gensym "SET")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let (,value ,set)
         (defun ,name ()
           (unless ,set
             (setf ,value (progn ,@initform)
                   ,set t))
           ,value)
         (define-symbol-macro ,name (,name))))))

(define-condition required-argument-missing (error)
  ())

(defmacro required ()
  `(error 'required-argument-missing))

(defmacro try (exceptionable-form &body clauses)
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

(defclass iterator ()
  ((compute
    :initarg :compute)))

(defmacro make-iterator ((&key type) &body body)
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
           (make-instance (or ,type-sym 'iterator) :compute #',compute)
           ;; Now would be a good time to set the funcall function for
           ;; this iterator
           )))))

(defun iterate-iterator (iter)
  (with-slots (compute) iter
    (when (not (slot-boundp iter 'compute))
      (return-from iterate-iterator (values nil nil)))

    (multiple-value-bind (value more) (funcall compute)
      (unless more
        (slot-makunbound iter 'compute))
      (values value more))))

(defgeneric iterate-function (iterator))
(defmethod iterate-function ((iter iterator))
  #'iterate-iterator)

(defun next (iter)
  (funcall (iterate-function iter) iter))

(defmacro do-iterator ((value-sym iter &key result) &body body)
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

(defun map-iterator (iter function &key type)
  (make-iterator (:type type)
    (multiple-value-bind (value more) (next iter)
      (unless more
        (stop))
      (emit (funcall function value)))))

(defun iterator-values (iter)
  (let ((vector (make-array 0 :adjustable t :fill-pointer t)))
    (do-iterator (value iter :result vector)
      (vector-push-extend value vector))))

(defclass lookahead-iterator ()
  ((compute
    :initform (cons t nil)
    :initarg :compute)
   (buffer
    :initform (cons 'tail 'tail)
    :initarg :buffer
    :type cons)))
(defmethod initialize-instance :after ((iter lookahead-iterator) &key)
  (with-slots (compute buffer) iter
    (unless (consp compute)
      (setf compute (cons t compute)))))

(defmethod update-instance-for-different-class :after ((old iterator) (new lookahead-iterator) &key)
  (declare (ignore old))
  (with-slots (compute) new
    (unless (consp compute)
      (setf compute (cons t compute))))
  ;; Now would be a good time to set the funcall function
  )

(defun make-iterator-lookahead (iterator)
  (check-type iterator iterator)
  (change-class iterator (find-class 'lookahead-iterator)))

(defun fork-lookahead-iterator (iter)
  (check-type iter lookahead-iterator)
  (with-slots (compute buffer) iter
    (make-instance (class-of iter) :compute compute :buffer buffer)))

(defun iterate-lookahead-iterator (iter)
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
  (iterate-lookahead-iterator (fork-lookahead-iterator iter)))

(defun move-lookahead-to (iter-to-change model-iter)
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

(defun vector-iterator (vector)
  (let ((index 0))
    (make-iterator ()
      (when (>= index (length vector))
        (stop))
      (let ((value (aref vector index)))
        (incf index)
        (emit value)))))

(defun list-iterator (list)
  (let ((cons list))
    (make-iterator ()
      (when (eq nil cons)
        (stop))
      (let ((value (car cons)))
        (setf cons (cdr cons))
        (emit value)))))
