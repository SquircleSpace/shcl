(defpackage :shcl/test/fd-table
  (:use :common-lisp :shcl/core/fd-table :prove :shcl/test/foundation)
  (:import-from :shcl/core/utility #:optimization-settings)
  (:import-from :fset))
(in-package :shcl/test/fd-table)

(optimization-settings)

(defvar *nop-fd-number* 0)

(defmacro post-incf (place)
  (let ((old-value (gensym "OLD-VALUE")))
    `(let ((,old-value ,place))
       (incf ,place)
       ,old-value)))

(defclass nop-fd ()
  ((value
    :reader fd-wrapper-value
    :initform (post-incf *nop-fd-number*)
    :initarg :value)))

(defmethod fset:compare ((left nop-fd) (right nop-fd))
  (fset:compare-slots left right #'fd-wrapper-value))

(defmethod print-object ((fd nop-fd) stream)
  (print-unreadable-object (fd stream :type t)
    (format stream "~A" (fd-wrapper-value fd))))

(defun make-nop-fd (&optional value)
  (if value
      (make-instance 'nop-fd :value value)
      (make-instance 'nop-fd)))

(defmethod fd-wrapper-retain ((nop-fd nop-fd))
  nop-fd)
(defmethod fd-wrapper-release ((nop-fd nop-fd)))

(defun check-linear-fd-bindings (original-fd-bindings)
  (let ((linear-bindings (linearize-fd-bindings original-fd-bindings))
        (unique-values (make-hash-table))
        (expected-table (fset:empty-map))
        (built-table (fset:empty-map)))
    (labels
        ((lookup-fd-unique-value (fd)
           (let ((value (gethash fd unique-values)))
             (unless value
               (setf value (gensym fd))
               (setf (gethash fd unique-values) value))
             value))
         (lookup-fd (fd)
           (or (fset:lookup built-table fd)
               (lookup-fd-unique-value fd))))
      (fset:do-map (virtual-fd value-fd original-fd-bindings)
        (setf (fset:lookup expected-table virtual-fd)
              (lookup-fd-unique-value (fd-wrapper-value value-fd))))

      (dolist (binding linear-bindings)
        (destructuring-bind (key . value) binding
          (cond
            ((null value)
             (setf built-table (fset:less built-table key)))

            (t
             (let ((new-value (lookup-fd value)))
               (setf (fset:lookup built-table key) new-value)))))))
    (values
     (eq :equal (fset:compare expected-table built-table))
     expected-table
     built-table)))

(defmacro with-private-fd-bindings (fd-bindings-place &body body)
  `(let ((shcl/core/fd-table::*fd-bindings* ,fd-bindings-place))
     (unwind-protect
          (progn
            ,@body)
       (setf ,fd-bindings-place shcl/core/fd-table::*fd-bindings*))))

(define-test linearize-fd-bindings
  (let ((fd-bindings (fset:empty-map))
        (*nop-fd-number* 0))
    (destructuring-bind (first second third)
        (loop :for index :below 3 :collect
           (make-nop-fd))
      (with-private-fd-bindings fd-bindings
        (set-fd-binding (fd-wrapper-value first)
                        second)
        (set-fd-binding (fd-wrapper-value second)
                        third)
        (set-fd-binding (fd-wrapper-value third)
                        first)))
    (ok (check-linear-fd-bindings fd-bindings)
        "A simple cycle is okay"))

  (let ((fd-bindings (fset:empty-map))
        (*nop-fd-number* 1))
    (with-private-fd-bindings fd-bindings
      (set-fd-binding 0 (make-nop-fd)))
    (ok (check-linear-fd-bindings fd-bindings)
        "Binding from a private fd is okay")))
