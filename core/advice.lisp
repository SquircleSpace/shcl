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

(defpackage :shcl/core/advice
  (:use :common-lisp :shcl/core/utility)
  (:import-from :closer-mop)
  #+ccl
  (:import-from :alexandria)
  (:export
   #:define-advisable #:define-advice #:remove-advice #:list-advice))
(in-package :shcl/core/advice)

(optimization-settings)

(defclass advisable-generic-function (closer-mop:standard-generic-function)
  (#+ccl
   (advice-method-table
    :initform (make-hash-table :test 'equal)
    :accessor advice-method-table))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "A generic function representing an advisable function.  See
`define-advisable'."))

;; Clozure doesn't do method functions how the MOP says they should.
;; We're going to get hacky with it.
#+ccl
(progn
  (defun advice-methods (advisable)
    (alexandria:hash-table-values (advice-method-table advisable)))

  (defclass compatability-advice-method ()
    ((function
      :initarg :function
      :reader closer-mop:method-function)
     (qualifiers
      :initarg :qualifiers
      :reader method-qualifiers)))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun make-advice-lambda (lambda-expression)
      (let ((args (gensym "ARGS"))
            (next-methods (gensym "NEXT-METHODS"))
            (replacement-args (gensym "REPLACEMENT-ARGS")))
        `(lambda (,args ,next-methods)
           (labels
               ((call-next-method (&rest ,replacement-args)
                  (unless ,next-methods
                    (error "No next method to call"))
                  (funcall (car ,next-methods) (or ,replacement-args ,args) (cdr ,next-methods)))
                (next-method-p ()
                  (not (not ,next-methods))))
             (apply ,lambda-expression ,args))))))

  (defmacro %define-advice (function-name qualifiers lambda-list &body body)
    (let* ((args (gensym "ARGS"))
           (method-lambda (make-advice-lambda
                           `(flet ((,function-name ,lambda-list
                                     ,@body))
                              #',function-name))))
      `(progn
         (add-method #',function-name
                     (make-instance 'compatability-advice-method
                                    :function ,method-lambda
                                    :qualifiers '(,@qualifiers)))
         ',function-name)))

  (defmethod add-method ((gf advisable-generic-function) (method compatability-advice-method))
    (setf (gethash (method-qualifiers method) (advice-method-table gf)) method)
    (values))
  (defmethod remove-method ((gf advisable-generic-function) (method compatability-advice-method))
    (remhash (method-qualifiers method) (advice-method-table gf))
    (values)))

#-ccl
(progn
  (defmacro %define-advice (function-name qualifiers lambda-list &body body)
    `(defmethod ,function-name ,@qualifiers ,lambda-list ,@body))

  (defun advice-methods (advisable-generic-function)
    (closer-mop:generic-function-methods advisable-generic-function)))

(defclass advice-method (closer-mop:standard-method)
  ()
  (:documentation
   "A method representing advice on an advisable function.  See
`define-advice'."))

(defmethod initialize-instance :before ((method advice-method) &rest initargs &key specializers qualifiers)
  (declare (ignore initargs))
  ;; No specialization allowed!
  (dolist (specializer specializers)
    (unless (eq specializer (find-class t))
      (error "Cannot have specializers on an `advice-method',")))
  (unless (or (null qualifiers)
              (find (first qualifiers) #(:before :after :around)))
    (error "advice-method does not support the ~A qualifier" (first qualifiers))))

;; The advisable method combination strategy only exists to avoid
;; warnings about method qualifiers that aren't valid under the
;; standard strategy.  All the work of creating a combined method is
;; done (non-traditionally) in the compute-discriminating-function
;; method.
(define-method-combination advisable () ((all *)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun assert-pure-lambda-list (lambda-list)
    "Signals an error if the provided lambda list appears to have
method specializers in it."
    (dolist (arg lambda-list)
      (when (member arg '(&optional &rest &key &aux &allow-other-keys))
        (return-from assert-pure-lambda-list (values)))
      (when (consp arg)
        (error "Malformed lambda list.  Expected required parameter name, but found a list:~%~A" arg)))
    (values)))

(defmacro define-advisable (&whole whole name lambda-list &body body)
  "Define an advisable function.

An advisable function is a function that can be augmented with
`define-advice'.  If no advice is provided for an advisable function,
then `define-advisable' is more or less equivalent to `defun'."
  (assert-pure-lambda-list lambda-list)
  (multiple-value-bind (forms declarations documentation)
      (alexandria:parse-body body :documentation t :whole whole)
    (declare (ignore forms declarations))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defgeneric ,name ,lambda-list
           (:generic-function-class advisable-generic-function)
           (:method-class advice-method)
           (:method-combination advisable)
           ,@(when documentation
               `((:documentation ,documentation)))))
       (%define-advice ,name nil ,lambda-list
         ,@body))))

(defmacro define-advice (function-name qualifier advice-name lambda-list &body body)
  "Augment an advisable function with a piece of advice.

`qualifier' is one of `:before', `:after', or `:around'.
`advice-name' is a symbol that (combined with `qualifier') uniquely
names this advice.

Advice qualifiers retain the same meanings they have in the standard
method combination strategy.  Unlike normal methods, advice cannot be
specialized on the classes of the method's arguments.  However, you
can have multiple pieces of advice with the same qualifier as long as
the `advice-name' is different.  If two pieces of advice have the same
qualifier, then it is unspecified which piece of advice will run
first.

Advice can be removed with the `remove-advice' function."
  (assert-pure-lambda-list lambda-list)
  (check-type advice-name symbol)
  (unless (member qualifier '(:before :after :around))
    (error "Qualifier must be one of :before, :after, or :around."))
  `(%define-advice ,function-name (,qualifier ,advice-name) ,lambda-list ,@body))

(defun remove-advice (function qualifier advice-name)
  "Remove advice from an advisable function.

The function must have been defined with `define-advisable'.  The
`qualifier' and `advice-name' must describe a piece of advice defined
with `define-advice'."
  (when (symbolp function)
    (setf function (symbol-function function)))
  (check-type function advisable-generic-function)

  (let ((methods (advice-methods function))
        (our-qualifiers (list qualifier advice-name)))
    (dolist (method methods)
      (let ((method-qualifiers (method-qualifiers method)))
        (when (equal method-qualifiers our-qualifiers)
          (remove-method function method)
          (return-from remove-advice (values)))))
    (error "Unable to find matching advice.")))

(defun list-advice (function &optional qualifier)
  (when (symbolp function)
    (setf function (symbol-function function)))
  (check-type function advisable-generic-function)

  (let (result)
    (dolist (method (advice-methods function))
      (let ((qualifiers (method-qualifiers method)))
        (when (and qualifiers (or (null qualifier)
                                  (eq qualifier (first qualifiers))))
          (push qualifiers result))))
    result))

(defmethod closer-mop:compute-discriminating-function ((gf advisable-generic-function))
  (let* ((before-functions (make-array 0 :adjustable t :fill-pointer t))
         (after-functions (make-array 0 :adjustable t :fill-pointer t))
         (around-functions (make-array 0 :adjustable t :fill-pointer t))
         primary-function)

    (dolist (method (advice-methods gf))
      (let ((qualifiers (method-qualifiers method)))
        (cond
          ((null qualifiers)
           (assert (not primary-function))
           (setf primary-function (closer-mop:method-function method)))
          ((eq :before (first qualifiers))
           (vector-push-extend (closer-mop:method-function method) before-functions))
          ((eq :after (first qualifiers))
           (vector-push-extend (closer-mop:method-function method) after-functions))
          ((eq :around (first qualifiers))
           (vector-push-extend (closer-mop:method-function method) around-functions))
          (t
           (assert nil nil "Unrecognized qualifier")))))

    (unless primary-function
      (return-from closer-mop:compute-discriminating-function
        (lambda (&rest args)
          (declare (ignore args))
          (error "No primary method found"))))

    (labels
        ((main-impl (&rest args)
           (loop :for method :across before-functions :do
              (funcall method args nil))
           (multiple-value-prog1
               (funcall primary-function args nil)
             (loop :for method :across after-functions :do
                (funcall method args nil)))))

      (when (zerop (length around-functions))
        (return-from closer-mop:compute-discriminating-function
          #'main-impl))

      (let (around-functions-list)
        (labels
            ((main-method-function (args other-methods)
               (declare (ignore other-methods))
               (apply #'main-impl args))
             (around-impl (&rest args)
               (funcall (car around-functions-list) args (cdr around-functions-list))))
          (vector-push-extend #'main-method-function around-functions)
          (setf around-functions-list (coerce around-functions 'list))
          #'around-impl)))))
