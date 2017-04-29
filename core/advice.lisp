(defpackage :shcl/core/advice
  (:use :common-lisp :shcl/core/utility)
  (:import-from :closer-mop)
  (:export #:define-advisable #:define-advice #:remove-advice #:list-advice))
(in-package :shcl/core/advice)

(optimization-settings)

(defclass advisable-generic-function (closer-mop:standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "A generic function representing an advisable function.  See
`define-advisable'."))

(defclass advice-method (closer-mop:standard-method)
  ()
  (:documentation
   "A method representing advice on an advisable function.  See
`define-advice'."))

(defmethod initialize-instance :before ((method advice-method) &rest initargs &key specializers)
  (declare (ignore initargs))
  ;; No specialization allowed!
  (dolist (specializer specializers)
    (unless (eq specializer (find-class t))
      (error "Cannot have specializers on an `advice-method',"))))

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
       (defgeneric ,name ,lambda-list
         (:generic-function-class advisable-generic-function)
         (:method-class advice-method)
         ,@(when documentation
             `((:documentation ,documentation))))
       (defmethod ,name ,lambda-list
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
  `(defmethod ,function-name ,qualifier ,advice-name ,lambda-list ,@body))

(defun remove-advice (function qualifier advice-name)
  "Remove advice from an advisable function.

The function must have been defined with `define-advisable'.  The
`qualifier' and `advice-name' must describe a piece of advice defined
with `define-advice'."
  (when (symbolp function)
    (setf function (symbol-function function)))
  (check-type function advisable-generic-function)

  (let ((methods (closer-mop:generic-function-methods function))
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
    (dolist (method (closer-mop:generic-function-methods function))
      (let ((qualifiers (method-qualifiers method)))
        (when (and qualifiers (or (null qualifier)
                                  (eq qualifier (first qualifiers))))
          (push qualifiers result))))
    result))

(defmethod closer-mop:compute-effective-method ((gf advisable-generic-function) combin methods)
  (let (primary-methods
        before-methods
        after-methods
        around-methods)
    (dolist (method methods)
      (tagbody
         (let ((qualifiers (method-qualifiers method)))
           (unless qualifiers
             (push method primary-methods)
             (go next))

           (case (first qualifiers)
             (:before
              (push method before-methods)
              (go next))
             (:after
              (push method after-methods)
              (go next))
             (:around
              (push method around-methods)
              (go next)))
           (error "Unrecognized qualifier"))
       next))
    (unless primary-methods
      (error "Need a primary method"))
    (labels
        ((call-methods (methods)
           (loop :for method :in methods :collect
              `(call-method ,method))))
      (destructuring-bind (primary &rest primary-rest) primary-methods
        (let ((main-form
               `(multiple-value-prog1
                    (progn
                      ,@(call-methods before-methods)
                      (call-method ,primary ,primary-rest))
                  ,@(call-methods (reverse after-methods)))))
          (if around-methods
              `(call-method ,(first around-methods) (,@(rest around-methods) (make-method ,main-form)))
              main-form))))))
