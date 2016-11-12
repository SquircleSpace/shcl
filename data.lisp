(defpackage :shcl/data
  (:use :common-lisp :shcl/utility)
  (:import-from :closer-mop)
  (:import-from :fset)
  (:export #:define-data))
(in-package :shcl/data)

(optimization-settings)

(defmacro define-cloning-accessor (name &key slot-name accessor)
  (unless (and (or slot-name accessor) (not (and slot-name accessor)))
    (error "define-cloning-accessor requires either a slot-name xor an accessor function"))
  (let* ((object (gensym "OBJECT"))
         (env (gensym "ENV"))
         (vars (gensym "VARS"))
         (vals (gensym "VALS"))
         (set-vars (gensym "SET-VARS"))
         (setter (gensym "SETTER"))
         (getter (gensym "GETTER"))
         (inner-clone (gensym "INNER-CLONE"))
         (set-var (gensym "SET-VAR"))
         ;; AAAAH nested backquotes are hard!  I couldn't figure out
         ;; how to get the appropriate accessor form into the setf
         ;; expander forms.  This is a bit hacky, but it works, damn
         ;; it!
         (access-macro (gensym "ACCESS-MACRO"))
         (access-wrapper
          (if slot-name
              (list `(defmacro ,access-macro (,object)
                       `(slot-value ,,object ,'',slot-name)))
              (list `(defmacro ,access-macro (,object)
                       `(,',accessor ,,object))))))
    `(progn
       ,@access-wrapper
       (defun ,name (,object)
         (,access-macro ,object))
       (define-setf-expander ,name (,object &environment ,env)
         (multiple-value-bind (,vars ,vals ,set-vars ,setter ,getter) (get-setf-expansion ,object ,env)
           (let ((,inner-clone (gensym "INNER-CLONE"))
                 (,set-var (gensym "SET-VAR")))
             (values
              `(,@,vars ,,inner-clone)
              `(,@,vals (clone ,,getter))
              `(,,set-var)
              `(progn
                 (setf (,',access-macro ,,inner-clone) ,,set-var)
                 (multiple-value-bind ,,set-vars ,,inner-clone
                   ,,setter)
                 ,,set-var)
              `(,',access-macro ,,inner-clone))))))))

(defmacro clone-slots (slots object maker)
  (let ((new (gensym "NEW"))
        (old (gensym "OLD"))
        (slot (gensym "SLOT")))
    `(let ((,new ,maker)
           (,old ,object))
       (dolist (,slot ',slots)
         (setf (slot-value ,new ,slot) (slot-value ,old ,slot)))
       ,new)))

(defgeneric clone (object))
(defmethod clone (object)
  object)

(defclass data-class (standard-class)
  ())

(defmethod closer-mop:validate-superclass ((data-class data-class) superclass)
  (if (eq superclass (find-class 'standard-object))
      t
      (call-next-method)))

(defclass direct-data-slot (closer-mop:standard-direct-slot-definition)
  ((updater
    :initarg :updater
    :initform nil)))

(defmethod closer-mop:finalize-inheritance :after ((class data-class))
  (let ((slot-names (mapcar 'closer-mop:slot-definition-name
                            (closer-mop:class-slots class))))
    (eval
     `(defmethod clone ((object ,(class-name class)))
          (clone-slots ,slot-names
           object
           (make-instance ',(class-name class)))))

    (eval
     `(defmethod fset:compare ((first ,(class-name class)) (second ,(class-name class)))
          (let ((result (fset:compare-slots
                         first second
                         ,@(mapcar (lambda (s) `',s) slot-names))))
            (cond
              ;; If they are :equal, then we need to make sure we
              ;; considered all the slots.
              ((not (eq :equal result)))

              ;; If they are the same class, then we considered all the
              ;; slots.  We don't have to worry about subclasses because
              ;; subclasses would have their own method on compare.
              ((eq (class-of first) (class-of second))
               (assert (eq (find-class ',(class-name class)) (class-of first))))

              ;; If second is a derived type, then first is less
              ((typep second (type-of first))
               (setf result :less))

              ;; If first is a derived type, then first is greater
              ((typep first (type-of second))
               (setf result :greater))

              ;; They are both derived types!  We've compared all their
              ;; common slots and didn't find an order, so they are
              ;; unequal.
              (t
               (setf result :unequal)))
            result)))

    (eval
     `(defmethod make-load-form ((object ,(class-name class)) &optional environment)
          (make-load-form-saving-slots object :slot-names ',slot-names :environment environment)))))

(defmethod initialize-instance :around ((slot direct-data-slot) &rest initargs)
  (declare (ignore initargs))
  (let ((result (call-next-method))
        (updater (slot-value slot 'updater)))
    (unless (listp updater)
      (setf updater (list updater)))
    (dolist (fn-name updater)
      (unless (typep fn-name 'symbol)
        (error "updater must have a symbol as the argument"))
      (let ((updater (slot-value slot 'updater)))
        (unless (listp updater)
          (setf updater (list updater)))
        (dolist (fn-name updater)
          (assert (typep fn-name 'symbol))
          (eval `(define-cloning-accessor ,fn-name :slot-name ,(closer-mop:slot-definition-name slot))))))
    result))

(defmethod closer-mop:direct-slot-definition-class ((data-class data-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-data-slot))

(defmacro define-data (name direct-superclasses direct-slots &rest options)
  (when (find :metaclass options :key #'car)
    (error "metaclass option is forbidden"))
  ;; We need to eval the class definition now so that the updater
  ;; functions are generated.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses ,direct-slots
       (:metaclass data-class)
       ,@options)))
