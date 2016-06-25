(in-package :shcl.yacc-parser)

(defun iterator-as-lexer (iterator)
  (lambda ()
    (multiple-value-bind (value more) (next iterator)
      (if more
          (values (class-name (class-of value)) value)
          (values nil nil)))))

(defun list-to-lexer (list)
  (iterator-as-lexer (list-iterator list)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun take-while (predicate list)
    (let (result)
      (loop :while (and list (funcall predicate (car list)))
         :do (progn
               (push (car list) result)
               (setf list (cdr list))))
      (values (nreverse result) list)))

  (defun maker (class-name values slot-names)
    (let ((instance (make-instance class-name)))
      (loop :for val :in values
         :for name :in slot-names :do
         (setf (slot-value instance name) val))
      (setf (slot-value instance 'raw-matches) values)
      instance))

  (defun inject-function-form (class-name production-forms)
    (let ((slot-names (make-hash-table))
          (new-forms))
      (labels ((handle-rule (rule)
                 (unless (consp rule)
                   (return-from handle-rule rule))

                 (let ((slots (copy-seq rule)))
                   (dolist (name slots)
                     (setf (gethash name slot-names) t))
                   `(,@rule (lambda (&rest values) (maker ',class-name values ',slots))))))
        (setf new-forms
              (loop :for rule :in production-forms :collect
                 (handle-rule rule)))
        (values new-forms (hash-table-keys slot-names)))))

  (defun slot-definition (name)
    `(,name
      :initarg ,(intern (symbol-name name) :keyword)
      :accessor ,name)))

(defclass syntax-tree ()
  ((raw-matches
    :initform nil
    :accessor raw-matches)))

(defmethod print-object ((st syntax-tree) stream)
  (format stream "~A" (cons (class-name (class-of st)) (slot-value st 'raw-matches))))

(defmacro define-syntax-tree (grammar-name &body body)
  (let (classes
        new-grammar)
    (labels ((option-form (form)
               (and (consp form)
                    (keywordp (car form)))))
      (multiple-value-bind (options grammar) (take-while #'option-form body)
        (dolist (nonterminal grammar)
          (destructuring-bind (name &rest production) nonterminal
            (multiple-value-bind (new-forms slot-names) (inject-function-form name production)
              (let ((class-form
                     `(defclass ,name (syntax-tree) (,@(mapcar #'slot-definition slot-names)))))
                (push class-form classes))
              (push `(,name ,@new-forms) new-grammar))))
        (setf new-grammar (nreverse new-grammar))
        `(progn
           ,@classes
           (define-parser ,grammar-name
             ,@options
             ,@new-grammar))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind ((warning #'muffle-warning))
    (load "grammar-definition.lisp")))

(defun parse-stream (stream)
  (parse-with-lexer (iterator-as-lexer (token-iterator stream)) *shell-parser*))
