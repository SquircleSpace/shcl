(in-package :shcl.yacc-parser)

(optimization-settings)

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
      :accessor ,name))

  (defun grammar-option-form-p (form)
    (and (consp form)
         (keywordp (car form))))

  (defun grammar-lookup-option (option-name options)
    (second (find option-name options :key #'car :test #'eq)))

  (defun left-recursion-p (nonterminals)
    (let ((graph (make-hash-table)))
      (dolist (nonterm nonterminals)
        (destructuring-bind (name &rest productions) nonterm
          (dolist (production productions)
            (push (if (consp production)
                      (car production)
                      production)
                  (gethash name graph)))))
      (labels ((examine (where &optional (stack (list where)))
                 (let ((children (gethash where graph)))
                   (dolist (child children)
                     (when (member child stack)
                       (return-from left-recursion-p (nreverse (cons child stack))))

                     (examine child (cons child stack))))))
        (dolist (sym (hash-table-keys graph))
          (examine sym))
        nil))))

(defclass syntax-tree ()
  ((raw-matches
    :initform nil
    :accessor raw-matches)))

(defmethod print-object ((st syntax-tree) stream)
  (format stream "~A" (cons (class-name (class-of st)) (slot-value st 'raw-matches))))

(defmacro define-syntax-tree (grammar-name &body body)
  (let (classes
        new-grammar)
    (multiple-value-bind (options grammar) (take-while #'grammar-option-form-p body)
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
         (define-grammar ,grammar-name
           ,@options
           ,@new-grammar)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind ((warning #'muffle-warning))
    (load "grammar-definition.lisp")))

(defun parse-stream (stream)
  (parse-with-lexer (iterator-as-lexer (token-iterator stream)) *shell-parser*))

(defun parse-string (string)
  (parse-stream (make-string-input-stream string)))

(defgeneric parse (source))
(defmethod parse ((stream stream))
  (parse-stream stream))

(defmethod parse ((string string))
  (parse-string string))

(defgeneric parse-recursive (type iterator))

(define-condition no-parse ()
  ((message
    :initarg :message
    :type string
    :initform "<No reason>"
    :accessor no-parse-message))
  (:report (lambda (c s) (format s "Parse error (~A)" (no-parse-message c)))))

(defmacro no-parse (message &rest expected-tokens)
  (declare (ignore expected-tokens))
  `(signal 'no-parse :message ,message))

(defmacro try-parse (((iter-sym iter-form) &key no-parse-form) &body body)
  (let ((iter (gensym "ITER"))
        (no-advance (gensym "NO-ADVANCE") ))
    `(let* ((,iter ,iter-form)
            (,iter-sym (fork-lookahead-iterator ,iter-form))
            ,no-advance)
       (unwind-protect
            (handler-case
                (progn ,@body)
              (no-parse ()
                (setf ,no-advance t)
                ,no-parse-form))
         (unless ,no-advance
           (move-lookahead-to ,iter ,iter-sym))))))

(define-condition abort-parse (error)
  ((message
    :initarg :message
    :type string
    :initform "<No reason>"
    :accessor parse-error-message))
  (:report (lambda (c s) (format s "Parse error (~A)" (parse-error-message c)))))

(defmacro abort-parse (message &rest expected-tokens)
  (declare (ignore expected-tokens))
  `(signal 'parse-error :message ,message))

(defmacro define-recursive-descent-parser (name &body body)
  (let (result-forms)
    (macrolet ((send (form) `(push ,form result-forms))
               (send-to (place form) `(push ,form ,place)))
      (multiple-value-bind (options nonterminals) (take-while #'grammar-option-form-p body)
        (let ((start-symbol (grammar-lookup-option :start-symbol options))
              (terminals (grammar-lookup-option :terminals options)))

          (unless (and start-symbol terminals)
            (error "Start symbol and terminals are required"))

          (when (left-recursion-p nonterminals)
            (error "Grammar has left recursion ~A" (left-recursion-p nonterminals)))

          ;; Easiest stuff first.  The root node
          (send `(defmethod parse-recursive ((type (eql ',name)) (iter lookahead-iterator))
                   (parse-recursive ',start-symbol iter)))

          ;; Now parsers for the terminals
          (dolist (term terminals)
            (send `(defmethod parse-recursive ((type (eql ',term)) (iter lookahead-iterator))
                     (unless (typep (peek-lookahead-iterator iter) ',term)
                       (no-parse "Token mismatch" ',term))
                     (next iter))))

          ;; Now the nonterminals
          (dolist (nonterm nonterminals)
            (destructuring-bind (nonterm-name &rest productions) nonterm
              (let (the-body
                    hit-epsilon
                    slots)
                (dolist (production productions)
                  (cond
                    (hit-epsilon
                     (error "Epsilon must be the last production"))

                    ((eq nil production)
                     (send-to the-body `(return-from parse-recursive))
                     (setf hit-epsilon t))

                    ((symbolp production)
                     (send-to the-body
                              `(try-parse ((iter iter))
                                 (return-from parse-recursive (parse-recursive ',production iter)))))

                    ((consp production)
                     (labels ((slot-name (thing)
                                (if (consp thing) (car thing) thing)))
                       (dolist (thing production)
                         (unless (keywordp thing)
                           (push (slot-name thing) slots))))
                     (send-to the-body
                              `(let (strict)
                                 (try-parse ((iter iter)
                                             :no-parse-form (when strict
                                                              (abort-parse "Oh no")))
                                   (let ((instance (make-instance ',nonterm-name))
                                         matches)
                                     (dolist (thing ',production)
                                       (block continue
                                         (when (eq :strict thing)
                                           (setf strict t)
                                           (return-from continue))
                                         (unless (consp thing)
                                           (setf thing (cons thing thing)))
                                         (let ((match (parse-recursive (cdr thing) iter)))
                                           (setf (slot-value instance (car thing)) match)
                                           (push match matches))))
                                     (setf (slot-value instance 'raw-matches) (nreverse matches))
                                     (return-from parse-recursive instance))))))))

                (when slots
                  (let ((unique-slots (make-hash-table)))
                    (dolist (slot slots)
                      (setf (gethash slot unique-slots) slot))
                    (send `(defclass ,nonterm-name (syntax-tree)
                             (,@(hash-table-keys unique-slots))))))

                (send `(defmethod parse-recursive
                           ((type (eql ',nonterm-name)) (iter lookahead-iterator))
                         ,@(nreverse the-body)
                         (no-parse "Nonterminal failed to match" ',nonterm-name)))))))))
    `(progn ,@(nreverse result-forms))))

(load "grammar-definition2.lisp")

(defgeneric parse-rec (source))

(defmethod parse-rec ((s string))
  (parse-rec (make-string-input-stream s)))

(defmethod parse-rec ((s stream))
  (parse-rec (make-iterator-lookahead (token-iterator s))))

(defmethod parse-rec ((iter lookahead-iterator))
  (parse-recursive 'shell iter))
