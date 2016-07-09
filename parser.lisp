(in-package :shcl.parser)

(optimization-settings)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun take-while (predicate list)
    (let (result)
      (loop :while (and list (funcall predicate (car list)))
         :do (progn
               (push (car list) result)
               (setf list (cdr list))))
      (values (nreverse result) list)))

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

(defgeneric parse (type iterator))

(defmacro no-parse (message &rest expected-tokens)
  `(throw 'no-parse-tag (list ,message (list ,@expected-tokens))))

(defmacro try-parse ((iter-sym iter-form) no-parse-function &body body)
  (let ((iter (gensym "ITER"))
        (no-advance (gensym "NO-ADVANCE"))
        (no-parse (gensym "NO-PARSE"))
        (info (gensym "INFO")))
    `(let* ((,iter ,iter-form)
            (,iter-sym (fork-lookahead-iterator ,iter-form))
            (,no-parse ,no-parse-function)
            ,no-advance)
       (unwind-protect
            (try
                (progn ,@body)
              (no-parse-tag (,info)
                (setf ,no-advance t)
                (when ,no-parse
                  (apply ,no-parse ,info))))
         (unless ,no-advance
           (move-lookahead-to ,iter ,iter-sym))))))

(define-condition abort-parse (error)
  ((message
    :initarg :message
    :type string
    :initform "<No reason>"
    :accessor parse-error-message)
   (expected
    :initarg :expected-tokens
    :type list
    :initform nil
    :accessor parse-error-expected-tokens))
  (:report (lambda (c s)
             (format s "Parse error (~A)"
                     (parse-error-message c))
             (let ((expected (parse-error-expected-tokens c)))
               (when expected
                 (format s ", expected tokens ~A" expected))))))

(defmacro abort-parse (message &rest expected-tokens)
  `(error 'abort-parse :message ,message :expected-tokens (list ,@expected-tokens)))

(defmacro define-parser (name &body body)
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
          (send `(defparameter ,name ',name))
          (send `(defmethod parse ((type (eql ',name)) (iter token-iterator))
                   (parse ',start-symbol iter)))

          ;; Now parsers for the terminals
          (dolist (term terminals)
            (send `(defmethod parse ((type (eql ',term)) (iter token-iterator))
                     (multiple-value-bind (value more) (peek-lookahead-iterator iter)
                       (unless more
                         (no-parse "unexpected EOF" ',term))
                       (unless (typep value ',term)
                         (no-parse "Token mismatch" ',term)))

                     (next iter))))

          ;; Now the nonterminals
          (dolist (nonterm nonterminals)
            (destructuring-bind (nonterm-name &rest productions) nonterm
              (let (the-body
                    hit-epsilon
                    slots)

                (when (eq nonterm-name start-symbol)
                  (send-to the-body
                           `(multiple-value-bind (value more) (peek-lookahead-iterator iter)
                              (declare (ignore value))
                              (unless more
                                (return-from parse nil)))))

                (dolist (production productions)
                  (cond
                    (hit-epsilon
                     (error "Epsilon must be the last production"))

                    ((eq nil production)
                     (send-to the-body `(return-from parse))
                     (setf hit-epsilon t))

                    ((symbolp production)
                     (send-to the-body
                              `(try-parse (iter iter) nil
                                 (return-from parse (parse ',production iter)))))

                    ((consp production)
                     (labels ((slot-name (thing)
                                (if (consp thing) (first thing) thing)))
                       (dolist (thing production)
                         (unless (keywordp thing)
                           (push (slot-name thing) slots))))
                     (send-to the-body
                              `(let (strict)
                                 (try-parse (iter iter) (lambda (message expected)
                                                          (when strict
                                                            (abort-parse message expected)))
                                   (let ((instance (make-instance ',nonterm-name))
                                         matches)
                                     (dolist (thing ',production)
                                       (block continue
                                         (when (eq :strict thing)
                                           (setf strict t)
                                           (return-from continue))
                                         (unless (consp thing)
                                           (setf thing (list thing thing)))
                                         (let ((match (parse (second thing) iter)))
                                           (setf (slot-value instance (first thing)) match)
                                           (push match matches))))
                                     (setf (slot-value instance 'raw-matches) (nreverse matches))
                                     (return-from parse instance))))))))

                (when slots
                  (let ((unique-slots (make-hash-table)))
                    (dolist (slot slots)
                      (setf (gethash slot unique-slots) slot))
                    (send `(defclass ,nonterm-name (syntax-tree)
                             (,@(hash-table-keys unique-slots))))))

                (send `(defmethod parse
                           ((type (eql ',nonterm-name)) (iter token-iterator))
                         ,@(nreverse the-body)
                         (no-parse "Nonterminal failed to match" ',nonterm-name)))))))))
    `(progn ,@(nreverse result-forms))))

(defclass syntax-iterator (iterator)
  ())

(defun syntax-iterator (grammar token-iterator)
  (make-iterator (:type 'syntax-iterator)
    (try-parse (iter token-iterator)
        (lambda (message expected) (error 'abort-parse :message message :expected-tokens expected))
      (let ((value (parse grammar iter)))
        (when (eq nil value)
          (stop))
        (emit value)))))

(defparameter *depth* nil)

(defmethod parse :around (type iterator)
  (unless *depth*
    (return-from parse (call-next-method)))

  (let ((*depth* (+ 1 *depth*)))
    (format *error-output* "~A" (make-string (* 2 *depth*) :initial-element #\Space))
    (format *error-output* "~A~%" type)
    (let ((result (call-next-method)))
      (format *error-output* "~A" (make-string (* 2 *depth*) :initial-element #\-))
      (format *error-output* "Good ~A ~A~%" type result)
      result)))
