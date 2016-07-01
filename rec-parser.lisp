(in-package :shcl.rec-parser)

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
          (send `(defmethod parse ((type (eql ',name)) (iter lookahead-iterator))
                   (parse ',start-symbol iter)))

          ;; Now parsers for the terminals
          (dolist (term terminals)
            (send `(defmethod parse ((type (eql ',term)) (iter lookahead-iterator))
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
                     (send-to the-body `(return-from parse))
                     (setf hit-epsilon t))

                    ((symbolp production)
                     (send-to the-body
                              `(try-parse ((iter iter))
                                 (return-from parse (parse ',production iter)))))

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
                                         (let ((match (parse (cdr thing) iter)))
                                           (setf (slot-value instance (car thing)) match)
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
                           ((type (eql ',nonterm-name)) (iter lookahead-iterator))
                         ,@(nreverse the-body)
                         (no-parse "Nonterminal failed to match" ',nonterm-name)))))))))
    `(progn ,@(nreverse result-forms))))

(load "shell-grammar.lisp")

(defgeneric parse-shell (source))

(defmethod parse-shell ((s string))
  (parse (make-string-input-stream s)))

(defmethod parse-shell ((s stream))
  (parse (make-iterator-lookahead (token-iterator s))))

(defmethod parse-shell ((iter lookahead-iterator))
  (parse 'shell iter))
