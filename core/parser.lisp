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

(defpackage :shcl/core/parser
  (:use
   :common-lisp :alexandria :shcl/core/lexer :shcl/core/utility
   :shcl/core/iterator)
  (:import-from :shcl/core/advice #:define-advisable)
  (:import-from :closer-mop)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:export #:define-parser #:syntax-iterator #:make-internal-parse-error #:abort-parse))
(in-package :shcl/core/parser)

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

(defmethod make-load-form ((sy syntax-tree) &optional environment)
  (let ((slots (mapcar 'closer-mop:slot-definition-name (closer-mop:class-slots (class-of sy)))))
    (make-load-form-saving-slots sy :slot-names slots :environment environment)))

(defmethod print-object ((st syntax-tree) stream)
  (print-unreadable-object (st stream :type t :identity nil)
    (format stream "~A" (slot-value st 'raw-matches))))

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

(defstruct internal-parse-error
  message
  expected-tokens
  fatal)

(defmacro define-parser-part (&whole whole name (iter-sym) &body body)
  (multiple-value-bind (real-body declarations doc-string)
      (parse-body body :documentation t :whole whole)
    `(define-advisable ,name (,iter-sym)
       ,@(when doc-string (list doc-string))
       ,@declarations
       (labels
           ((no-parse (message &rest expected-tokens)
              (return-from ,name
                (values nil (make-internal-parse-error
                             :message message
                             :expected-tokens expected-tokens
                             :fatal nil))))
            (abort-parse (message &rest expected-tokens)
              (return-from ,name
                (values nil (make-internal-parse-error
                             :message message
                             :expected-tokens expected-tokens
                             :fatal t))))
            (emit (object)
              (return-from ,name
                (values object nil))))
         (declare (inline no-parse abort-parse emit)
                  (dynamic-extent #'no-parse #'abort-parse #'emit)
                  (ignorable #'no-parse #'abort-parse #'emit))
         (progn ,@real-body)
         (error "Explicit emit is required")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parser-part-name (thing)
    (symbol-nconc-intern nil "PARSE-" thing)))

(defmacro define-nonterminal (nonterminal-name &body productions)
  (let (the-body
        hit-epsilon
        (slots (make-hash-table)))

    (macrolet
        ((send-to (where &body rest)
           `(progn
              ,@(loop :for form :in rest :collect
                   `(push ,form ,where)))))
      (dolist (production productions)
        (cond
          (hit-epsilon
           (error "Epsilon must be the last production"))

          ((eq nil production)
           (send-to the-body `(emit nil))
           (setf hit-epsilon t))

          ((symbolp production)
           (send-to the-body
                    `(let ((ahead (fork-lookahead-iterator iter)))
                       (multiple-value-bind (value error) (,(parser-part-name production) ahead)
                         (when (or value (and error (internal-parse-error-fatal error)))
                           (move-lookahead-to iter ahead)
                           (return-from ,(parser-part-name nonterminal-name)
                             (values value error)))))))

          ((consp production)
           (dolist (thing production)
             (unless (keywordp thing)
               (setf (gethash (if (consp thing) (first thing) thing) slots) t)))

           (let (let-body
                 strict)
             (dolist (thing production)
               (block next
                 (when (eq :strict thing)
                   (setf strict t)
                   (return-from next))
                 (unless (consp thing)
                   (setf thing (list thing thing)))
                 (send-to let-body
                          `(multiple-value-bind (value error) (,(parser-part-name (second thing)) ahead)
                             ,(if strict
                                  `(when error
                                     (setf (internal-parse-error-fatal error) t)
                                     (move-lookahead-to iter ahead)
                                     (return-from ,(parser-part-name nonterminal-name)
                                       (values value error)))
                                  `(when error
                                     (return-from next-production)))
                             (setf (slot-value instance ',(car thing)) value)
                             (vector-push-extend value matches)))))
             (send-to the-body
                      `(block next-production
                         (let ((instance (make-instance ',nonterminal-name))
                               (matches (make-extensible-vector))
                               (ahead (fork-lookahead-iterator iter)))
                           ,@(nreverse let-body)
                           (setf (slot-value instance 'raw-matches) matches)
                           (move-lookahead-to iter ahead)
                           (return-from ,(parser-part-name nonterminal-name)
                             (values instance nil))))))))))

    `(progn
       ,@(unless (zerop (hash-table-size slots))
           `((defclass ,nonterminal-name (syntax-tree)
               (,@(hash-table-keys slots)))))

       (define-parser-part ,(parser-part-name nonterminal-name) (iter)
         ,@(nreverse the-body)
         (no-parse "Nonterminal failed to match" ',nonterminal-name))
       ',nonterminal-name)))

(defmacro define-terminal (name type)
  `(define-parser-part ,(parser-part-name name) (iter)
     (multiple-value-bind (value more) (peek-lookahead-iterator iter)
       (unless more
         (no-parse "unexpected EOF" ',name))
       (unless (typep value ',type)
         (no-parse "Token mismatch" ',name)))

     (emit (next iter))))

(defmacro define-parser (name &body body)
  (let (result-forms)
    (macrolet ((send (form) `(push ,form result-forms))
               (send-to (place form) `(push ,form ,place)))
      (multiple-value-bind (options nonterminals) (take-while #'grammar-option-form-p body)
        (let ((start-symbol (grammar-lookup-option :start-symbol options))
              (terminals (grammar-lookup-option :terminals options))
              (eof-symbol (grammar-lookup-option :eof-symbol options)))

          (unless (and start-symbol terminals)
            (error "Start symbol and terminals are required"))

          (when (left-recursion-p nonterminals)
            (error "Grammar has left recursion ~A" (left-recursion-p nonterminals)))

          ;; Easiest stuff first.  The root node
          (send `(define-parser-part ,(parser-part-name name) (iter)
                   (return-from ,(parser-part-name name) (,(parser-part-name start-symbol) iter))))

          ;; Now parsers for the terminals
          (dolist (term terminals)
            (send `(define-terminal ,term ,term)))

          (when eof-symbol
            (send `(define-parser-part ,(parser-part-name eof-symbol) (iter)
                     (multiple-value-bind (value more) (peek-lookahead-iterator iter)
                       (declare (ignore value))
                       (when more
                         (no-parse "expected EOF" ',eof-symbol))
                       (emit ',eof-symbol)))))

          ;; Now the nonterminals
          (dolist (nonterm nonterminals)
            (send `(define-nonterminal ,(first nonterm)
                     ,@(rest nonterm)))))))
    `(progn ,@(nreverse result-forms))))

(defun syntax-iterator (parser-function token-iterator)
  (make-iterator ()
    (let ((methods (closer-mop:generic-function-methods parser-function)))
      (dolist (m methods)
        (check-type m shcl/core/advice::advice-method)))
    (multiple-value-bind (value error) (funcall parser-function token-iterator)
      (when error
        (error 'abort-parse :message (internal-parse-error-message error)
               :expected-tokens (internal-parse-error-expected-tokens error)))
      (unless value
        (stop))
      (emit value))))
