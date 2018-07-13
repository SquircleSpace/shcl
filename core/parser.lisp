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
  (:export
   ;; Foundations of parsing
   #:parser-error #:parser-value #:parser-let* #:parser-let #:parser-lookahead
   #:parser-try #:parser-choice #:parse-eof #:parse-object-of-type
   #:parser-handler-case

   ;; Convenience and high-level parsing tools
   #:syntax-iterator #:define-terminal #:define-nonterminal #:syntax-tree
   #:parse-failure))
(in-package :shcl/core/parser)

(optimization-settings)

(defclass syntax-tree ()
  ())

(defmethod make-load-form ((sy syntax-tree) &optional environment)
  (let ((slots (mapcar 'closer-mop:slot-definition-name (closer-mop:class-slots (class-of sy)))))
    (make-load-form-saving-slots sy :slot-names slots :environment environment)))

(defmethod print-object ((st syntax-tree) stream)
  (print-unreadable-object (st stream :type t :identity nil)
    (format stream "~A" (slot-value st 'raw-matches))))

(define-condition parse-failure (error)
  ((message
    :initarg :message
    :type string
    :initform "<No reason>"
    :accessor parse-failure-message)
   (expected
    :initarg :expected-tokens
    :type list
    :initform nil
    :accessor parse-failure-expected-tokens))
  (:report (lambda (c s)
             (format s "Parse failure: ~A"
                     (parse-failure-message c))
             (let ((expected (parse-failure-expected-tokens c)))
               (when expected
                 (format s ", expected tokens ~A" expected))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parser-part-name (thing)
    (symbol-nconc-intern nil "PARSE-" thing)))

(defun syntax-iterator (parser-function token-iterator)
  (make-iterator ()
    (multiple-value-bind (value error) (funcall parser-function token-iterator)
      (when error
        (error 'parse-failure :message (format nil "~S" error)))
      (unless value
        (stop))
      (emit value))))

(defun parser-error (error)
  (when (null error)
    (error "Parser errors must not be nil"))
  (values nil error))

(defun parser-value (value)
  (values value nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun let-bindings-transformer (block-name)
    (let ((err (gensym "ERR"))
          (value (gensym "VALUE")))
      (lambda (binding)
        (destructuring-bind (variable form) binding
          `(,variable (multiple-value-bind (,value ,err) ,form
                        (when ,err
                          (return-from ,block-name
                            (parser-error ,err)))
                        ,value)))))))

(defmacro parser-let* (bindings &body body)
  (let ((let*-block (gensym "LET*-BLOCK")))
    `(block ,let*-block
       (let* ,(mapcar (let-bindings-transformer let*-block) bindings)
         ,@body))))

(defmacro parser-let (bindings &body body)
  (let ((let-block (gensym "LET-BLOCK")))
    `(block ,let-block
       (let ,(mapcar (let-bindings-transformer let-block) bindings)
         ,@body))))

(defmacro parser-lookahead (iter &body body)
  (let ((fork (gensym "FORK"))
        (original-iter (gensym "ORIGINAL-ITER"))
        (value (gensym "VALUE"))
        (err (gensym "ERR")))
    `(let* ((,original-iter ,iter)
            (,fork (fork-lookahead-iterator ,original-iter)))
       (multiple-value-bind (,value ,err) (progn ,@body)
         (cond
           (,err
            (parser-error ,err))

           (t
            (move-lookahead-to ,original-iter ,fork)
            (parser-value ,value)))))))

(defmacro parser-try (iter &body body)
  (let ((fork (gensym "FORK"))
        (original-iter (gensym "ORIGINAL-ITER"))
        (value (gensym "VALUE"))
        (err (gensym "ERR")))
    `(let* ((,original-iter ,iter)
            (,fork (fork-lookahead-iterator ,original-iter)))
       (multiple-value-bind (,value ,err) (progn ,@body)
         (cond
           (,err
            (move-lookahead-to ,original-iter ,fork)
            (parser-error ,err))

           (t
            (parser-value ,value)))))))

(defmacro parser-choice (iter &body options)
  (let ((choice-block (gensym "CHOICE-BLOCK"))
        (original-iter (gensym "ORIGINAL-ITER"))
        (position (gensym "POSITION"))
        (err (gensym "ERR"))
        (errors (gensym "ERRORS"))
        (value (gensym "VALUE")))
    (labels
        ((option-handler (option)
           `(multiple-value-bind (,value ,err) ,option

              (unless ,err
                ;; Happy day!
                (return-from ,choice-block
                  (parser-value ,value)))

              (unless (eq (lookahead-iterator-position-token ,original-iter)
                          ,position)
                ;; Sad day :(
                (return-from ,choice-block
                  (parser-error ,err)))

              (push ,err ,errors)
              ;; Live to parse another day
              )))
      (if options
          `(block ,choice-block
             (let* ((,original-iter ,iter)
                    (,position (lookahead-iterator-position-token ,original-iter))
                    ,errors)
               ,@(mapcar #'option-handler options)
               (parser-error (list* :choices ,errors))))
          '(parser-error "No choices")))))

(defmacro parser-handler-case (iter parser-form &body clauses)
  (let ((value (gensym "VALUE"))
        (err (gensym "ERR"))
        (original-iter (gensym "ORIGINAL-ITER"))
        (position (gensym "POSITION")))
    (labels
        ((transform-clause (clause)
           (destructuring-bind (type (&optional var nil var-p) &rest forms) clause
             (if var-p
                 `(,type
                   (let ((,var ,err))
                     ,@forms))
                 `(,type
                   ,@forms)))))
      `(let* ((,original-iter ,iter)
              (,position (lookahead-iterator-position-token ,original-iter)))
         (multiple-value-bind (,value ,err) ,parser-form
           (cond
             ((not ,err)
              (parser-value ,value))
             ((eq ,position (lookahead-iterator-position-token ,original-iter))
              (typecase ,err
                ,@(mapcar #'transform-clause clauses)
                (t
                 (parser-error ,err))))
             (t
              (parser-error ,err))))))))

(defun parse-eof (iter)
  (parser-try iter
    (multiple-value-bind (value valid) (next iter)
      (declare (ignore value))
      (if valid
          (parser-error "EOF expected")
          (parser-value :eof)))))

(defun parse-object-of-type (iter type)
  (multiple-value-bind (value valid) (next iter)
    (cond
      ((not valid)
       (parser-error (format nil "Unexpected EOF, wanted ~A" type)))

      ((not (typep value type))
       (parser-error (format nil "Type mismatch, expected ~A got ~A" type (class-name (class-of value)))))

      (t
       (parser-value value)))))

(defmacro define-terminal (type &optional (name (parser-part-name type)))
  (let ((iter (gensym "ITER")))
    `(define-advisable ,name (,iter)
       ,(format nil "Parse successfully if the next object satisfies the type ~W" type)
       (parser-try ,iter
         (parse-object-of-type ,iter ',type)))))

(defmacro parse-instance (class &rest initargs)
  (let ((parse-instance-block (gensym "PARSE-INSTANCE-BLOCK"))
        (value (gensym "VALUE"))
        (err (gensym "ERR"))
        (args (make-extensible-vector)))
    (loop :while initargs :do
       (destructuring-bind (initarg initform &rest rest) initargs
         (setf initargs rest)
         (vector-push-extend initarg args)
         (vector-push-extend `(multiple-value-bind (,value ,err) ,initform
                                (when ,err
                                  (return-from ,parse-instance-block
                                    (parser-error ,err)))
                                ,value)
                             args)))
    `(block ,parse-instance-block
       (parser-value
        (make-instance ,class ,@(coerce args 'list))))))

(defmacro define-nonterminal (name &body productions)
  (let ((iter (gensym "ITER"))
        (slots (make-hash-table))
        (options (make-extensible-vector))
        (result-forms (make-extensible-vector))
        function-name
        class-name)
    (cond
      ((symbolp name)
       (setf function-name (parser-part-name name))
       (setf class-name name))

      (t
       (destructuring-bind (class fn) name
         (setf function-name fn)
         (setf class-name class))))

    (labels
        ((ensure-slot (name)
           (setf (gethash name slots) t)
           name)
         (add-option (form)
           (vector-push-extend form options))
         (add-result-form (form)
           (vector-push-extend form result-forms))
         (slot-parts (slot-description)
           (if (symbolp slot-description)
               (values (ensure-slot slot-description) (parser-part-name slot-description))
               (destructuring-bind (slot-name parser-function) slot-description
                 (values (ensure-slot slot-name) parser-function)))))

      (dolist (production productions)
        (cond
          ((null production)
           (add-option `(parser-value nil)))

          ((symbolp production)
           (add-option `(,production ,iter)))

          (t
           (let ((initargs
                  (loop :while production
                     :for slot-description = (pop production) :nconc
                     (multiple-value-bind (slot-name parser-function)
                         (slot-parts slot-description)
                       `(',slot-name (,parser-function ,iter))))))
             (add-option `(parse-instance ',class-name ,@initargs))))))

      (unless (zerop (hash-table-count slots))
        (add-result-form
         `(defclass ,class-name (syntax-tree)
            ,(loop :for key :being :the :hash-keys :of slots :collect
                `(,key :initarg ,key)))))

      (add-result-form
       `(define-advisable ,function-name (,iter)
          ,(format nil "This parser function tries to produce instance of the ~W class" class-name)
          (parser-choice ,iter
            ,@(coerce options 'list)
            (parser-error ,(format nil "No matches for ~A" function-name))))))

    (apply 'progn-concatenate (coerce result-forms 'list))))
