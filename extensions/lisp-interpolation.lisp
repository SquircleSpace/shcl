(defpackage :shcl.extensions.lisp-interpolation
  (:use :common-lisp :shcl.utility :shcl.lexer :shcl.shell-grammar :shcl.evaluate :shcl.expand))
(in-package :shcl.extensions.lisp-interpolation)

(defgeneric prepare-for-baking (token))
(defmethod prepare-for-baking (token)
  token)

(defclass lisp-form (a-word)
  ((form
    :initarg :form
    :initform (required))))
(defmethod print-object ((token lisp-form) stream)
  (format stream "#<~A ~A>" (class-name (class-of token)) (slot-value token 'form)))

(defmethod prepare-for-baking ((lisp-form lisp-form))
  (let ((token (make-instance 'cooked-lisp-form)))
    (values
     token
     `(setf (slot-value ,token 'function)
            (lambda ()
              (fset:seq
               (make-string-fragment
                (format nil "~A" ,(slot-value lisp-form 'form))
                :quoted t)))))))

(defclass lisp-splice-form (lisp-form)
  ())

(defmethod prepare-for-baking ((lisp-form lisp-splice-form))
  (let ((token (make-instance 'cooked-lisp-form)))
    (values
     token
     `(setf (slot-value ,token 'function)
            (lambda ()
              (let ((seq ,(slot-value lisp-form 'form))
                    (result (fset:empty-seq)))
                (do-iterator (thing (iterator seq))
                  (fset:push-last result (make-string-fragment (format nil "~A" thing) :quoted t))
                  (fset:push-last result (word-boundary)))
                (setf result (fset:less-last result))
                result))))))

(defclass cooked-lisp-form (a-word)
  ((function
    :initform nil)))
(defmethod print-object ((token cooked-lisp-form) stream)
  (format stream "#<~A>" (class-name (class-of token))))

(defmethod expand ((token cooked-lisp-form))
  (funcall (slot-value token 'function)))

(defun read-lisp-form (stream initiation-sequence context)
  (declare (ignore initiation-sequence context))
  (let ((form (read-preserving-whitespace stream)))
    (make-instance 'lisp-form :form form)))

(defun read-lisp-splice-form (stream initiation-sequence context)
  (declare (ignore initiation-sequence context))
  (let ((form (read-preserving-whitespace stream)))
    (make-instance 'lisp-splice-form :form form)))

(defun hash-default-handler (stream initiation-sequence context)
  (unless (equal #\linefeed (aref initiation-sequence (- (length initiation-sequence) 1)))
    (read-line stream nil :eof))
  (lexer-context-mark-end-of-token context)
  t)

(defun read-shell-command (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((*shell-readtable* *shell-readtable*)
        proper-end-found)
    (make-shell-dispatch-character #\# :default-handler 'hash-default-handler)
    (labels
        ((end-shell-parse (s is context)
           (declare (ignore s is))
           (lexer-context-mark-end-of-token context)
           (setf proper-end-found t)
           t))
      (set-shell-dispatch-character #\# #\$ #'end-shell-parse))
    (make-shell-dispatch-character #\, :default-handler 'read-lisp-form)
    (set-shell-dispatch-character #\, #\@ 'read-lisp-splice-form)
    (let* ((raw-token-iter (token-iterator stream))
           (token-iter
            (make-iterator (:type 'token-iterator)
              (when proper-end-found
                (stop))
              (multiple-value-bind (value more) (next raw-token-iter)
                (if more
                    (emit value)
                    (stop)))))
           (tokens (iterator-values token-iter)))
      (unless proper-end-found
        (error "Expected #$ before EOF"))
      `(parse-token-sequence ,(coerce tokens 'list)))))

(defun enable-reader-syntax ()
  (set-dispatch-macro-character #\# #\$ 'read-shell-command))

(defmacro parse-token-sequence (tokens)
  (let ((oven (make-extensible-vector))
        (mangled-tokens (make-extensible-vector)))
    (dolist (token tokens)
      (multiple-value-bind (new-token preperation-form) (prepare-for-baking token)
        (vector-push-extend new-token mangled-tokens)
        (when preperation-form
          (vector-push-extend preperation-form oven))))
    (let* ((token-iter (vector-iterator mangled-tokens :type 'token-iterator))
           (commands (command-iterator token-iter))
           (evaluates (make-extensible-vector)))
      (do-iterator (command commands)
        (vector-push-extend `(evaluate ,command) evaluates))
      (do-iterator (value token-iter)
        (assert nil nil "Unconsumed token ~A found" value))
      (assert (not (and (not (zerop (length oven)))
                        (zerop (length evaluates))))
              nil "There can't be stuff in the oven but nothing to evaluate")
      (cond
        ((zerop (length evaluates))
         nil)
        ((zerop (length oven))
         `(progn ,@(coerce evaluates 'list)))
        (t
         `(progn
            ,@(coerce oven 'list)
            ,@(coerce evaluates 'list)))))))
