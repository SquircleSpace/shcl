(defpackage :shcl/lisp-interpolation
  (:use :common-lisp :shcl/utility :shcl/lexer :shcl/shell-grammar
        :shcl/evaluate :shcl/expand :shcl/baking :shcl/builtin)
  (:import-from :fset)
  (:import-from :shcl/evaluate)
  (:import-from :shcl/posix)
  (:import-from :shcl/thread)
  (:import-from :bordeaux-threads)
  (:export
   #:enable-shell-splice-syntax #:enable-reader-syntax))
(in-package :shcl/lisp-interpolation)

(defclass lisp-form (a-word)
  ((form
    :initarg :form
    :initform (required))
   function))
(defmethod print-object ((token lisp-form) stream)
  (format stream "#<~A ~A>" (class-name (class-of token)) (slot-value token 'form)))

(defmethod bake-form-for-token ((lisp-form lisp-form))
  `(setf (slot-value ,lisp-form 'function)
         (lambda ()
           (fset:seq
            (make-string-fragment
             (format nil "~A" ,(slot-value lisp-form 'form))
             :quoted t)))))

(defclass lisp-splice-form (lisp-form)
  ())

(defmethod bake-form-for-token ((lisp-form lisp-splice-form))
  (let ((seq (gensym "SEQ"))
        (result (gensym "RESULT"))
        (thing (gensym "THING")))
    `(setf (slot-value ,lisp-form 'function)
           (lambda ()
             (let ((,seq ,(slot-value lisp-form 'form))
                   (,result (fset:empty-seq)))
               (do-iterator (,thing (iterator ,seq))
                 (fset:push-last ,result (make-string-fragment (format nil "~A" thing) :quoted t))
                 (fset:push-last ,result (word-boundary)))
               (setf ,result (fset:less-last ,result))
               ,result)))))

(defmethod expand ((token lisp-form))
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
    (enable-shell-splice-syntax)
    (let* ((raw-token-iter (token-iterator stream))
           (token-iter
            (make-iterator ()
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

(defun enable-shell-splice-syntax ()
  (make-shell-dispatch-character #\, :default-handler 'read-lisp-form)
  (set-shell-dispatch-character #\, #\@ 'read-lisp-splice-form))

(defmacro parse-token-sequence (tokens)
  (let ((oven (make-extensible-vector)))
    (dolist (token tokens)
      (let ((form (bake-form-for-token token)))
        (when form
          (vector-push-extend form oven))))
    (let* ((token-iter (lookahead-iterator-wrapper (list-iterator tokens)))
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

(define-condition exit-failure (error)
  ((code
    :type integer
    :initarg :code
    :reader exit-failure-code))
  (:report (lambda (c s) (format s "Command exited with status ~A" (exit-failure-code c)))))

(defun %check-result (shell-command-fn)
  (let ((result (funcall shell-command-fn)))
    (unless (shcl/evaluate::exit-true-p result)
      (cerror "Ignore error" 'exit-failure :code (shcl/evaluate::exit-code result)))
    result))

(defmacro check-result (() shell-command)
  `(%check-result (lambda () ,shell-command)))

(defgeneric decode-stream-descriptor (descriptor))
(defmethod decode-stream-descriptor ((descriptor integer))
  descriptor)
(defmethod decode-stream-descriptor ((stdout (eql :stdout)))
  1)
(defmethod decode-stream-descriptor ((stdout (eql :stderr)))
  2)

(defconstant +read-rate+ 4096)

(defun consume (retained-fd output-buffer semaphore)
  (unwind-protect
       (let (part)
         (loop :do
            (progn
              (setf part (shcl/posix:posix-read retained-fd +read-rate+))
              (debug-log 'status "READ ~A BYTES" (length part))
              (write-string part output-buffer))
            :while (not (zerop (length part)))))
    (shcl/evaluate::fd-release retained-fd)
    (shcl/thread:semaphore-signal semaphore)))

(defun %capture (streams shell-command-fn)
  (shcl/evaluate::with-fd-scope ()
    (let ((fds (mapcar 'decode-stream-descriptor streams))
          (result-buffer (make-string-output-stream))
          (semaphore (shcl/thread:make-semaphore)))
      (multiple-value-bind (read-end write-end) (shcl/evaluate::pipe-retained)
        (unwind-protect
             (progn
               (dolist (fd fds)
                 (shcl/evaluate::bind-fd fd write-end))
               (shcl/evaluate::fd-retain read-end)
               (bordeaux-threads:make-thread (lambda () (consume read-end result-buffer semaphore)))

               (funcall shell-command-fn)

               (shcl/evaluate::fd-release write-end)
               (setf write-end nil)
               (shcl/thread:semaphore-wait semaphore)
               (get-output-stream-string result-buffer))
          (when read-end
            (shcl/evaluate::fd-release read-end))
          (when write-end
            (shcl/evaluate::fd-release write-end)))))))

(defmacro capture ((&rest streams) shell-command)
  (setf shell-command `(check-result () ,shell-command))
  (when (null streams)
    (return-from capture `(progn ,shell-command "")))
  `(%capture (list ,@streams) (lambda () ,shell-command)))

(define-builtin enable-lisp-syntax (args)
  (unless (equal 1 (fset:size args))
    (return-from enable-lisp-syntax 1))
  (enable-shell-splice-syntax)
  0)

(defun return-to-shell ()
  (throw 'return-to-shell t))

(define-builtin lisp-repl (args)
  (declare (ignore args))
  (catch 'return-to-shell
    (let ((*package* (find-package :cl-user)))
      (labels
          ((print-list (list)
             (dolist (value list)
               (format *standard-output* "~A~%" value))
             (finish-output *standard-output*))
           (rep ()
             (fresh-line *standard-output*)
             (format *standard-output* "shcl (lisp)> ")
             (finish-output *standard-output*)
             (print-list (multiple-value-list (eval (read))))))
        (loop
           (restart-case (rep)
             (restart-lisp-repl ()
               (fresh-line *standard-output*))
             (exit-lisp-repl ()
               (return-to-shell)))))))
  0)
