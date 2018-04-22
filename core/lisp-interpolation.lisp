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

(defpackage :shcl/core/lisp-interpolation
  (:use
   :common-lisp :shcl/core/utility :shcl/core/lexer :shcl/core/shell-grammar
   :shcl/core/evaluate :shcl/core/expand :shcl/core/baking
   :shcl/core/exit-info :shcl/core/iterator :shcl/core/fd-table
   :shcl/core/shell-readtable)
  (:import-from :shcl/core/command #:define-special-builtin)
  (:import-from :babel)
  (:import-from :cffi)
  (:import-from :fset)
  (:import-from :shcl/core/evaluate)
  (:import-from :shcl/core/posix)
  (:import-from :shcl/core/thread)
  (:import-from :bordeaux-threads)
  (:export
   #:enable-reader-syntax #:evaluate-shell-string
   #:evaluate-constant-shell-string #:exit-failure #:check-result #:capture
   #:*splice-table*))
(in-package :shcl/core/lisp-interpolation)

(defclass lisp-form (a-word)
  ((form
    :initarg :form
    :initform (required)
    :documentation
    "The form this token represents")
   (function
    :documentation
    "This function evaluates the form."))
  (:documentation
   "A token representing a lisp form.

If possible, the `function' slot should be compiled in the Lisp
lexical environment where the token was used.

This token always expands to one word."))
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
  ()
  (:documentation
   "A token representing a lisp form which evaulates to a sequence.

During expansion, this token traverses the sequence returned by the
lisp form and turns each element into a separate word."))

(defmethod bake-form-for-token ((lisp-form lisp-splice-form))
  (let ((seq (gensym "SEQ"))
        (result (gensym "RESULT"))
        (thing (gensym "THING")))
    `(setf (slot-value ,lisp-form 'function)
           (lambda ()
             (let ((,seq ,(slot-value lisp-form 'form))
                   (,result (fset:empty-seq)))
               (do-iterator (,thing (iterator ,seq))
                 (fset:push-last ,result (make-string-fragment (format nil "~A" ,thing) :quoted t))
                 (fset:push-last ,result (word-boundary)))
               (setf ,result (fset:less-last ,result))
               ,result)))))

(defmethod expand ((token lisp-form))
  (funcall (slot-value token 'function)))

(defun read-lisp-form (stream initiation-sequence context)
  "Read a `lisp-form'."
  (declare (ignore initiation-sequence))
  (let* ((form (read-preserving-whitespace stream))
         (token (make-instance 'lisp-form :form form)))
    (shell-lexer-context-add-part context token)))

(defun read-lisp-splice-form (stream initiation-sequence context)
  "Read a `lisp-splice-form'."
  (declare (ignore initiation-sequence))
  (let* ((form (read-preserving-whitespace stream))
         (token (make-instance 'lisp-splice-form :form form)))
    (shell-lexer-context-add-part context token)))

(defun hash-default-handler (stream initiation-sequence context)
  "Read a comment"
  (unless (equal #\linefeed (aref initiation-sequence (- (length initiation-sequence) 1)))
    (read-line stream nil :eof))
  (lexer-context-mark-end-of-token context))

(define-shell-readtable *splice-table-mixin*
  (with-dispatch-character ",")
  (with-default-handler "," 'read-lisp-form)
  (with-handler ",@" 'read-lisp-splice-form))

(define-shell-readtable *splice-table*
  "A shell readtable which supports injecting lisp forms."
  (use-table +standard-shell-readtable+)
  (use-table *splice-table-mixin*))

(define-shell-readtable *exit-reader-macro-table-mixin*
  "A readtable which allows the shell reader macro to terminate."
  (with-dispatch-character "#")
  (with-default-handler "#" 'hash-default-handler)
  (with-handler "#$" 'end-shell-parse))

(define-shell-readtable *interpolation-table*
  "A shell readtable which is suitable for use with the shell reader
macro."
  (use-table +standard-shell-readtable+)
  (use-table *splice-table-mixin*)
  (use-table *exit-reader-macro-table-mixin*))

(defvar *proper-end-found* nil
  "This is used by `end-shell-parse' and `read-shell-command' to
ensure that use of the #$ reader macro is properly terminated.")

(defun end-shell-parse (stream initiation-sequence context)
  "Mark the end of a #$ reader macro form."
  (declare (ignore stream initiation-sequence))
  (lexer-context-mark-end-of-token context)
  (setf *proper-end-found* t))

(defun read-shell-command (stream subchar arg)
  "This function implements the #$ reader macro

It looks for an expression of the form
#$ <shell expression> #$"
  (declare (ignore subchar arg))
  (let ((*proper-end-found* nil))
    (let* ((raw-token-iter (token-iterator stream :readtable *interpolation-table*))
           (token-iter
            (make-iterator ()
              (when *proper-end-found*
                (stop))
              (multiple-value-bind (value more) (next raw-token-iter)
                (if more
                    (emit value)
                    (stop)))))
           (tokens (iterator-values token-iter)))
      (unless *proper-end-found*
        (error "Expected #$ before EOF"))
      `(parse-token-sequence ,(coerce tokens 'list)))))

(defun enable-reader-syntax ()
  "Enable use of the #$ reader macro for reading shell commands."
  (set-dispatch-macro-character #\# #\$ 'read-shell-command))

(defmacro evaluate-constant-shell-string (string &key (readtable *splice-table*))
  "Parse the given shell command and arrange for the commands
contained within to be executed at runtime.

Note, embedded lisp forms will have access to the lexical environment
where this form appears."
  (assert (typep string 'string) (string) "Only constant shell strings can be evaluated by this macro")
  (let ((table (typecase readtable
                 (symbol (symbol-value readtable))
                 (t readtable))))
    `(parse-token-sequence ,(coerce (tokens-in-string string :readtable table) 'list))))

(defun evaluate-shell-string (string &key (readtable *splice-table*))
  "At runtime, parse and execute the given shell command.

Note, embedded lisp forms will not have access to the lexical
environment in which this function call appears.  They will be
evauluated in the null lexical environment."
  (eval `(evaluate-constant-shell-string ,string ,@(when readtable `(:readtable ,readtable)))))

(define-special-builtin (builtin-eval "eval") (&rest args)
  "Evaluate the given arguments as a shell command."
  (let* (sep-needed
         (command
          (with-output-to-string (s)
            (dolist (word args)
              (unless (zerop (length word))
                (when sep-needed
                  (write-char #\space s))
                (write-string word s)
                (setf sep-needed t))))))
    (when (zerop (length command))
      (return-from builtin-eval 0))
    (evaluate-shell-string command :readtable +standard-shell-readtable+)))

(defmacro parse-token-sequence (tokens)
  "This macro is responsible for parsing (at macro expansion time) a
sequence of tokens and producing code which evaulates the shell
command they describe."
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
  ((info
    :type exit-info
    :initarg :info
    :reader exit-failure-info
    :documentation
    "Information about the process that exited abnormally"))
  (:report (lambda (c s) (format s "Command exited with info ~A" (exit-failure-info c))))
  (:documentation
   "A condition that represents a command exiting with non-zero exit
status."))

(defun check-result (exit-info)
  "Returns the given `exit-info', but signals an `exit-failure'
condition if it indicates a non-zero exit status."
  (unless (exit-info-true-p exit-info)
    (cerror "Ignore error" 'exit-failure :info exit-info))
  exit-info)

(defgeneric decode-stream-descriptor (descriptor)
  (:documentation
   "Translate the given description of a stream into its corresponding
file descriptor.

This is intended to be used with `capture'."))
(defmethod decode-stream-descriptor ((descriptor integer))
  descriptor)
(defmethod decode-stream-descriptor ((stdout (eql :stdout)))
  1)
(defmethod decode-stream-descriptor ((stdout (eql :stderr)))
  2)

(defconstant +read-rate+ 4096
  "The number of bytes that we should try to read from a pipe at
once.")

(defstruct place
  value)

(defun consume (retained-fd output-place semaphore encoding)
  "Read from `retained-fd' and store the resulting string in `output-place'.

When EOF is hit, the given semaphore will be signaled.  This function
will release `retained-fd' when it finishes.  `output-place' should be
an instance of the `place' struct.

This function is not meant to be used directly.  Only `capture' should
call this function."
  ;; Sigh.  We need to collect all of the bytes from posix-read and
  ;; then decode it all at once.  Otherwise, we might cut a character
  ;; in half and have decoding failures.  We could try to figure out a
  ;; subset of the data we read that is whole and decode just that,
  ;; but we're probably not going to be dealing with big strings
  ;; anyway.  So, let's just take the easy way out for now.
  (unwind-protect
       (let ((content (make-extensible-vector :element-type '(unsigned-byte 8)))
             (part #()))
         (loop :do
            (progn
              (setf part (shcl/core/posix:posix-read retained-fd +read-rate+ :binary t))
              (debug-log status "READ ~A BYTES" (length part))
              (loop :for byte :across part :do
                 (vector-push-extend byte content)))
            :while (not (zerop (length part))))
         (setf (place-value output-place) (babel:octets-to-string content :encoding encoding))
         (values))
    (fd-release retained-fd)
    (shcl/core/thread:semaphore-signal semaphore)))

(defun %capture (streams encoding shell-command-fn)
  "This function implements the `capture' macro."
  (unless streams
    (funcall shell-command-fn)
    (return-from %capture ""))

  (with-fd-scope ()
    (let ((fds (mapcar 'decode-stream-descriptor streams))
          (string-result-place (make-place))
          (semaphore (shcl/core/thread:make-semaphore))
          exit-info)
      (multiple-value-bind (read-end write-end) (pipe-retained)
        (unwind-protect
             (progn
               (with-fd-scope ()
                 (dolist (fd fds)
                   (bind-fd fd write-end))
                 (fd-retain read-end)
                 (bordeaux-threads:make-thread
                  (lambda () (consume read-end string-result-place semaphore encoding)))

                 ;; Maybe not actually an exit-info.  Still, its a
                 ;; good name.
                 (setf exit-info (multiple-value-list (funcall shell-command-fn))))

               (fd-release write-end)
               (setf write-end nil)
               (shcl/core/thread:semaphore-wait semaphore)
               (values-list (cons (place-value string-result-place) exit-info)))
          (when read-end
            (fd-release read-end))
          (when write-end
            (fd-release write-end)))))))

(defmacro capture ((&key (streams ''(:stdout))
                         (encoding 'cffi:*default-foreign-encoding*))
                   &body shell-command)
  "Capture the output of the given shell command and return it as a
string.

"
  `(%capture ,streams ,encoding (lambda () ,@shell-command)))

(defmethod bake-form-for-token ((command-word command-word))
  `(setf (command-word-evaluate-fn ,command-word)
         (lambda ()
           (capture (:streams '(:stdout))
             (parse-token-sequence ,(coerce (command-word-tokens command-word) 'list))))))

(defmethod expand ((command-word command-word))
  (let* ((fn (or (command-word-evaluate-fn command-word)
                 (error "command-word is missing its fn ~A" command-word))))
    (multiple-value-bind (expansion exit-info) (funcall fn)
      (check-type exit-info (or null exit-info))
      (values
       (if *split-fields*
           (split expansion)
           (fset:seq (make-string-fragment expansion)))
       exit-info))))
