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
   :shcl/core/expand
   :shcl/core/exit-info :shcl/core/iterator :shcl/core/fd-table
   :shcl/core/dispatch-table)
  (:import-from :shcl/core/data #:define-data #:define-cloning-setf-expander)
  (:import-from :shcl/core/command #:define-special-builtin)
  (:import-from :shcl/core/evaluate #:evaluation-form-iterator #:translate #:expansion-preparation-form)
  (:import-from :shcl/core/posix)
  (:import-from :bordeaux-threads)
  (:import-from :babel)
  (:import-from :cffi)
  (:import-from :fset)
  (:export
   #:enable-reader-syntax #:evaluate-shell-string
   #:evaluate-constant-shell-string #:exit-failure #:check-result #:capture
   #:*splice-table*))
(in-package :shcl/core/lisp-interpolation)

(define-data lisp-form (a-word)
  ((form
    :initarg :form
    :initform (required)
    :documentation
    "The form this token represents")
   (function
    :reader lisp-form-function
    :writer unsafe-set-lisp-form-function
    :documentation
    "This function evaluates the form."))
  (:documentation
   "A token representing a lisp form.

If possible, the `function' slot should be compiled in the Lisp
lexical environment where the token was used.

This token always expands to one word."))

(define-cloning-setf-expander lisp-form-function
    unsafe-set-lisp-form-function)

(defmethod print-object ((token lisp-form) stream)
  (print-unreadable-object (token stream :type t)
    (format stream "~W" (slot-value token 'form))))

(defmethod expansion-preparation-form ((lisp-form lisp-form))
  (with-slots (form) lisp-form
    (let ((value (gensym "VALUE")))
      `(let ((,value ,lisp-form))
         (setf (lisp-form-function ,value)
               (lambda ()
                 (fset:seq
                  (make-string-fragment
                   (format nil "~A" ,form)
                   :quoted-p t))))
         ,value))))

(define-data lisp-splice-form (lisp-form)
  ()
  (:documentation
   "A token representing a lisp form which evaulates to a sequence.

During expansion, this token traverses the sequence returned by the
lisp form and turns each element into a separate word."))

(defmethod expansion-preparation-form ((lisp-form lisp-splice-form))
  (with-slots (form) lisp-form
    (let ((value (gensym "VALUE"))
          (seq (gensym "SEQ"))
          (result (gensym "RESULT"))
          (thing (gensym "THING")))
      `(let ((,value ,lisp-form))
         (setf (lisp-form-function ,value)
               (lambda ()
                 (let ((,seq ,form)
                       (,result (fset:empty-seq)))
                   (do-iterator (,thing (iterator ,seq))
                     (fset:push-last ,result (make-string-fragment (format nil "~A" ,thing) :quoted-p t))
                     (fset:push-last ,result (word-boundary)))
                   (setf ,result (fset:less-last ,result))
                   ,result)))
         ,value))))

(defmethod expand ((token lisp-form))
  (check-side-effects-allowed)
  (funcall (slot-value token 'function)))

(defun read-lisp-form (stream initiation-sequence)
  "Read a `lisp-form'."
  (declare (ignore initiation-sequence))
  (let ((form (read-preserving-whitespace stream)))
    (make-instance 'lisp-form :form form)))

(defun read-lisp-splice-form (stream initiation-sequence)
  "Read a `lisp-splice-form'."
  (declare (ignore initiation-sequence))
  (let ((form (read-preserving-whitespace stream)))
    (make-instance 'lisp-splice-form :form form)))

(defun hash-default-handler (stream initiation-sequence)
  "Read a comment"
  (unless (equal #\linefeed (aref initiation-sequence (- (length initiation-sequence) 1)))
    (read-line stream nil :eof))
  (close stream)
  nil)

(defparameter *splice-table-mixin*
  (as-> *empty-dispatch-table* x
    (with-dispatch-character x ",")
    (with-default-handler x "," 'read-lisp-form)
    (with-handler x ",@" 'read-lisp-splice-form)))

(defparameter *splice-table*
  (as-> *empty-dispatch-table* x
    (use-table x (standard-shell-readtable))
    (use-table x *splice-table-mixin*))
  "A shell readtable which supports injecting lisp forms.")

(defparameter *exit-reader-macro-table-mixin*
  (as-> *empty-dispatch-table* x
    (with-dispatch-character x "#")
    (with-default-handler x "#" 'hash-default-handler)
    (with-handler x "#$" 'end-shell-parse))
  "A readtable which allows the shell reader macro to terminate.")

(defparameter *interpolation-table*
  (as-> *empty-dispatch-table* x
    (use-table x (standard-shell-readtable))
    (use-table x *splice-table-mixin*)
    (use-table x *exit-reader-macro-table-mixin*))
  "A shell readtable which is suitable for use with the shell reader
macro.")

(defvar *proper-end-found* nil
  "This is used by `end-shell-parse' and `read-shell-command' to
ensure that use of the #$ reader macro is properly terminated.")

(defun end-shell-parse (stream initiation-sequence)
  "Mark the end of a #$ reader macro form."
  (declare (ignore initiation-sequence))
  (close stream)
  (setf *proper-end-found* t)
  nil)

(defun read-shell-command (stream subchar arg)
  "This function implements the #$ reader macro

It looks for an expression of the form
#$ <shell expression> #$"
  (declare (ignore subchar arg))
  (let ((*proper-end-found* nil))
    (let* ((raw-token-iter (token-iterator stream :readtable *interpolation-table*))
           (token-iter
            (make-computed-iterator
              (when *proper-end-found*
                (stop))
              (multiple-value-bind (value more) (next raw-token-iter)
                (if more
                    (emit value)
                    (stop)))))
           (tokens (iterable-values token-iter)))
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
    `(parse-token-sequence ,(walkable-to-list (tokens-in-string string :readtable table)))))

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
    (evaluate-shell-string command :readtable (standard-shell-readtable))))

(defmacro parse-token-sequence (tokens)
  "This macro is responsible for parsing (at macro expansion time) a
sequence of tokens and producing code which evaulates the shell
command they describe."
  (let* ((token-iter (forkable-wrapper-iterator (list-iterator tokens)))
         (commands (command-iterator token-iter))
         (evaluates (iterable-values (evaluation-form-iterator commands))))
    (do-iterator (value token-iter)
      (assert nil nil "Unconsumed token ~A found" value))
    (when (zerop (length evaluates))
      (setf evaluates #((truthy-exit-info))))
    (apply 'progn-concatenate (coerce evaluates 'list))))

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

(defun consume (retained-fd output-place encoding)
  "Read from `retained-fd' and store the resulting string in `output-place'.

When EOF is hit, this function will return.  This function will
release `retained-fd' when it finishes.  `output-place' should be an
instance of the `place' struct.

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
              (setf part (shcl/core/posix:posix-read (fd-wrapper-value retained-fd) +read-rate+ :binary t))
              (debug-log status "READ ~A BYTES" (length part))
              (loop :for byte :across part :do
                 (vector-push-extend byte content)))
            :while (not (zerop (length part))))
         (setf (place-value output-place) (babel:octets-to-string content :encoding encoding))
         (values))
    (fd-wrapper-release retained-fd)))

(defun %capture (streams encoding shell-command-fn)
  "This function implements the `capture' macro."
  (unless streams
    (funcall shell-command-fn)
    (return-from %capture ""))

  (with-fd-scope ()
    (let ((fds (mapcar 'decode-stream-descriptor streams))
          (string-result-place (make-place))
          thread
          exit-info)
      (destructuring-bind (read-end write-end) (retained-fds-pipe)
        (unwind-protect
             (progn
               (with-fd-scope ()
                 (dolist (fd fds)
                   (set-fd-binding fd write-end))
                 (fd-wrapper-retain read-end)
                 (setf thread
                       (bordeaux-threads:make-thread
                        (lambda () (consume read-end string-result-place encoding))))

                 ;; Maybe not actually an exit-info.  Still, its a
                 ;; good name.
                 (setf exit-info (multiple-value-list (funcall shell-command-fn))))

               (fd-wrapper-release write-end)
               (setf write-end nil)
               (when thread
                 (bordeaux-threads:join-thread thread))
               (values-list (cons (place-value string-result-place) exit-info)))
          (when read-end
            (fd-wrapper-release read-end))
          (when write-end
            (fd-wrapper-release write-end)))))))

(defmacro capture ((&key (streams ''(:stdout))
                         (encoding 'cffi:*default-foreign-encoding*))
                   &body shell-command)
  "Capture the output of the given shell command and return it as a
string.

"
  `(%capture ,streams ,encoding (lambda () ,@shell-command)))

(defmethod expansion-preparation-form ((command-word command-word))
  (let ((token-sym (gensym "TOKEN")))
    `(let ((,token-sym ,command-word))
       (setf (command-word-evaluate-fn ,token-sym)
             (lambda ()
               (capture (:streams '(:stdout))
                 (parse-token-sequence ,(coerce (command-word-tokens command-word) 'list)))))
       ,token-sym)))

(defmethod expand ((command-word command-word))
  (check-side-effects-allowed)
  (let* ((fn (or (command-word-evaluate-fn command-word)
                 (error "command-word is missing its fn ~A" command-word))))
    (multiple-value-bind (expansion exit-info) (funcall fn)
      (check-type exit-info (or null exit-info))
      (values
       (if *split-fields*
           (split expansion)
           (fset:seq (make-string-fragment expansion)))
       exit-info))))
