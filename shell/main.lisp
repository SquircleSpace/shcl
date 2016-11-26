(defpackage :shcl/shell/main
  (:use
   :common-lisp :shcl/core/lexer :shcl/core/shell-grammar :shcl/core/utility
   :shcl/core/evaluate :shcl/core/baking :shcl/core/thread
   :shcl/core/lisp-interpolation :shcl/core/shell-readtable :shcl/core/builtin)
  (:import-from :shcl/core/posix #:exit)
  (:import-from :shcl/shell/directory)
  (:import-from :shcl/shell/logs)
  (:import-from :shcl/shell/lisp-repl)
  (:import-from :trivial-gray-streams)
  (:import-from :cl-cli)
  (:import-from :uiop)
  (:export #:main #:run-shell-commands-in-stream #:run-shell-commands-in-string))
(in-package :shcl/shell/main)

(optimization-settings)

(defparameter *shell-readtable* +standard-shell-readtable+)

(defclass echo (trivial-gray-streams:fundamental-character-output-stream)
    ())

(defmethod trivial-gray-streams:stream-write-char ((s echo) char)
  (format *standard-output* "CHAR: ~W~%" char)
  char)

(defparameter *echo-characters* nil)

(defun debug-char-stream (stream)
  (if *echo-characters*
      (make-echo-stream stream (make-instance 'echo))
      stream))

(defun main-token-iterator (stream form-queue)
  (lookahead-iterator-wrapper
   (bake-tokens
    (make-iterator ()
      (let ((token (next-token stream :readtable *shell-readtable*)))
        (debug-log status "TOKEN: ~A" token)
        (when (typep token 'eof)
          (stop))
        (emit token)))
    form-queue)))

(defun display-prompt (input-stream output-stream)
  (when (and output-stream
             (interactive-stream-p input-stream)
             (not (listen input-stream)))
    (format output-stream "shcl> ")
    (finish-output output-stream)))

(defun restartable-command-iterator (raw-stream form-queue)
  (let* ((stream (debug-char-stream raw-stream))
         (tokens (main-token-iterator stream form-queue))
         (commands (command-iterator tokens)))
    (labels
        ((reset-token-iterator ()
           (loop :while (read-char-no-hang raw-stream nil nil))
           (setf stream (debug-char-stream raw-stream)
                 tokens (main-token-iterator stream form-queue)
                 commands (command-iterator tokens))))
      (if (interactive-stream-p raw-stream)
          (make-iterator ()
            (tagbody
             start
               (restart-case
                   (multiple-value-bind (value more) (next commands)
                     (if more
                         (emit value)
                         (stop)))
                 (ignore ()
                   (reset-token-iterator)
                   (go start)))))
          commands))))

(defparameter *enable-lisp-splice* nil)

(defparameter *help* nil)

(defparameter *options*
  '((*enable-lisp-splice* nil "Extend shell language to support splicing lisp expressions")
    (*help* nil "Show list of options")))

(defmacro with-options ((argv &key (options '*options*)) &body body)
  (let ((vars (gensym "VARS"))
        (values (gensym "VALUES"))
        (command (gensym "COMMAND"))
        (kwargs (gensym "KWARGS"))
        (rest (gensym "REST")))
    `(multiple-value-bind (,vars ,values ,command ,kwargs ,rest) (cl-cli:parse-cli ,argv ,options)
       (declare (ignore ,command ,kwargs ,rest))
       (progv ,vars ,values
         ,@body))))

(defun run-shell-commands-in-stream (stream &key prompt-stream)
  (let* ((form-queue (make-queue))
         (commands (restartable-command-iterator stream form-queue))
         last-result)
    (display-prompt stream prompt-stream)
    (restart-case
        (do-iterator (tree commands)
          (loop
             (let* ((stop '#:stop)
                    (form (dequeue-no-block form-queue stop)))
               (when (eq stop form)
                 (return))
               (debug-log status "EVAL ~A" form)
               (eval form)))
          (debug-log status "TREE: ~A" tree)
          (restart-case
              (let ((result (evaluate tree)))
                (setf last-result result)
                (debug-log status "RESULT ~A" result))
            (skip ()))
          (display-prompt stream prompt-stream))
      (die () (exit 1)))
    last-result))

(defun run-shell-commands-in-string (string)
  (run-shell-commands-in-stream (make-string-input-stream string)))

(define-builtin enable-lisp-syntax (args)
  (unless (equal 1 (fset:size args))
    (return-from enable-lisp-syntax 1))
  (setf *shell-readtable* (use-table *shell-readtable* *splice-table*))
  0)

(defun main ()
  (observe-revival)
  (with-options ((uiop:raw-command-line-arguments))
    (when *help*
      (cl-cli:help *options* nil :prog-name "shcl")
      (return-from main))

    (when *enable-lisp-splice*
      (setf *shell-readtable* *splice-table*))

    (run-shell-commands-in-stream *standard-input* :prompt-stream *standard-output*)))