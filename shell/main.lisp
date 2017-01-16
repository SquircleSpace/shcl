(defpackage :shcl/shell/main
  (:use
   :common-lisp :trivial-gray-streams :shcl/core/lexer :shcl/core/shell-grammar
   :shcl/core/utility :shcl/core/evaluate :shcl/core/baking :shcl/core/thread
   :shcl/core/lisp-interpolation :shcl/core/shell-readtable :shcl/core/builtin)
  (:import-from :shcl/core/posix #:exit)
  (:import-from :shcl/shell/directory)
  (:import-from :shcl/shell/logs)
  (:import-from :shcl/shell/lisp-repl)
  (:import-from :shcl/shell/prompt #:make-editline-stream)
  (:import-from :cl-cli)
  (:import-from :uiop)
  (:export #:main #:run-shell-commands-in-stream #:run-shell-commands-in-string))
(in-package :shcl/shell/main)

(optimization-settings)

(defparameter *shell-readtable* +standard-shell-readtable+)

(defparameter *fresh-prompt* t)

(defun main-prompt ()
  (let ((result (if *fresh-prompt* "shcl> " "> ")))
    (setf *fresh-prompt* nil)
    result))

(defclass echo (fundamental-character-output-stream)
    ())

(defmethod stream-write-char ((s echo) char)
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

(defun restartable-command-iterator (stream form-queue)
  (let* ((tokens (main-token-iterator stream form-queue))
         (commands (command-iterator tokens)))
    (labels
        ((reset-token-iterator ()
           (loop :while (read-char-no-hang stream nil nil))
           (setf tokens (main-token-iterator stream form-queue)
                 commands (command-iterator tokens))))
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
               (go start))))))))

(defparameter *enable-lisp-splice* nil)
(defparameter *debug* nil)
(defparameter *help* nil)

(defparameter *options*
  '((*enable-lisp-splice* nil "Extend shell language to support splicing lisp expressions")
    (*debug* nil "Run a SWANK server")
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

(defun run-shell-commands-in-stream (stream)
  (let* ((form-queue (make-queue))
         (commands (restartable-command-iterator stream form-queue))
         (*fresh-prompt* t)
         last-result)
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
          (setf *fresh-prompt* t))
      (die () (exit 1)))
    last-result))

(defun run-shell-commands-in-string (string)
  (run-shell-commands-in-stream (make-string-input-stream string)))

(define-builtin enable-lisp-syntax (args)
  (unless (equal 1 (fset:size args))
    (return-from enable-lisp-syntax 1))
  (setf *shell-readtable* (use-table *shell-readtable* *splice-table*))
  0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "SWANK-LOADER")
    (load #P"/usr/share/emacs/site-lisp/slime/swank-loader.lisp"))
  (unless (find-package "SWANK")
    (funcall (intern "INIT" (find-package "SWANK-LOADER")))))

(defun main ()
  (observe-revival)
  (with-options ((uiop:raw-command-line-arguments))
    (when *help*
      (cl-cli:help *options* nil :prog-name "shcl")
      (return-from main))

    (when *debug*
      (funcall (intern "CREATE-SERVER" (find-package "SWANK")) :port 4005))

    (when *enable-lisp-splice*
      (setf *shell-readtable* *splice-table*))

    (let ((*package* (find-package :shcl-user)))
      (run-shell-commands-in-stream (make-editline-stream 'main-prompt)))))
