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
  (:export #:main #:run-shell-commands-in-stream))
(in-package :shcl/shell/main)

(optimization-settings)

(defparameter *shell-readtable* +standard-shell-readtable+
  "The shell readtable used by the main shell loop.

This only changes how `shcl/shell/main:main' reads shell commands.
Macros and functions which consume shell expressions are not impacted
in any way by this variable.")

(defparameter *fresh-prompt* t
  "If non-nil, then the next time the user is prompted for input, the
standard prompt should be displayed.

When nil, a prompt indicating that the previous line is incomplete
should be displayed.")

(defun main-prompt ()
  "Return the string which should be displayed as the prompt for the
next line.

See `*fresh-prompt*'."
  (let ((result (if *fresh-prompt* "shcl> " "> ")))
    (setf *fresh-prompt* nil)
    result))

(defun main-token-iterator (stream form-queue)
  "Return an iterator that produces the tokens found in `stream'.

Tokens are read using `*shell-readtable*', and their bake forms (see
`shcl/core/baking:bake-tokens') are added to form-queue."
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
  "Return an iterator that emits the fully parsed commands that
`stream' contains.

Tokens are read using `main-token-iterator', and bake forms are placed
in `form-queue'.  See `shcl/core/baking:bake-tokens'."
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

(defparameter *enable-lisp-splice* nil
  "SHCL should permit splicing in lisp forms.

Do not set this variable directly.  `main' will set this based on the
arguments it receives.")
(defparameter *debug* nil)
(defparameter *help* nil
  "SHCL should display some basic help information and then exit.

Do not set this variable directly.  `main' will set this based on the
arguments it receives.")

(defparameter *options*
  '((*enable-lisp-splice* nil "Extend shell language to support splicing lisp expressions")
    (*debug* nil "Run a SWANK server")
    (*help* nil "Show list of options"))
  "The set of options recognized by `main'.  See `cl-cli:parse-cli'.")

(defmacro with-options ((argv &key (options '*options*)) &body body)
  "Parse options and establish dynamic bindings for them.

See `cl-cli:parse-cli'."
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
  "Read and evaluate shell commands contained within the given
stream.

This function will not display prompts.  You probably want to provide
a stream like the one created by `shcl/shell/prompt:make-editline-stream'."
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

(define-builtin enable-lisp-syntax (args)
  "Permit the use of lisp splice forms in shell expressions."
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
  "Start running SHCL's shell.

This function assumes that SHCL \"owns\" the process.  That means that
the process environment won't change without SHCL's knowledge and that
SHCL has total control over the process environment.  This means, for
example, that...
* SHCL is soley responsible for all file descriptor manipulations
* SHCL may install or remove any signal handlers it wishes"
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
