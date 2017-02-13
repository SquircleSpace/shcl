(defpackage :shcl/shell/lisp-repl
  (:use :common-lisp :shcl/core/builtin :shcl/core/utility)
  (:import-from :shcl/shell/prompt
   #:with-history #:history-set-size #:history-enter #:make-editline-stream)
  (:import-from :shcl/shell/directory #:physical-pwd)
  (:export #:return-to-shell #:shell-help))
(in-package :shcl/shell/lisp-repl)

(optimization-settings)

(defpackage :shcl-user
  (:use :common-lisp :shcl/shell/lisp-repl))

(defun return-to-shell ()
  "Exit the lisp repl and return to the shell.

This function assumes that the lisp repl was entered with the builtin
comand `shcl-repl'."
  (throw 'return-to-shell t))

(defun shell-help ()
  "Print some helpful information about the SHCL repl."
  (format
   *standard-output*
   "Welcome to SHCL REPL!  The following special commands are recognized.
:help to see this
:shell to return to SHCL
"))

(defun shcl-repl-read (stdin)
  "Read a lisp form from the given stream.

Calls `return-to-shell' when no characters were read due to
encountering EOF."
  (let* ((eof-value '#:eof-value)
         (form (read stdin nil eof-value)))
    (when (eq form eof-value)
      (debug-log status "SHCL-REPL EOF")
      (format *standard-output* "~%")
      (return-to-shell))

    (debug-log status "SHCL-REPL READ ~A" form)
    form))

(defun print-list (list stdout)
  "Print a multiple-value list."
  (dolist (value list)
    (format stdout "~A~%" value))
  (finish-output stdout))

(defparameter *fresh-prompt* t
  "A non-nil value indicates that the next prompt the user sees should
be the standard prompt.

A nil value indicates that the next prompt the user sees should
indicate that the previous input isn't done yet.")

(defun repl-prompt ()
  "Return the string that should be displayed to the user at the
prompt."
  (let ((result (if *fresh-prompt* "shcl (lisp)> " "> ")))
    (setf *fresh-prompt* nil)
    result))

(define-builtin shcl-repl (args)
  "Enter the lisp repl."
  (declare (ignore args))
  (with-history (h)
    (history-set-size h 800)
    (catch 'return-to-shell
      (let* ((*package* (find-package :shcl-user))
             (*fresh-prompt* t)
             (*default-pathname-defaults* (uiop:parse-native-namestring (physical-pwd)))
             (stdin-raw (make-editline-stream :prompt-fn 'repl-prompt :history h))
             (captured-input (make-string-output-stream))
             (stdin (make-echo-stream stdin-raw captured-input)))
        (loop
           (restart-case
               (progn
                 (fresh-line *standard-output*)
                 (finish-output *standard-output*)
                 (let ((form (shcl-repl-read stdin)))
                   (history-enter h (get-output-stream-string captured-input))
                   (setf *fresh-prompt* t)
                   (case form
                     (:shell
                      (return-to-shell))
                     (:help
                      (shell-help)
                      (values))
                     (otherwise
                      (print-list (multiple-value-list (eval form))
                                  *standard-output*)))))
             (restart-lisp-repl ()
               (fresh-line *standard-output*))
             (exit-lisp-repl ()
               (return-to-shell)))))))
  0)
