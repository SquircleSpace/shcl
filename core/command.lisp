;; Copyright 2018 Bradley Jensen
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

(defpackage :shcl/core/command
  (:use :common-lisp :shcl/core/utility)
  (:import-from :fset)
  (:import-from :alexandria)
  (:import-from :closer-mop)
  (:import-from :shcl/core/fd-table
                #:with-fd-streams #:with-living-fds #:simplify-fd-bindings)
  (:import-from :shcl/core/exit-info #:exit-info #:make-exit-info)
  (:import-from :shcl/core/data #:define-data)
  (:import-from :shcl/core/shell-environment #:preserve-special-variable #:with-subshell)
  (:import-from :shcl/core/support
                #:wifexited #:wexitstatus #:wifsignaled #:wtermsig #:wifstopped
                #:wstopsig)
  (:import-from :shcl/core/posix-types #:wuntraced)
  (:import-from :shcl/core/posix #:waitpid)
  (:import-from :shcl/core/environment #:*environment* #:linearized-exported-environment)
  (:import-from :shcl/core/working-directory #:current-working-directory-fd)
  (:import-from :shcl/core/fork-exec #:run)
  (:export
   #:define-builtin #:define-special-builtin #:lookup-command
   #:command-error #:wrap-errors #:invoke-command)
  (:documentation
   "This package contains functionality related to defining and
running shell commands."))
(in-package :shcl/core/command)

(optimization-settings)

(defgeneric invoke-command (command environment-modifier &rest args)
  (:documentation
   "Run the given command.

Think of this function like `funcall' for commands.

`environment-modifier' is a function that will, well, modify the
environment.  For example, it might set environment variables or
modify file descriptors.

`args' contains the strings that are passed to the shell command as
arguments."))

(defgeneric command-priority (command)
  (:documentation
   "Return the priority of the given command.

See `install-command' to learn more about priorities.

You do not need to specialize this function, but it is recommended."))

(define-condition command-error (error)
  ((message
    :initarg :message
    :initform nil
    :reader command-error-message
    :type (or string null)
    :documentation
    "A string describing the error that occured.")
   (error
    :initarg :error
    :initform nil
    :reader command-error-error
    :type (or error null)
    :documentation
    "Any `error'.")
   (return-code
    :initarg :return-code
    :initform 1
    :type (integer 0 255)
    :reader command-error-return-code
    :documentation
    "The desired return code for the command."))
  (:report
   (lambda (c s)
     (format s "~A"
             (or (command-error-message c)
                 (command-error-error c)
                 "Unknown error"))))
  (:documentation
   "A condition class that represents a failure while executing a
shell command implemented in lisp.

If you want to signal a pre-existing error, pass it in as the `error'
argument.  If you instead want to signal a simple error, pass in a
string message "))

(defmacro handle-command-errors (command-name &body body)
  "A convenience macro that handles `command-error' signals in a sane way.

The body is evaluted with a signal handler installed for
`command-error'.  Any `command-error's handled by this macro will be
printed to stderr along with the provided command name.  This macro
will then evaluate to the return code of the error.  See
`command-error-return-code'."
  (let ((name (gensym "NAME"))
        (e (gensym "E")))
    `(let ((,name ,command-name))
       (handler-case
           (progn ,@body)
         (command-error (,e)
                        (format *error-output* "~A: ~A~%" ,name ,e)
                        (ensure-exit-info (command-error-return-code ,e)))))))

(define-data command-namespace ()
  ((table
    :initarg :table
    :initform (fset:empty-map)
    :updater command-namespace-table)
   (fallback
    :initarg :fallback
    :updater command-namespace-fallback)))

(defun make-command-namespace (&key fallback)
  "Produce a new, empty `command-namespace'.

`fallback' is the command that should"
  (make-instance 'command-namespace :fallback fallback))

(define-data binary ()
  ()
  (:documentation
   "An object representing a shell command backed by a file on disk."))

(defvar *namespace* (make-instance 'command-namespace :fallback (make-instance 'binary))
  "The namespace containing all known commands.")

(preserve-special-variable '*namespace*)

(defun install-command (name command &optional (priority (command-priority command)))
  (let* ((table (command-namespace-table *namespace*))
         (priority-table (or (fset:lookup table name) (fset:empty-map))))
    (multiple-value-bind (existing-command found) (fset:lookup priority-table priority)
      (declare (ignore existing-command))
      (when found
        (warn "Redefining command: ~A" name))
      (setf (fset:lookup priority-table priority) command)
      (setf (fset:lookup (command-namespace-table *namespace*) name) priority-table)
      (values))))

(defun lookup-command (name)
  (multiple-value-bind (value found) (fset:lookup (command-namespace-table *namespace*) name)
    (if (and found (not (fset:empty? value)))
        (nth-value 1 (fset:at-rank value 0))
        (command-namespace-fallback *namespace*))))

(defun run-binary (args)
  (let ((bindings (fset:convert 'list (simplify-fd-bindings))))
    (with-living-fds (fds)
      (run args
           :fd-alist bindings
           :managed-fds fds
           :environment (linearized-exported-environment *environment*)
           :working-directory-fd (current-working-directory-fd)))))

(defmethod invoke-command ((command binary) environment-modifier &rest args)
  (let ((pid (with-subshell
               (funcall environment-modifier)
               (run-binary args)))
        status)

    (debug-log status "PID ~A = ~A" pid args)
    (setf status (nth-value 1 (waitpid pid wuntraced)))
    (debug-log status "EXITED ~A" pid)
    (when (wifstopped status)
      (warn "Stopped jobs should get a job number, but they don't"))

    (make-exit-info :pid pid
                    :exit-status (when (wifexited status)
                                   (wexitstatus status))
                    :exit-signal (when (wifsignaled status)
                                   (wtermsig status))
                    :stop-signal (when (wifstopped status)
                                   (wstopsig status)))))

(define-data special-builtin ()
  ((handler
    :initarg :handler
    :initform (required)
    :updater special-builtin-handler)))

(defun ensure-exit-info (exit-value)
  (etypecase exit-value
    ((integer 0 255)
     (make-exit-info :exit-status exit-value))
    (exit-info
     exit-value)))

(defmethod invoke-command ((command special-builtin) environment-modifier &rest args)
  (with-slots (handler) command
    (funcall environment-modifier)
    (handle-command-errors (first args)
      (ensure-exit-info (apply handler args)))))

(defmethod command-priority ((command special-builtin))
  0)

(defun register-special-builtin (name handler)
  (install-command name (make-instance 'special-builtin :handler handler)))

(define-data builtin ()
  ((handler
    :initarg :handler
    :initform (required)
    :updater builtin-handler)))

(defmethod invoke-command ((command builtin) environment-modifier &rest args)
  (with-slots (handler) command
    (funcall environment-modifier)
    (with-fd-streams ()
      (handle-command-errors (first args)
        (ensure-exit-info (apply handler args))))))

(defmethod command-priority ((command builtin))
  1)

(defun register-builtin (name handler)
  (install-command name (make-instance 'builtin :handler handler)))

(defun wrap-error (error)
  "If the given error isn't already a `command-error', wrap it with
one and resignal."
  (unless (typep error 'command-error)
    (error 'command-error :error error)))

(defmacro wrap-errors (&body body)
  "All errors that would normally escape this form are wrapped in a
`command-error' first."
  `(handler-bind
       ((error 'wrap-error))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-name (name)
    (check-type name (and (or symbol list) (not null)))
    (etypecase name
      (list
       (destructuring-bind (sym-name str-name) name
         (check-type sym-name (and symbol (not null)))
         (check-type str-name string)
         (values sym-name str-name)))
      (symbol
       (values name (string-downcase (symbol-name name)))))))

(defmacro %define-command (class name lambda-list &body body)
  (multiple-value-bind (function-sym string-form) (parse-name name)
                       (when (find #\/ string-form)
                         (warn "Command name ~W contains a #\/ character and will produce confusing results in shell commands" string-form))
                       `(progn
                          (defun ,function-sym ,lambda-list
                            ,@body)
                          (install-command ,string-form (make-instance ',class :handler #',function-sym)))))

(defmacro define-builtin (name shell-lambda-list &body body)
  "Define a new shell builtin.

`name' should either be a symbol or a list of the
form (`function-name' `builtin-name') where `function-name' is a
symbol and `builtin-name' is a string.  If `name' is simply a symbol,
then the builtin name is the downcased symbol name."
  `(%define-command builtin ,name ,shell-lambda-list
                    ,@body))

(defmacro define-special-builtin (name shell-lambda-list &body body)
  "Define a new special shell builtin.

`name' should either be a symbol or a list of the
form (`function-name' `builtin-name') where `function-name' is a
symbol and `builtin-name' is a string.  If `name' is simply a symbol,
then the builtin name is the downcased symbol name."
  `(%define-command special-builtin ,name ,shell-lambda-list
                    ,@body))

(define-special-builtin (colon ":") (&rest args)
  (declare (ignore args))
  0)

(define-special-builtin (dot ".") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "Sourcing files"))

(define-special-builtin exec (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "exec"))

(define-special-builtin exit (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "exit"))

(define-special-builtin (builtin-export "export") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "export"))

(define-special-builtin readonly (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "readonly"))

(define-special-builtin (builtin-return "return") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "return"))

(define-special-builtin (builtin-set "set") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "set"))

(define-special-builtin shift (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "shift"))

(define-special-builtin times (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "times"))

(define-special-builtin trap (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "trap"))

(define-special-builtin unset (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "unset"))
