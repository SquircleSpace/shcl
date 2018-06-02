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

(defpackage :shcl/core/shell-form
  (:use :common-lisp :shcl/core/utility)
  (:import-from :shcl/core/iterator #:do-iterator #:iterator)
  (:import-from :shcl/core/shell-environment
   #:destroy-preserved-shell-environment #:preserve-shell-environment
   #:with-restored-shell-environment #:with-subshell)
  (:import-from :shcl/core/environment #:env)
  (:import-from :shcl/core/exit-info
   #:exit-info-true-p #:exit-info-false-p #:invert-exit-info #:truthy-exit-info
   #:falsey-exit-info)
  (:import-from :shcl/core/fd-table
   #:retained-fds-pipe #:fd-wrapper-release #:with-fd-scope #:set-fd-binding
   #:fd-bind* #:receive-ref-counted-fd)
  (:import-from :shcl/core/command
   #:invoke-command #:lookup-command #:define-special-builtin #:wrap-errors)
  (:import-from :bordeaux-threads)
  (:import-from :alexandria #:parse-body)
  (:export #:shell-pipeline #:shell-not #:& #:with-subshell #:shell-when
           #:shell-unless #:shell-if #:shell-while #:shell-for
           #:shell-run #:shell-and #:shell-or))
(in-package :shcl/core/shell-form)

(optimization-settings)

(defstruct async-result
  values
  thread
  error)

(defun async-result-get (result)
  (when (async-result-thread result)
    (bordeaux-threads:join-thread (async-result-thread result))
    (setf (async-result-thread result) nil))

  (when (async-result-error result)
    (error (async-result-error result)))

  (values-list (async-result-values result)))

(defun call-with-parallel (fn)
  (let ((result (make-async-result)))
    (setf (async-result-thread result)
          (bordeaux-threads:make-thread
           (lambda ()
             (handler-case
                 (setf (async-result-values result)
                       (multiple-value-list (funcall fn)))
               (error (e)
                 (setf (async-result-error result) e))))))
    result))

(defmacro parallel (&body body)
  `(call-with-parallel (lambda () ,@body)))

(defun call-with-parallel-shell (fn)
  (let ((shell-environment (preserve-shell-environment :label 'call-with-parallel-shell))
        spawned)
    (unwind-protect
         (prog1
             (parallel
               (unwind-protect
                    (with-restored-shell-environment shell-environment
                      ;; Happy case.  The other thread has taken the
                      ;; shell environment
                      (destroy-preserved-shell-environment shell-environment)
                      (funcall fn))
                 ;; Sad case.  The shell environment might not have
                 ;; been fully applied.  Do our best to clean it up.
                 (destroy-preserved-shell-environment shell-environment)))
           ;; Happy case.  We were able to spawn the other thread
           (setf spawned t))

      (unless spawned
        ;; Very sad case.  Spawning the other thread failed.  The
        ;; other thread won't be cleaning up this shell environment.
        (destroy-preserved-shell-environment shell-environment)))))

(defmacro parallel-shell (&body body)
  `(call-with-parallel-shell (lambda () ,@body)))

(defconstant +pipe-read-fd+ 0)
(defconstant +pipe-write-fd+ 1)

(defun pipeline-fn (&rest pipeline-fns)
  (let ((pipeline-fns (coerce pipeline-fns 'vector))
        (results (make-array (length pipeline-fns)))
        write-fd)
    (case (length pipeline-fns)
      (0
       (error "Pipelines must have at least one command"))
      (1
       (return-from pipeline-fn (funcall (aref pipeline-fns 0)))))

    (unwind-protect
         (labels
             ((run (read-end write-end index)
                (with-fd-scope ()
                  (when read-end
                    (set-fd-binding +pipe-read-fd+ read-end))
                  (when write-end
                    (set-fd-binding +pipe-write-fd+ write-end))
                  (setf (aref results index) (call-with-parallel-shell (aref pipeline-fns index)))
                  (check-type (aref results index) async-result))))
           (loop :for index :from (1- (length pipeline-fns)) :downto 1 :do
              (destructuring-bind (read-end write-end) (retained-fds-pipe)
                (unwind-protect
                     (run read-end write-fd index)
                  (when write-fd
                    (fd-wrapper-release write-fd))
                  (setf write-fd write-end)
                  (fd-wrapper-release read-end))))

           (assert write-fd)
           (run nil write-fd 0))
      (when write-fd
        (fd-wrapper-release write-fd)
        (setf write-fd nil)))

    ;; Wait for each job in the pipeline except the last one
    (loop :for index :from 0 :below (1- (length pipeline-fns)) :do
       (async-result-get (aref results index)))

    (async-result-get (aref results (1- (length pipeline-fns))))))

(defmacro shell-pipeline (&body body)
  "Evaluate each form of body where standard-output of each form is
connected to standard-input of the next form."
  `(pipeline-fn
    ,@(mapcar (lambda (form)
                `(lambda ()
                   ,form))
              body)))

(defmacro shell-not (&body body)
  "Invert the exit status returned by the given forms.

This is basically just `invert-exit-info' as a macro."
  `(invert-exit-info (progn ,@body)))

(defmacro shell-and (&body body)
  "Return the first falsey exit status produced by a form in `body'.

When a falsey exit status is produced, this macro will short circut
and skip the remaining forms."
  (unless body
    (return-from shell-and
      '(truthy-exit-info)))

  (unless (cdr body)
    (return-from shell-and
      (car body)))

  (let ((and (gensym "AND"))
        (result (gensym "RESULT")))
    `(block ,and
       ,@(loop :for tail :on body :collect
            (if (cdr tail)
                `(let ((,result ,(car tail)))
                   (unless (exit-info-true-p ,result)
                     (return-from ,and ,result)))
                (car tail))))))

(defmacro shell-or (&body body)
  "Return the first truthy exit status produced by a form in `body'.

When a truthy exit status is produced, this macro will short circut
and skip the remaining forms."
  (unless body
    (return-from shell-or
      '(falsey-exit-info)))

  (unless (cdr body)
    (return-from shell-or
      (car body)))

  (let ((or (gensym "OR"))
        (result (gensym "RESULT")))
    `(block ,or
       ,@(loop :for tail :on body :collect
            (if (cdr tail)
                `(let ((,result ,(car tail)))
                   (when (exit-info-true-p ,result)
                     (return-from ,or ,result)))
                (car tail))))))

(defmacro & (&body body)
  "Evaluate the given forms in the background."
  (declare (ignore body))
  (error 'not-implemented :feature "Background jobs"))

(defmacro shell-if (condition then &optional (else nil else-p))
  "The shell version of `if'.

First, `condition' is evaluated.  If it produces a truthy exit status,
then the `then' form is evaluated and the result is returned.  If it a
produces a falsey exit status, then this macro will evaluate and
return the result of `else'.  If an `else' form isn't provided, then
the return value of `condition' is produced."
  (let ((value (gensym "VALUE")))
    `(let ((,value ,condition))
       (if (exit-info-true-p ,value)
           ,then
           ,(if else-p
                else
                value)))))

(defmacro shell-when (condition &body body)
  "Like `when', but `condition' is evaluated as an `exit-info'."
  `(shell-if ,condition
             ,@body))

(defmacro shell-unless (condition &body body)
  "Like `unless', but `condition' is evaluated as a `exit-info'"
  `(shell-if (shell-not ,condition)
             ,@body))

(define-condition loop-jump ()
  ((count
    :initarg :count
    :initform 1
    :accessor loop-jump-count)
   (exit-info
    :initarg :exit-info
    :initform (truthy-exit-info)
    :reader loop-jump-exit-info)))

(define-condition loop-break (loop-jump)
  ()
  (:documentation
   "A condition representing that the an enclosing loop should stop
running."))

(define-condition loop-continue (loop-jump)
  ()
  (:documentation
   "A condition representing that an enclosing loop should skip ahead
to the next iteration."))

(defmacro jump-level (name &body body)
  (let ((block-label (gensym (symbol-name name)))
        (err (gensym "ERR")))
    `(block ,block-label
       (handler-bind
           ((,name
             (lambda (,err)
               (when (plusp (loop-jump-count ,err))
                 (decf (loop-jump-count ,err))
                 (when (plusp (loop-jump-count ,err))
                   (signal ,err))
                 (return-from ,block-label
                   (loop-jump-exit-info ,err))))))
         ,@body))))

(defmacro break-level (&body body)
  `(jump-level loop-break
     ,@body))

(defmacro continue-level (&body body)
  `(jump-level loop-continue
     ,@body))

(define-special-builtin (builtin-break "break") (&optional (count "1"))
  (wrap-errors
    (let ((count (parse-integer count :junk-allowed nil)))
      (unless (plusp count)
        (error "Count must be positive"))
      (signal 'loop-break :count count))
    (error "No loops detected")))

(define-special-builtin (builtin-continue "continue") (&optional (count "1"))
  (wrap-errors
    (let ((count (parse-integer count :junk-allowed nil)))
      (unless (plusp count)
        (error "Count must be positive"))
      (signal 'loop-continue :count count))
    (error "No loops detected")))

(defmacro shell-while (condition &body body)
  "As long as `condition' returns a truthy exit status, repeatedly
evaluate `body'."
  (let ((result (gensym "RESULT")))
    `(break-level
       (let ((,result (truthy-exit-info)))
         (loop :while (exit-info-true-p ,condition) :do
            (setf ,result (continue-level ,@body)))
         ,result))))

(defmacro shell-for ((variable word-seq) &body body)
  "Evaluate `body' with shell environment variable named `variable'
bound to each string in `word-seq'.

`variable' should be a string or a form that produces a string.  It is
evaluated once.

`word-seq' should be an object that can be iterated using `iterator'.
The values produced by the iterator should be strings."
  (let ((result (gensym "RESULT"))
        (value (gensym "VALUE"))
        (var (gensym "VAR")))
    `(break-level
       (let ((,result (truthy-exit-info))
             (,var ,variable))
         (do-iterator (,value (iterator ,word-seq))
           (setf (env ,var) ,value)
           (setf ,result (continue-level ,@body)))
         ,result))))

(defun %run (arguments modify-environment)
  (if arguments
      (apply 'invoke-command (lookup-command (car arguments)) modify-environment arguments)
      (with-fd-scope ()
        (funcall modify-environment))))

(defmacro shell-run (argument-list &key environment-changes fd-changes)
  "Run a shell command.

This function is not documented in detail because it is likely to get
changed."
  (let ((modify-environment (gensym "MODIFY-ENVIRONMENT"))
        (fd (gensym "FD"))
        (value (gensym "VALUE"))
        (exit-info (gensym "EXIT-INFO"))
        (result (gensym "RESULT")))
    `(labels
         ((,modify-environment ()
            ,@(loop :for redirect :in fd-changes :collect
                 (destructuring-bind (virtual-fd physical-fd-form)
                     redirect
                   `(receive-ref-counted-fd (,fd ,physical-fd-form)
                      (set-fd-binding ,virtual-fd ,fd))))
            (let ((,result (truthy-exit-info)))
              ,@(loop :for assign :in environment-changes :collect
                   (destructuring-bind (env-var value-form) assign
                     `(multiple-value-bind (,value ,exit-info) ,value-form
                        (setf (env ,env-var) (or ,value ""))
                        (when ,exit-info
                          (setf ,result ,exit-info)))))
              ,result)))
       (declare (dynamic-extent #',modify-environment))
       (%run ,argument-list #',modify-environment))))
