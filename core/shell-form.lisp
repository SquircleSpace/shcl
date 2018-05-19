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
  (:import-from :shcl/core/command #:invoke-command #:lookup-command)
  (:import-from :bordeaux-threads)
  (:import-from :lisp-namespace #:define-namespace)
  (:import-from :alexandria #:parse-body)
  (:export #:pipeline #:shell #:progn #:! #:or #:and #:& #:lisp #:subshell
           #:when #:unless #:if #:while #:for #:loop-break #:loop-continue
           #:run))
(in-package :shcl/core/shell-form)

(optimization-settings)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-over-shell (form)
    `(lambda () (shell ,form))))

(deftype macro-type ()
  '(function (cons t) (values t &optional)))

(define-namespace shell-form-translator
    macro-type nil)

(defmethod translate-shell-form ((form cons) environment)
  (funcall (symbol-shell-form-translator (car form)) form environment))

(defmacro define-shell-form-translator (&whole whole name lambda-list &body body)
  (let ((macro-name (gensym (symbol-name name))))
    (multiple-value-bind (real-body declarations documentation)
        (parse-body body :documentation t :whole whole)
      `(progn
         ;; Leverage defmacro to get macro lambda-list support
         (defmacro ,macro-name ,lambda-list
           ,@declarations
           (block ,name
             ,@real-body))
         (setf (symbol-shell-form-translator ',name) (macro-function ',macro-name))
         (setf (documentation ',name 'shell-form-translator) ,documentation)
         ',name))))

(defmacro shell (&body body &environment env)
  (cond
    ((null body)
     `(falsey-exit-info))

    ((null (cdr body))
     (translate-shell-form (car body) env))

    (t
     (apply 'progn-concatenate
            (loop :for form :in body :collect
               (translate-shell-form form env))))))

(define-shell-form-translator shell (&whole whole &body body &environment env)
  "Evaluate each form in `body' as a shell form.

This is the shell form equivalent of `progn'."
  (declare (ignore body))
  (macroexpand-1 whole env))

(define-shell-form-translator progn (&body body &environment env)
  (macroexpand-1 `(shell ,@body) env))

(define-shell-form-translator lisp (&body body)
  (apply 'progn-concatenate body))

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

(define-shell-form-translator pipeline (&body body)
  `(pipeline-fn
    ,@(mapcar 'lambda-over-shell body)))

(define-shell-form-translator ! (&body body)
  `(invert-exit-info (shell ,@body)))

(define-shell-form-translator and (&body body)
  (unless body
    (return-from and
      '(truthy-exit-info)))

  (unless (cdr body)
    (return-from and
      `(shell ,(car body))))

  (let ((and (gensym "AND"))
        (result (gensym "RESULT")))
    `(block ,and
       ,@(loop :for tail :on body :collect
            (if (cdr tail)
                `(let ((,result (shell ,(car tail))))
                   (unless (exit-info-true-p ,result)
                     (return-from ,and ,result)))
                `(shell ,(car tail)))))))

(define-shell-form-translator or (&body body)
  (unless body
    (return-from or
      '(falsey-exit-info)))

  (unless (cdr body)
    (return-from or
      `(shell ,(car body))))

  (let ((or (gensym "OR"))
        (result (gensym "RESULT")))
    `(block ,or
       ,@(loop :for tail :on body :collect
            (if (cdr tail)
                `(let ((,result (shell ,(car tail))))
                   (when (exit-info-true-p ,result)
                     (return-from ,or ,result)))
                `(shell ,(car tail)))))))

(define-shell-form-translator & (&body body)
  (declare (ignore body))
  (error 'not-implemented :feature "Background jobs"))

(define-shell-form-translator subshell (&body body)
  `(with-subshell
     (shell ,@body)))

(define-shell-form-translator if (condition then &optional (else nil else-p))
  (let ((value (gensym "VALUE")))
    `(let ((,value (shell ,condition)))
       (if (exit-info-true-p ,value)
           (shell ,then)
           ,(if else-p
                `(shell ,else)
                value)))))

(define-shell-form-translator when (condition &body body)
  `(shell
     (if ,condition
         (shell ,@body))))

(define-shell-form-translator unless (condition &body body)
  `(shell
     (if (! ,condition)
         (shell ,@body))))

(define-shell-form-translator fd-bind* (bindings &body body)
  `(fd-bind*
       ,bindings
     (shell ,@body)))

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
  ())

(define-condition loop-continue (loop-jump)
  ())

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

(define-shell-form-translator while (condition &body body)
  (let ((result (gensym "RESULT")))
    `(break-level
       (let ((,result (truthy-exit-info)))
         (loop :while (exit-info-true-p (shell ,condition)) :do
            (setf ,result (continue-level (shell ,@body))))
         ,result))))

(define-shell-form-translator for ((variable word-seq) &body body)
  (let ((result (gensym "RESULT"))
        (value (gensym "VALUE")))
    `(break-level
       (let ((,result (truthy-exit-info)))
         (do-iterator (,value (iterator ,word-seq))
           (setf (env ,variable) ,value)
           (setf ,result (continue-level (shell ,@body))))
         ,result))))

(defun %run (arguments modify-environment)
  (if arguments
      (apply 'invoke-command (lookup-command (car arguments)) modify-environment arguments)
      (with-fd-scope ()
        (funcall modify-environment))))

(define-shell-form-translator run (argument-list &key environment-changes fd-changes)
  (let ((modify-environment (gensym "MODIFY-ENVIRONMENT"))
        (fd (gensym "FD")))
    `(labels
         ((,modify-environment ()
            ,@(loop :for redirect :in fd-changes :collect
                 (destructuring-bind (virtual-fd physical-fd-form)
                     redirect
                   `(receive-ref-counted-fd (,fd ,physical-fd-form)
                      (set-fd-binding ,virtual-fd ,fd))))
            ,@(loop :for assign :in environment-changes :collect
                 (destructuring-bind (env-var value) assign
                   `(setf (env ,env-var) ,value)))))
       (declare (dynamic-extent #',modify-environment))
       (%run ,argument-list #',modify-environment))))
