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
           #:shell-unless #:shell-if #:shell-while #:shell-for #:loop-break
           #:loop-continue #:shell-run))
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
  `(pipeline-fn
    ,@(mapcar (lambda (form)
                `(lambda ()
                   ,form))
              body)))

(defmacro shell-not (&body body)
  `(invert-exit-info (progn ,@body)))

(defmacro shell-and (&body body)
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
  (declare (ignore body))
  (error 'not-implemented :feature "Background jobs"))

(defmacro shell-if (condition then &optional (else nil else-p))
  (let ((value (gensym "VALUE")))
    `(let ((,value ,condition))
       (if (exit-info-true-p ,value)
           ,then
           ,(if else-p
                else
                value)))))

(defmacro shell-when (condition &body body)
  `(shell-if ,condition
             ,@body))

(defmacro shell-unless (condition &body body)
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
  (let ((result (gensym "RESULT")))
    `(break-level
       (let ((,result (truthy-exit-info)))
         (loop :while (exit-info-true-p ,condition) :do
            (setf ,result (continue-level ,@body)))
         ,result))))

(defmacro shell-for ((variable word-seq) &body body)
  (let ((result (gensym "RESULT"))
        (value (gensym "VALUE")))
    `(break-level
       (let ((,result (truthy-exit-info)))
         (do-iterator (,value (iterator ,word-seq))
           (setf (env ,variable) ,value)
           (setf ,result (continue-level ,@body)))
         ,result))))

(defun %run (arguments modify-environment)
  (if arguments
      (apply 'invoke-command (lookup-command (car arguments)) modify-environment arguments)
      (with-fd-scope ()
        (funcall modify-environment))))

(defmacro shell-run (argument-list &key environment-changes fd-changes)
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
