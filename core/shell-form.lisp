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
  (:import-from :shcl/core/shell-environment
   #:destroy-preserved-shell-environment #:preserve-shell-environment
   #:with-restored-shell-environment)
  (:import-from :shcl/core/exit-info
   #:exit-info-true-p #:exit-info-false-p)
  (:import-from :shcl/core/fd-table
   #:pipe-retained #:fd-release #:with-fd-scope #:bind-fd)
  (:import-from :bordeaux-threads)
  (:export #:pipeline #:pipeline-fn))
(in-package :shcl/core/shell-form)

(optimization-settings)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambdaify (form)
    `(lambda ()
       ,form)))

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

(defun pipeline-fn (pipeline-fns)
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
                    (bind-fd +pipe-read-fd+ read-end))
                  (when write-end
                    (bind-fd +pipe-write-fd+ write-end))
                  (setf (aref results index) (call-with-parallel-shell (aref pipeline-fns index)))
                  (check-type (aref results index) async-result))))
           (loop :for index :from (1- (length pipeline-fns)) :downto 1 :do
              (multiple-value-bind (read-end write-end) (pipe-retained)
                (unwind-protect
                     (run read-end write-fd index)
                  (when write-fd
                    (fd-release write-fd))
                  (setf write-fd write-end)
                  (fd-release read-end))))

           (assert write-fd)
           (run nil write-fd 0))
      (when write-fd
        (fd-release write-fd)
        (setf write-fd nil)))

    ;; Wait for each job in the pipeline except the last one
    (loop :for index :from 0 :below (1- (length pipeline-fns)) :do
       (async-result-get (aref results index)))

    (async-result-get (aref results (1- (length pipeline-fns))))))

(defmacro pipeline (&body body)
  `(pipeline-fn
    (list ,@(mapcar 'lambdaify body))))
