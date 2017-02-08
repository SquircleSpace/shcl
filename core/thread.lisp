(defpackage :shcl/core/thread
  (:use :common-lisp :shcl/core/utility :bordeaux-threads)
  (:export
   ;; Semaphores
   #:semaphore #:make-semaphore #:semaphore-signal #:semaphore-wait
   #:semaphore-p
   #:queue #:make-queue #:enqueue #:dequeue #:dequeue-no-block #:queue-p))
(in-package :shcl/core/thread)

(optimization-settings)

(defstruct (semaphore
             (:constructor %make-semaphore))
  (count 0)
  (lock (make-lock))
  (cv (make-condition-variable)))

(defun make-semaphore ()
  (%make-semaphore))

(defun semaphore-signal (semaphore)
  (with-accessors ((count semaphore-count)
                   (lock semaphore-lock) (cv semaphore-cv))
      semaphore
    (with-lock-held (lock)
      (incf count)
      (condition-notify cv)
      nil)))

(defun semaphore-wait (semaphore)
  (with-accessors ((count semaphore-count)
                   (lock semaphore-lock) (cv semaphore-cv))
      semaphore
    (with-lock-held (lock)
      (loop :while (equal 0 count) :do (condition-wait cv lock))
      (decf count)
      (condition-notify cv)
      nil)))

(defstruct (queue
             (:constructor %make-queue))
  front
  back
  (lock (make-lock))
  (cv (make-condition-variable)))

(defun make-queue ()
  (%make-queue))

(defun enqueue (item queue)
  (with-accessors ((front queue-front) (back queue-back)
                   (lock queue-lock) (cv queue-cv)) queue
    (with-lock-held (lock)
      (cond
        ((null front)
         (setf front (cons item nil)
               back front))

        (t
         (setf (cdr back) (cons item nil)
               back (cdr back))))
      (condition-notify cv)
      nil)))

(defun %dequeue (queue &key (wait t))
  (with-accessors ((front queue-front) (back queue-back)
                   (lock queue-lock) (cv queue-cv)) queue
    (with-lock-held (lock)
      (when (and (not wait) (null front))
        (return-from %dequeue (values nil nil)))
      (loop :while (null front) :do (condition-wait cv lock))
      (let ((item (car front)))
        (if (eq front back)
            (setf front nil
                  back nil)
            (setf front (cdr front)))
        (values item t)))))

(defun dequeue (queue)
  (nth-value 0 (%dequeue queue)))

(defun dequeue-no-block (queue &optional default)
  (multiple-value-bind (value valid) (%dequeue queue :wait nil)
    (if valid
        value
        default)))
