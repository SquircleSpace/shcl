(defpackage :shcl/core/thread
  (:use :common-lisp :shcl/core/utility :bordeaux-threads)
  (:export
   ;; Semaphores
   #:semaphore #:make-semaphore #:semaphore-signal #:semaphore-wait
   #:semaphore-p
   ;; Queues
   #:queue #:make-queue #:enqueue #:dequeue #:dequeue-no-block #:queue-p))
(in-package :shcl/core/thread)

(optimization-settings)

(defstruct (semaphore
             (:constructor %make-semaphore))
  "This struct represents the standard semaphore synchronization
primitive."
  (count 0)
  (lock (make-lock))
  (cv (make-condition-variable)))

(defun make-semaphore ()
  "Create a new semaphore."
  (%make-semaphore))

(defun semaphore-signal (semaphore)
  "Increment the given semaphore and allow a thread blocked in
`semaphore-wait' to continue."
  (with-accessors ((count semaphore-count)
                   (lock semaphore-lock) (cv semaphore-cv))
      semaphore
    (with-lock-held (lock)
      (incf count)
      (condition-notify cv)
      nil)))

(defun semaphore-wait (semaphore)
  "Either decrement the given semaphore immediately or wait until the
semaphore is incremented by `semaphore-signal'."
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
  "This struct is a standard queue data structure."
  front
  back
  (lock (make-lock))
  (cv (make-condition-variable)))

(defun make-queue ()
  "Create a new queue."
  (%make-queue))

(defun enqueue (item queue)
  "Add an item to the given queue."
  (with-accessors
        ((front queue-front) (back queue-back)
         (lock queue-lock) (cv queue-cv))
      queue
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
  "The brains of `dequeue' and `dequeue-no-block'."
  (with-accessors
        ((front queue-front) (back queue-back)
         (lock queue-lock) (cv queue-cv))
      queue
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
  "Return and remove the item at the front of the queue.

This function will block if there are no items in the queue.  See
`dequeue-no-block'."
  (nth-value 0 (%dequeue queue)))

(defun dequeue-no-block (queue &optional default)
  "Return and remove the item at the front of the queue.

This function will return immediately if there aren't any items in the
queue."
  (multiple-value-bind (value valid) (%dequeue queue :wait nil)
    (if valid
        value
        default)))
