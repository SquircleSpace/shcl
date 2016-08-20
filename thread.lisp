(in-package :shcl.thread)

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

(defstruct (queue-thread
             (:constructor %make-queue-thread)
             (:include queue))
  (stop-value (gensym "STOP"))
  wrapper
  thread)

(defun make-queue-thread (&key name wrapper)
  (let* ((queue (%make-queue-thread :wrapper wrapper))
         (thread (make-thread (lambda () (work-loop queue)) :name name)))
    (setf (queue-thread-thread queue) thread)
    queue))

(defun close-queue-thread (queue)
  (enqueue (queue-thread-stop-value queue) queue))

(defun kill-queue-thread (queue)
  (interrupt-thread (queue-thread-thread queue)
                    (lambda () (throw (queue-thread-stop-value queue) nil))))

(defun work-loop (queue)
  (let ((stop-value (queue-thread-stop-value queue))
        (wrapper (or (queue-thread-wrapper queue)
                     #'funcall))
        job)
    (catch stop-value
      (loop
         (setf job (dequeue queue))
         (when (eq job stop-value)
           (return))
         (funcall wrapper job)))))

(defun async-f (thread-queue function)
  (enqueue function thread-queue))

(defmacro async ((thread-queue) &body body)
  `(async-f ,thread-queue (lambda () ,@body)))

(defun sync-f (thread-queue function)
  (let ((semaphore-1 (make-semaphore))
        (semaphore-2 (make-semaphore)))
    (labels ((semaphore-junk ()
               (semaphore-signal semaphore-1)
               (semaphore-wait semaphore-2)))
      (async-f thread-queue #'semaphore-junk)
      (semaphore-wait semaphore-1)
      (unwind-protect (funcall function)
        (semaphore-signal semaphore-2)))))

(defmacro sync ((thread-queue) &body body)
  `(sync-f ,thread-queue (lambda () ,@body)))
