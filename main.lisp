(in-package :shcl)

(optimization-settings)

#+sbcl
(defclass echo (sb-gray:fundamental-character-output-stream)
    ())

#+sbcl
(defmethod sb-gray:stream-write-char ((s echo) char)
  (format *standard-output* "CHAR: ~W~%" char)
  char)

#+sbcl
(defparameter *echo-characters* nil)

(defun debug-char-stream (stream)
  (let* ((sbcl #+sbcl *echo-characters*))
    (if sbcl
        (make-echo-stream stream (make-instance 'echo))
        stream)))

(defun main-token-iterator (stream form-queue)
  (bake-tokens
   (map-iterator (token-iterator stream)
                 (lambda (x)
                   (debug-log 'status "TOKEN: ~A~%" x)
                   x)
                 :type 'token-iterator)
   form-queue))

(defun restartable-command-iterator (raw-stream form-queue)
  (let* ((stream (debug-char-stream raw-stream))
         (tokens (main-token-iterator stream form-queue))
         (commands (command-iterator tokens)))
    (labels
        ((reset-token-iterator ()
           (loop :while (read-char-no-hang raw-stream nil nil))
           (setf stream (debug-char-stream raw-stream)
                 tokens (main-token-iterator stream form-queue)
                 commands (command-iterator tokens))))
      (if (interactive-stream-p raw-stream)
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
                   (go start)))))
          commands))))

(defun main ()
  (observe-revival)
  (enable-shell-splice-syntax)
  (let* ((form-queue (make-queue))
         (commands (restartable-command-iterator *standard-input* form-queue)))
    (restart-case
        (do-iterator (tree commands)
          (loop
             (multiple-value-bind (form valid) (dequeue-no-block form-queue)
               (unless valid
                 (return))
               (debug-log 'status "EVAL ~A" form)
               (eval form)))
          (debug-log 'status "TREE: ~A~%" tree)
          (restart-case
              (let ((result (evaluate tree)))
                (declare (ignorable result))
                (debug-log 'status "RESULT ~A" result))
            (skip ())))
      (die () (sb-ext:exit :code 1)))))
