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

(defun debug-token-iterator (stream)
  (map-iterator (token-iterator stream)
                (lambda (x)
                  (format *standard-output* "TOKEN: ~A~%" x)
                  x)
                :type 'token-iterator))

(defun restartable-command-iterator (raw-stream)
  (let* ((stream (debug-char-stream raw-stream))
         (tokens (debug-token-iterator stream))
         (commands (command-iterator tokens)))
    (labels
        ((reset-token-iterator ()
           (loop :while (read-char-no-hang raw-stream nil nil))
           (setf stream (debug-char-stream raw-stream)
                 tokens (debug-token-iterator stream)
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
  (let* ((commands (restartable-command-iterator *standard-input*)))
    (restart-case
        (do-iterator (tree commands)
          (format *standard-output* "TREE: ~A~%" tree)
          (restart-case
              (evaluate tree)
            (skip ())))
      (die () (sb-ext:exit :code 1)))))
