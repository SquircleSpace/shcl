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

(defun main ()
  (let* ((stream (debug-char-stream *standard-input*))
         (tokens (debug-token-iterator stream))
         (commands (command-iterator tokens)))
    (restart-case
        (do-iterator (tree commands)
          (format *standard-output* "TREE: ~A~%" tree)
          (evaluate tree))
      (die () (sb-ext:exit :code 1)))))
