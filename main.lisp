(in-package :shcl)

(optimization-settings)

(defclass echo (trivial-gray-streams:fundamental-character-output-stream)
    ())

(defmethod trivial-gray-streams:stream-write-char ((s echo) char)
  (format *standard-output* "CHAR: ~W~%" char)
  char)

(defparameter *echo-characters* nil)

(defun debug-char-stream (stream)
  (if *echo-characters*
      (make-echo-stream stream (make-instance 'echo))
      stream))

(defun main-token-iterator (stream form-queue)
  (lookahead-iterator-wrapper
   (bake-tokens
    (map-iterator (token-iterator stream)
                  (lambda (x)
                    (debug-log 'status "TOKEN: ~A~%" x)
                    x))
    form-queue)))

(defun display-prompt (input-stream output-stream)
  (when (and (interactive-stream-p input-stream)
             (not (listen input-stream)))
    (format output-stream "shcl> ")
    (finish-output output-stream)))

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

(defparameter *enable-lisp-splice* nil)

(defparameter *help* nil)

(defparameter *options*
  '((*enable-lisp-splice* nil "Extend shell language to support splicing lisp expressions")
    (*help* nil "Show list of options")))

(defmacro with-options ((argv &key (options '*options*)) &body body)
  (let ((vars (gensym "VARS"))
        (values (gensym "VALUES"))
        (command (gensym "COMMAND"))
        (kwargs (gensym "KWARGS"))
        (rest (gensym "REST")))
    `(multiple-value-bind (,vars ,values ,command ,kwargs ,rest) (cl-cli:parse-cli ,argv ,options)
       (declare (ignore ,command ,kwargs ,rest))
       (progv ,vars ,values
         ,@body))))

(defun main ()
  (observe-revival)
  (with-options ((uiop:raw-command-line-arguments))
    (when *help*
      (cl-cli:help *options* nil :prog-name "shcl")
      (return-from main))

    (when *enable-lisp-splice*
      (enable-shell-splice-syntax))

    (let* ((form-queue (make-queue))
           (commands (restartable-command-iterator *standard-input* form-queue)))
      (display-prompt *standard-input* *standard-output*)
      (restart-case
          (do-iterator (tree commands)
            (loop
               (let* ((stop '#:stop)
                      (form (dequeue-no-block form-queue stop)))
                 (when (eq stop form)
                   (return))
                 (debug-log 'status "EVAL ~A" form)
                 (eval form)))
            (debug-log 'status "TREE: ~A~%" tree)
            (restart-case
                (let ((result (evaluate tree)))
                  (declare (ignorable result))
                  (debug-log 'status "RESULT ~A" result))
              (skip ()))
            (display-prompt *standard-input* *standard-output*))
        (die () (exit 1))))))
