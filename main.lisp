(in-package :shcl)

(optimization-settings)

(defun main ()
  (let ((command-iterator (command-iterator (token-iterator *standard-input*))))
    (do-iterator (tree command-iterator)
      (format *standard-output* "~A~%" tree))))
