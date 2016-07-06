(in-package :shcl.evaluate)

(optimization-settings)

(define-condition not-implemented (error)
  ((message
    :initarg :message
    :initform ""
    :accessor not-implemented-message
    :type string)))

(defun separator-par-p (separator)
  (check-type separator separator)
  (with-slots (separator-op) separator
    (when (slot-boundp separator 'separator-op)
      (typep separator-op 'par))))

(defparameter *fd-bindings*
  (let ((table (make-hash-table)))
    (setf (gethash 0 table) 0
          (gethash 1 table) 1
          (gethash 2 table) 2)
    table))

(defmacro shadow-fd-bindings (&body body)
  `(let ((*fd-bindings* (copy-hash-table *fd-bindings*)))
     ,@body))

(defun bind-fd (fd description)
  (setf (gethash fd *fd-bindings*) description))

(defun get-fd (fd)
  (gethash fd *fd-bindings*))

(defgeneric handle-redirect (redirect &optional fd-override))

(defmethod handle-redirect ((r io-redirect) &optional fd-override)
  (when fd-override
    (error "You did bad.  This shouldn't be set here."))

  (assert (slot-boundp r 'io-number))

  (labels
      ((to-int (io-number)
         (parse-integer (token-value io-number))))
    (with-slots (io-number io-file io-here) r
      (cond
        ((slot-boundp r 'io-here)
         (error 'not-implemented :message "Here-documents are not implemented"))

        ((slot-boundp r 'io-file)
         (handle-redirect io-file (to-int io-number)))

        (t
         (error "Impossible"))))))

(defmethod handle-redirect ((r io-file) &optional fd-override)
  (labels
      ((to-int (filename)
         (let* ((fd-word (slot-value 'a-word filename))
                (fd-string (token-value fd-word))
                (fd (parse-integer fd-string)))
           fd))
       (fd (default) (or fd-override default)))
    (with-slots (filename) r
      (cond
        ((slot-boundp r 'less)
         (bind-fd (fd 0) r))

        ((slot-boundp r 'lessand)
         (bind-fd (fd 0) (get-fd (to-int r))))

        ((slot-boundp r 'great)
         (bind-fd (fd 1) r))

        ((slot-boundp r 'greatand)
         (bind-fd (fd 1) (get-fd (to-int r))))

        ((slot-boundp r 'dgreat)
         (bind-fd (fd 1) r))

        ((slot-boundp r 'lessgreat)
         (bind-fd (fd 0) r))

        ((slot-boundp r 'clobber)
         (bind-fd (fd 1) r))))))

(defmethod handle-redirect ((r io-here) &optional fd-override)
  (declare (ignore fd-override))
  (error 'not-implemented :message "Here-documents are not implemented"))

(defmethod handle-redirect ((r redirect-list) &optional fd-override)
  (when fd-override
    (error "You did bad.  This shouldn't be set here."))

  (with-slots (io-redirect redirect-list-tail) r
    (handle-redirect io-redirect)
    (when redirect-list-tail
      (handle-redirect redirect-list-tail))))

(defmethod handle-redirect ((r redirect-list-tail) &optional fd-override)
  (when fd-override
    (error "You did bad.  This shouldn't be set here."))

  (with-slots (io-redirect redirect-list-tail) r
    (handle-redirect io-redirect)
    (when redirect-list-tail
      (handle-redirect redirect-list-tail))))

(defgeneric evaluate (syntax-tree))

(defmethod evaluate (sy)
  (error 'not-implemented :message (format nil "Cannot eval ~A" (class-name (class-of sy)))))

(defmethod evaluate ((sy complete-command))
  (with-slots (newline-list complete-command command-list command-separator) sy
    (cond
      ((and (slot-boundp sy 'newline-list)
            complete-command)
       (evaluate complete-command))

      ((slot-boundp sy 'newline-list)
       (return-from evaluate nil))

      (t
       (let ((no-wait (typep command-separator 'par)))
         (when no-wait
           (error 'not-implemented :message "& not implemented"))

         (evaluate command-list))))))

(defun evaluate-command-list (sy)
  (with-slots (and-or command-list-tail) sy
    (let ((no-wait (and command-list-tail
                        (typep (slot-value command-list-tail 'separator-op) 'par))))
      (when no-wait
        (error 'not-implemented :message "& not implemented"))

      (evaluate and-or)
      (when command-list-tail
        (evaluate command-list-tail)))))

(defmethod evaluate ((sy command-list))
  (evaluate-command-list sy))
(defmethod evaluate ((sy command-list-tail))
  (evaluate-command-list sy))

(defun evaluate-and-or (sy)
  (with-slots (pipeline and-or-tail) sy
    (evaluate pipeline)
    (when and-or-tail
      (error 'not-implemented :message "&& and || are not implemented"))))

(defmethod evaluate ((sy and-or))
  (evaluate-and-or sy))
(defmethod evaluate ((sy and-or-tail))
  (evaluate-and-or sy))

(defmethod evaluate ((sy pipeline))
  (with-slots (bang pipe-sequence) sy
    (error 'not-implemented :message "! not implemented")))

(defun evaluate-pipe-sequence (sy)
  (with-slots (command pipe-sequence-tail) sy
    (when pipe-sequence-tail
      (error 'not-implemented :message "| not implemented"))
    (evaluate command)))

(defmethod evaluate ((sy pipe-sequence))
  (evaluate-pipe-sequence sy))
(defmethod evaluate ((sy pipe-sequence-tail))
  (evaluate-pipe-sequence sy))

(defmethod evaluate ((sy command))
  (with-slots (compound-command redirect-list) sy
    (shadow-fd-bindings
      (handle-redirect redirect-list)
      (evaluate compound-command))))

(defmethod evaluate ((sy subshell))
  (error 'not-implemented :message "Subshells not implemented"))

(defmethod evaluate ((sy compound-list))
  (with-slots (newline-list term separator) sy
    (when (and (slot-boundp sy 'separator)
               (separator-par-p separator))
      (error 'not-implemented :message "& not implemented"))

    (evaluate term)))

(defun evaluate-term (sy)
  (with-slots (and-or term-tail) sy
    (when (and term-tail
               (separator-par-p (slot-value term-tail 'separator)))
      (error 'not-implemented :message "& not implemented"))

    (evaluate and-or)
    (evaluate term-tail)))

(defmethod evaluate ((sy term))
  (evaluate-term sy))
(defmethod evaluate ((sy term-tail))
  (evaluate-term sy))

(defun cmd-prefix-parts (prefix)
  (with-slots (io-redirect assignment-word cmd-prefix-tail) prefix
    (multiple-value-bind (assignments redirects)
        (when cmd-prefix-tail
          (cmd-prefix-parts cmd-prefix-tail))

      (when (slot-boundp prefix 'io-redirect)
        (push io-redirect redirects))

      (when (slot-boundp prefix 'assignment-word)
        (push assignment-word assignments))

      (values assignments redirects))))

(defun cmd-suffix-parts (suffix)
  (with-slots (io-redirect a-word cmd-suffix-tail) suffix
    (multiple-value-bind (arguments redirects)
        (when cmd-suffix-tail
          (cmd-suffix-parts cmd-suffix-tail))

      (when (slot-boundp suffix 'io-redirect)
        (push io-redirect redirects))

      (when (slot-boundp suffix 'a-word)
        (push a-word arguments))

      (values arguments redirects))))

(defun simple-command-parts (sy)
  (let (assignments
        arguments
        redirects)
    (with-slots (cmd-prefix cmd-word cmd-name cmd-suffix) sy
      (when (slot-boundp sy 'cmd-prefix)
        (multiple-value-bind (prefix-assignments prefix-redirects) (cmd-prefix-parts cmd-prefix)
          (dolist (a prefix-assignments)
            (push a assignments))
          (dolist (r prefix-redirects)
            (push r redirects))))

      (when (slot-boundp sy 'cmd-name)
        (push cmd-name arguments))

      (when (slot-boundp sy 'cmd-word)
        (push cmd-word arguments))

      (when (slot-boundp sy 'cmd-suffix)
        (multiple-value-bind (suffix-arguments suffix-redirects) (cmd-suffix-parts cmd-suffix)
          (dolist (a suffix-arguments)
            (push a arguments))
          (dolist (r suffix-redirects)
            (push r redirects))))

      (values (nreverse assignments) (nreverse arguments) (nreverse redirects)))))

(defmethod evaluate ((sy simple-command))
  (with-slots (cmd-prefix cmd-word cmd-name cmd-suffix) sy
    (multiple-value-bind (assignments arguments redirects) (simple-command-parts sy)
      (format *standard-output* "EXEC: ~A ~A ~A~%" assignments arguments redirects)
      (shadow-fd-bindings
        (dolist (r redirects)
          (handle-redirect r))))))
