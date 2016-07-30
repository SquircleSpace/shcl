(in-package :shcl.expand)

(optimization-settings)

(defparameter *expand-variables* nil)
(defparameter *expand-aliases* nil)
(defparameter *expand-commands* nil)
(defparameter *expand-pathname* nil)
(defparameter *split-fields* t)

(defconstant +soft-word-boundary+ '+soft-word-boundary+)
(defconstant +hard-word-boundary+ '+hard-word-boundary+)

(defun soft-word-boundary-p (thing)
  (eq +soft-word-boundary+ thing))

(defun hard-word-boundary-p (thing)
  (eq +hard-word-boundary+ thing))

(defun word-boundary-p (thing)
  (or (soft-word-boundary-p thing)
      (hard-word-boundary-p thing)))

(defun expansion-for (thing &key expand-variables expand-aliases expand-commands expand-pathname)
  (let* ((*expand-variables* expand-variables)
         (*expand-aliases* expand-aliases)
         (*expand-commands* expand-commands)
         (*expand-pathname* expand-pathname)
         (seq (expand thing))
         (result (fset:empty-seq))
         (boundary-clean t)
         next-word)
    (labels
        ((observe (string)
           (unless next-word
             (setf next-word (make-string-output-stream)))
           (write-string string next-word)
           (setf boundary-clean nil))
         (boundary ()
           (setf result (fset:with-last result (get-output-stream-string next-word)))
           (setf next-word nil)
           (setf boundary-clean t)))
      (fset:do-seq (thing seq)
        (debug-log "next is ~A" thing)
        (block next
          (when (and boundary-clean (soft-word-boundary-p thing))
            (return-from next))
          (when (word-boundary-p thing)
            (observe "")
            (boundary)
            (return-from next))
          (observe thing))))
    (when next-word
      (setf result (fset:with-last result (get-output-stream-string next-word))))
    result))

(defgeneric expand (thing))

(defmethod expand ((thing simple-word))
  (fset:seq (simple-word-text thing)))

(defmethod expand ((thing compound-word))
  (let* ((parts (compound-word-parts thing))
         (result (fset:empty-seq)))
    (labels
        ((ingest (seq)
           (fset:appendf result seq)))
      (when (zerop (length parts))
        (return-from expand (fset:empty-seq)))

      (ingest (expand (aref parts 0)))

      (let ((*expand-aliases* nil)
            (*expand-user* nil))
        (loop :for index :from 1 :below (length parts) :do
           (ingest (expand (aref parts index))))))
    result))

(defmethod expand ((thing literal-token))
  (fset:seq (literal-token-string thing)))

(defmethod expand ((thing single-quote))
  (fset:seq (single-quote-contents thing)))

(defmethod expand ((thing double-quote))
  (let ((*expand-aliases* nil)
        (*split-fields* nil)
        (*expand-pathname* nil))
    (let* ((parts (double-quote-parts thing))
           (result (make-string-output-stream)))
      (loop :for part :across parts :do
         (let ((expansion (expand part)))
           (fset:do-seq (sub-part expansion)
             (unless (word-boundary-p sub-part)
               (write-string sub-part result)))))
      (fset:seq (get-output-stream-string result)))))

(defmethod expand ((thing command-word))
  (unless *expand-commands*
    (return-from expand (fset:seq (token-value thing))))

  (error "not implemented"))

(defmethod expand ((thing variable-expansion-word))
  (let* ((variable (variable-expansion-word-variable thing))
         (value
          (cond
            ((or (equal variable "@")
                 (equal variable "*"))
             (error "Not implemented"))

            (t
             (env variable)))))
    (if *split-fields*
        (split value)
        (fset:seq value))))

(defun ifs-parts (ifs)
  (labels ((blank (c) (cl-unicode:has-binary-property c "White_Space"))
           (not-blank (c) (not (blank c))))
    (values
     (remove-if #'not-blank ifs)
     (remove-if #'blank ifs))))

(defun split (string)
  (multiple-value-bind (whitespace non-whitespace) (ifs-parts $ifs)
    (when (and (zerop (length whitespace)) (zerop non-whitespace))
      (return-from split string))

    (let* ((result (fset:empty-seq))           
           (index 0)
           current-word)
      (labels
          ((yum-boundary ()
             (let ((boundary (boundary-p (aref string index)))
                   next)
               (assert boundary)
               (loop :while (and (< (1+ index) (length string))
                                 (setf next (boundary-p (aref string (1+ index))))) :do
                  (when (hard-word-boundary-p next)
                    (when (hard-word-boundary-p boundary)
                      (return-from yum-boundary boundary))
                    (setf boundary next))
                  (incf index))
               boundary))
           (boundary-p (char)
             (cond ((find char non-whitespace)
                    +hard-word-boundary+)
                   ((find char whitespace)
                    +soft-word-boundary+)
                   (t
                    nil)))
           (delimit (boundary)
             (finish)
             (setf result (fset:with-last result boundary))
             (setf current-word nil))
           (observe (char)
             (unless current-word
               (setf current-word (make-string-output-stream)))
             (write-char char current-word))
           (finish ()
             (when current-word
               (setf result (fset:with-last result (get-output-stream-string current-word))))))
        (declare (dynamic-extent #'boundary-p #'delimit))
        (loop
           (block again
             (when (>= index (length string))
               (return))

             (let* ((char (aref string index))
                    (boundary (boundary-p char)))
               (when boundary
                 (setf boundary (yum-boundary))
                 (delimit boundary)
                 (return-from again))

               (observe (aref string index))))

           (incf index))
        (finish)
        result))))
