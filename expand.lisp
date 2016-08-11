(in-package :shcl.expand)

(optimization-settings)

(defstruct (string-fragment
             (:constructor %make-string-fragment))
  string
  quoted
  literal)

(defun make-string-fragment (string &key quoted literal)
  (%make-string-fragment :string string :quoted quoted :literal literal))

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

(defgeneric to-string (thing))
(defmethod to-string ((thing string))
  thing)
(defmethod to-string ((thing simple-word))
  (simple-word-text thing))
(defmethod to-string ((thing token))
  nil)

(defparameter *aliases* (fset:empty-map))

(defstruct alias
  words
  continue-expansion)

(defun set-alias (name words &key continue-expansion)
  (setf (fset:lookup *aliases* name)
        (make-alias :words (fset:image #'to-string (fset:convert 'fset:seq words))
                    :continue-expansion continue-expansion))
  nil)

(defun unalias (name)
  (setf *aliases* (fset:less *aliases* name)))

(defun expand-aliases (tokens)
  (unless (typep tokens 'fset:seq)
    (setf tokens (fset:convert 'fset:seq tokens)))

  (let* ((remaining tokens)
         (*aliases* *aliases*))
    (labels
        ((finish ()
           (return-from expand-aliases remaining))
         (alias-for (string)
           (unless string
             (return-from alias-for))
           (fset:lookup *aliases* string))
         (next-word ()
           (to-string (fset:first remaining))))
      (loop
         (when (zerop (fset:size remaining))
           (finish))

         (let* ((next-word (to-string (next-word)))
                (alias (alias-for next-word)))
           (unless alias
             (finish))

           (setf *aliases* (fset:less *aliases* next-word))

           (let ((less-first (fset:less-first remaining)))
             (when (alias-continue-expansion alias)
               (setf less-first (expand-aliases less-first)))
             (setf remaining (fset:concat (alias-words alias) less-first))))))))

(defun expansion-for-word (word &rest args &key expand-aliases expand-pathname (split-fields t))
  (declare (ignore expand-aliases expand-pathname))
  (let ((result (apply 'expansion-for-words (fset:seq word) args)))
    (cond
      (split-fields
       result)
      (t
       (assert (equal 1 (fset:size result)))
       (fset:first result)))))

(defun expansion-for-words (things &key expand-aliases expand-pathname (split-fields t))
  (declare (ignore expand-pathname))
  (setf things (fset:convert 'fset:seq things))
  (when (equal 0 (fset:size things))
    (return-from expansion-for-words #()))

  (let* ((*split-fields* split-fields)
         (result (fset:empty-seq))
         (seqs (if expand-aliases
                   (expand-aliases things)
                   things))
         next-word)

    (setf seqs (fset:image #'expand seqs))
    (labels
        ((observe (fragment)
           (unless next-word
             (setf next-word (fset:empty-seq)))
           (fset:push-last next-word fragment))
         (boundary (value)
           (when (and (not next-word) (soft-word-boundary-p value))
             (return-from boundary))
           (when (not next-word)
             (observe (make-string-fragment "")))

           (fset:push-last result next-word)
           (setf next-word nil)))
      (fset:do-seq (sub-seq seqs)
        (fset:do-seq (fragment sub-seq)
          (if (word-boundary-p fragment)
              (boundary fragment)
              (observe fragment)))
        (boundary +soft-word-boundary+)))

    (fset:image #'concat-fragments result)))

(defun expand-pathname (fragments)
  (declare (ignore fragments)))

(defun concat-fragments (fragments)
  (let ((stream (make-string-output-stream)))
    (fset:do-seq (f fragments)
      (write-string (string-fragment-string f) stream))
    (get-output-stream-string stream)))

(defgeneric expand (thing))

(defmethod expand ((thing string))
  (fset:seq (make-string-fragment thing :literal t)))

(defmethod expand ((thing simple-word))
  (fset:seq (make-string-fragment (simple-word-text thing) :literal t)))

(defmethod expand ((thing compound-word))
  (let* ((parts (compound-word-parts thing))
         (result (fset:empty-seq)))
    (labels
        ((ingest (seq)
           (fset:appendf result seq)))
      (when (zerop (length parts))
        (return-from expand (fset:empty-seq)))

      (ingest (expand (aref parts 0)))

      (loop :for index :from 1 :below (length parts) :do
         (ingest (expand (aref parts index)))))
    result))

;; This is not meant to be used to expand the assignment statements at
;; the start of a command.  Those expand differently (in particular,
;; field splitting doesn't occur).
(defmethod expand ((thing assignment-word))
  (with-accessors ((name assignment-word-name) (value assignment-word-value-word)) thing
    (let ((value-expanded (expand value))
          (name-expanded (expand name))
          (result (fset:seq (make-string-fragment "="))))
      (fset:prependf result name-expanded)
      (fset:appendf result value-expanded)
      result)))

(defmethod expand ((thing literal-token))
  (fset:seq (make-string-fragment (literal-token-string thing) :literal t)))

(defmethod expand ((thing single-quote))
  (fset:seq (make-string-fragment (single-quote-contents thing) :quoted t)))

(defmethod expand ((thing double-quote))
  (let ((*split-fields* nil))
    (let* ((parts (double-quote-parts thing))
           (result (make-string-output-stream)))
      (loop :for part :across parts :do
         (let ((expansion (expand part)))
           (fset:do-seq (sub-part expansion)
             (unless (word-boundary-p sub-part)
               (write-string (string-fragment-string sub-part) result)))))
      (fset:seq (make-string-fragment (get-output-stream-string result) :quoted t)))))

(defmethod expand ((thing command-word))
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
        (fset:seq (make-string-fragment value)))))

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
               (setf result (fset:with-last result (make-string-fragment (get-output-stream-string current-word)))))))
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
