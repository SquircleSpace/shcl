(defpackage :shcl/core/expand
  (:use
   :common-lisp :shcl/core/utility :shcl/core/lexer :shcl/core/environment
   :shcl/core/working-directory)
  (:import-from :fset)
  (:import-from :cl-ppcre #:create-scanner #:scan)
  (:import-from :cffi #:foreign-string-to-lisp #:foreign-slot-pointer #:null-pointer-p)
  (:import-from :shcl/core/posix #:syscall-error)
  (:import-from :shcl/core/posix-types #:dirent #:d-name)
  (:import-from :shcl/core/fd-table #:with-dir-ptr-for-fd #:openat-retained #:fd-release)
  (:export
   #:expansion-for-words #:set-alias #:unalias #:expand #:make-string-fragment
   #:word-boundary #:*split-fields* #:split))
(in-package :shcl/core/expand)

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

(defun word-boundary ()
  +soft-word-boundary+)

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

(defun expansion-for-words (things &key expand-aliases expand-pathname (split-fields t))
  "Perform expansion on a sequence of tokens."
  (setf things (fset:convert 'fset:seq things))
  (when (equal 0 (fset:size things))
    (return-from expansion-for-words (fset:empty-seq)))

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

    (unless expand-pathname
      (return-from expansion-for-words
        (fset:image #'concat-fragments result)))

    (let ((pathname-expansion-results (fset:empty-seq)))
      (fset:do-seq (fragments result)
        (fset:appendf pathname-expansion-results (expand-pathname fragments)))
      pathname-expansion-results)))

(defstruct wild-path
  file-name
  (directories (make-extensible-vector)))

(defun wild-path-wild-p (wild-path)
  (with-accessors
        ((file-name wild-path-file-name) (directories wild-path-directories))
      wild-path
    (or (and file-name (not (stringp file-name)))
        (find-if-not 'stringp directories))))

(defun make-wild-path-from-fragments (fragments)
  (let ((path-part (make-extensible-vector))
        (path (make-wild-path)))
    (with-accessors
          ((directories wild-path-directories)
           (file-name wild-path-file-name))
        path
      (labels
          ;; Returns non-nil iff we're on the first path component and
          ;; it consists of nothing but slashes.
          ((leading-slashes-p ()
             (and (zerop (length directories))
                  (not (find-if-not (lambda (c) (equal c #\/)) path-part))))

           ;; We're done with a part of the path.  Let's finalize it
           ;; by turning it into a regex scanner or a string.
           (prep-part (part)
             (cond
               ((find-if-not 'characterp part)
                (create-scanner
                 (nconc (list :sequence :start-anchor) (coerce part 'list) (list :end-anchor))
                 :multi-line-mode t))
               (t
                (coerce part 'string))))

           ;; Its time to take the current part and add it to the
           ;; directory vector.
           (finish-segment ()
             (vector-push-extend (prep-part path-part) directories)
             (setf path-part (make-extensible-vector)))

           ;; We're all done!  Put anything left in the current part
           ;; into the path.
           (final-segment ()
             (when (zerop (length path-part))
               (return-from final-segment))

             ;; Ordinarily, we would always treat the last `path-part'
             ;; as a file (since encountering a slash causes us to
             ;; call `finish-segment').  However, the leading slashes
             ;; are treated specially, and we need to continue
             ;; treating them specially, here.
             (when (leading-slashes-p)
               (finish-segment)
               (return-from final-segment))

             (setf file-name (prep-part path-part)))

           ;; We just saw a #\/!
           (ingest-/ ()
             ;; If the very first component of the path is a slash,
             ;; then we want to put it into the `path-part'.  You
             ;; might naively think that is the end of the first part.
             ;; However, POSIX says that the OS is allowed to treat
             ;; two leading slashes differently from any other
             ;; quantity.  So, let's just accumulate ALL the leading
             ;; slashes into a single part and then simplify it later.
             (when (leading-slashes-p)
               (vector-push-extend #\/ path-part)
               (return-from ingest-/))

             (finish-segment))

           ;; Act on any character other than #\/.
           (ingest (char first-p quoted-p)
             (cond
               (quoted-p
                (vector-push-extend char path-part))

               ((equal char #\?)
                (vector-push-extend
                 (if first-p
                     '(:inverted-char-class #\.)
                     :everything)
                 path-part))

               ((equal char #\*)
                (vector-push-extend
                 (if first-p
                     '(:greedy-repetition 0 1
                       (:sequence
                        (:inverted-char-class #\.)
                        (:greedy-repetition 0 nil
                         :everything)))
                     '(:greedy-repetition 0 nil :everything))
                 path-part))

               ((equal char #\[)
                (error "[] not implemented"))

               (t
                (vector-push-extend char path-part)))))

        (fset:do-seq (fragment fragments)
          (let ((quoted-p (string-fragment-quoted fragment))
                (string (string-fragment-string fragment)))
            (loop :for index :below (length string) :do
               (let* ((char (aref string index))
                      (first-p (zerop (length path-part))))

                 (cond
                   ((equal #\/ char)
                    (ingest-/))

                   ((leading-slashes-p)
                    (unless (zerop (length path-part))
                      (finish-segment))
                    (ingest char first-p quoted-p))

                   (t
                    (ingest char first-p quoted-p)))))))

        (final-segment)
        (unless (zerop (length directories))
          (symbol-macrolet
              ((first-part (aref directories 0)))
            (unless (or (not (stringp first-part))
                        (find #\/ first-part :test-not 'equal))
              (assert (not (zerop (length first-part))))
              (setf first-part (if (equal 2 (length first-part)) "//" "/")))))
        path))))

(defun directory-contents-iterator (dir-ptr)
  (make-iterator ()
    (tagbody
     again
       (let ((dirent (shcl/core/posix:readdir dir-ptr)))
         (when (null-pointer-p dirent)
           (stop))
         (let ((name (foreign-string-to-lisp (foreign-slot-pointer dirent '(:struct dirent) 'd-name))))
           (if (or (equal "." name)
                   (equal ".." name))
               (go again)
               (emit name)))))))

(defmacro with-directory-or-nil ((dir-name) &body body)
  (let ((cd-done (gensym "CD-DONE"))
        (e (gensym "E"))
        (escape (gensym "ESCAPE")))
    `(block ,escape
       (let (,cd-done)
         (handler-bind
             ((path-invalid
               (lambda (,e)
                 (declare (ignore ,e))
                 (unless ,cd-done
                   (return-from ,escape)))))
           (with-local-working-directory (,dir-name)
             (setf ,cd-done t)
             ,@body))))))

(defun %expand-wild-path (directories index file-name)
  (let (next as-directory-p)
    (cond
      ((>= index (length directories))
       (setf next file-name)
       (setf as-directory-p nil))
      (t
       (setf next (aref directories index))
       (setf as-directory-p t)))

    (unless next
      (return-from %expand-wild-path (fset:seq (fset:empty-seq))))

    (labels
        ((recurse ()
           (%expand-wild-path directories (1+ index) file-name))
         (recurse-and-prefix (string slash-p)
           (let ((rest (recurse)))
             (unless rest
               (return-from recurse-and-prefix))
             (let ((s (fset:convert 'fset:seq string))
                   (slash (if slash-p (fset:seq #\/) (fset:empty-seq))))
               (fset:image (lambda (seq) (fset:concat s slash seq)) rest)))))
      (declare (dynamic-extent #'recurse #'recurse-and-prefix))
      (cond
        ((and (zerop index)
              (stringp next)
              (not (find-if-not (lambda (c) (equal #\/ c)) next)))
         (assert as-directory-p)
         (with-directory-or-nil (next)
           (recurse-and-prefix next nil)))

        ((equal next "")
         (assert as-directory-p)
         (recurse-and-prefix "/" nil))

        ((and (stringp next) as-directory-p)
         (with-directory-or-nil (next)
           (recurse-and-prefix next t)))

        ((stringp next)
         (assert (not as-directory-p))
         (let (fd)
           (unwind-protect
                (handler-case
                    (progn
                      (setf fd (openat-retained (current-working-directory-fd) next 0))
                      (fset:seq (fset:convert 'fset:seq next)))
                  (syscall-error (e)
                    (declare (ignore e))
                    nil))
             (when fd
               (fd-release fd)))))

        ((not (stringp next))
         (let ((matches (fset:empty-seq)))
           (with-dir-ptr-for-fd (dir-ptr (current-working-directory-fd))
             (do-iterator (file (directory-contents-iterator dir-ptr))
               (when (scan next file)
                 (if as-directory-p
                     (with-directory-or-nil (file)
                       (let ((rest (recurse-and-prefix file t)))
                         (when rest
                           (fset:appendf matches rest))))
                     (setf matches (fset:with-last matches (fset:convert 'fset:seq file)))))))
           (if (zerop (fset:size matches))
               nil
               matches)))))))

(defun expand-wild-path (wild-path)
  (unless (wild-path-wild-p wild-path)
    (error "Expansion of non-wild wild-paths is not supported"))

  (let ((matches
         (with-local-working-directory (".")
           (%expand-wild-path (wild-path-directories wild-path) 0 (wild-path-file-name wild-path)))))
    (when matches
      (fset:image (lambda (s) (fset:convert 'string s)) matches))))

(defun tilde-expansion (fragments)
  (when (zerop (fset:size fragments))
    (return-from tilde-expansion fragments))

  (let ((first (fset:first fragments)))
    (when (zerop (length (string-fragment-string first)))
      (return-from tilde-expansion fragments))

    (unless (and (string-fragment-literal first)
                 (equal #\~ (aref (string-fragment-string first) 0)))
      (return-from tilde-expansion fragments))

    (let* ((less-first (fset:less-first fragments))
           (previous (string-fragment-string first))
           (shortened (make-array (1- (length previous)) :element-type 'character :displaced-to previous :displaced-index-offset 1))
           (replacement-fragment (copy-string-fragment first))
           (new-first (make-string-fragment $home :quoted t :literal nil)))
      (unless (or (zerop (length shortened))
                  (equal #\/ (aref shortened 0)))
        (error "~~name/ is not implemented"))
      (setf (string-fragment-string replacement-fragment) shortened)
      (fset:with-first (fset:with-first less-first replacement-fragment) new-first))))

(defun expand-pathname (fragments)
  (setf fragments (tilde-expansion fragments))
  (let ((wild-path (make-wild-path-from-fragments fragments)))
    (or (when (wild-path-wild-p wild-path)
          (expand-wild-path wild-path))
        (fset:seq (concat-fragments fragments)))))

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
