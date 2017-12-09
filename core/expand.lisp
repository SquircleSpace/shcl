(defpackage :shcl/core/expand
  (:use
   :common-lisp :shcl/core/utility :shcl/core/lexer :shcl/core/environment
   :shcl/core/working-directory :shcl/core/iterator)
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
  "This struct represents a part of a word.

`string-fragment-string' stores the characters associated with this
fragment.

`string-fragment-quoted' is a boolean value indicating whether the
characters should be treated as though they were quoted.  This impacts
how the characters are interpreted in some expansion contexts.  For
example, a #\~ appearing in a quoted string fragment will not expand
to the user's home directory.

`string-fragment-literal' is a boolean value indicating whether the
characters should be treated as though they were typed by the user
directly.  Tokens like `shcl/core/lexer:variable-expansion-word'
should expand to non-literal string fragments.  Tokens like
`shcl/core/lexer:if-word' should expand to a literal string fragment."
  string
  quoted
  literal)

(defun make-string-fragment (string &key quoted literal)
  "Create a `string-fragment'."
  (%make-string-fragment :string string :quoted quoted :literal literal))

(defparameter *split-fields* t
  "Non-nil if field splitting should be performed after expansion
takes place.

Methods on the `expand' generic function should not emit word boundary
markers (see `+soft-word-boundary+' and `+hard-word-boundary+') in
their expansions when this variable is bound to nil.  When this
variable is non-nil, methods on `expand' are permitted to split their
expanded strings into multiple strings by inserting word boundary
markers into their return value.  If appropriate, they are encouraged
to use the `split' function to do this.")

(defconstant +soft-word-boundary+ '+soft-word-boundary+
  "A weaker version of `+hard-word-boundary+'.

Unlike `+hard-word-boundary+', this marker only introduces a new field
in specific circumstances.
- Soft boundaries at the beginning or end of a fragment sequence have
  no effect.
- Consecutive soft boundaries can be coalesced into a single soft
  boundary.
- If a soft word boundary is next to a hard word boundary, then the
  soft word boundary can be removed from the fragment sequence without
  changing the end result.")

(defconstant +hard-word-boundary+ '+hard-word-boundary+
  "A marker indicating that a new field has begun.

When fragments are being joined together, a hard word boundary will
always introduce a new field, even if there are no fragments on one
side of the boundary.  As a result,
- Hard boundaries at the beginning or end of a fragment sequence
  introduce an empty field.
- Consecutive hard boundaries introduce empty a sequence of empty
  fields.")

(defun word-boundary ()
  "Returns a marker which indicates that the previous string fragment
should be considered a seperate word from the next one."
  +soft-word-boundary+)

(defun soft-word-boundary-p (thing)
  "Returns non-nil iff the given object is a soft word boundary."
  (eq +soft-word-boundary+ thing))

(defun hard-word-boundary-p (thing)
  "Returns non-nil iff the given object is a hard word boundary."
  (eq +hard-word-boundary+ thing))

(defun word-boundary-p (thing)
  "Returns non-nil iff the given object is a word boundary."
  (or (soft-word-boundary-p thing)
      (hard-word-boundary-p thing)))

(defgeneric alias-name (thing)
  (:documentation
   "Returns the alias that the given object might name.

If the given object can not name an alias, then this function returns
nil.  You do not need to confirm that there is actually an alias
associated with the name you return.

For example, instances of `shcl/core/lexer:variable-expansion-word'
cannot name an alias (because POSIX says so), so this generic function
should return nil for objects of that type.  On the other hand, a
`shcl/core/lexer:simple-word' token can be involved in alias
expansion.  So, this function should return the string contents of a
`shcl/core/lexer:simple-word' token."))
(defmethod alias-name ((thing string))
  ;; This method is provided for testing convenience
  thing)
(defmethod alias-name ((thing simple-word))
  (simple-word-text thing))
(defmethod alias-name ((thing token))
  nil)

(defparameter *aliases* (fset:empty-map)
  "This maps alias names (strings) to `alias' structs.

Every entry in this map represents an alias definition in the current
shell execution environment.")

(defstruct alias
  "A struct describing the expansion behavior of an alias."
  words
  continue-expansion)

(defun set-alias (name words &key continue-expansion-p)
  "Establish an alias in the current shell execution environment.

`name' is the string name for the alias.  `words' are the tokens that
should replace use of the given alias.  If `continue-expansion-p' is
nil, then use of this alias prevents further alias expansion in the
current command."
  (setf (fset:lookup *aliases* name)
        (make-alias :words (fset:convert 'fset:seq words)
                    :continue-expansion (not (not continue-expansion-p))))
  (values))

(defun unalias (name)
  "Remove the named alias from the current shell execution
environment.

`name' should be a string."
  (setf *aliases* (fset:less *aliases* name)))

(defun expand-aliases (tokens)
  "Perform alias expansion on the given token sequence."
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
           (alias-name (fset:first remaining))))
      (loop
         (when (zerop (fset:size remaining))
           (finish))

         (let* ((next-word (next-word))
                (alias (alias-for next-word)))
           (unless alias
             (finish))

           (setf *aliases* (fset:less *aliases* next-word))

           (let ((less-first (fset:less-first remaining)))
             (when (alias-continue-expansion alias)
               (setf less-first (expand-aliases less-first)))
             (setf remaining (fset:concat (alias-words alias) less-first))))))))

(defun expansion-for-words (things &key expand-aliases expand-pathname (split-fields t))
  "Perform expansion on a sequence of tokens.

This always performs the expansion done by the `expand' generic
function.  Alias expansion and pathname (glob-style) expansion are
optional and enabled by keyword arguments.

The value of the `split-fields' keyword argument is bound to the
`*split-fields*' special variable.  See that variable's documentation
to understand how it impacts expansion."
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
  "This struct represents a path which may have components which
contain wildcards.

The first element of `directories' may start with slash characters.
This indicates that the path is absolute.  If the first directory
component doesn't start with a slash, then the path may be assumed to
be relative.

A wildcard component of the path is simply a scanner returned by
`cl-ppcre:create-scanner'.

How wild!"
  file-name
  (directories (make-extensible-vector)))

(defun wild-path-wild-p (wild-path)
  "Returns non-nil if the given `wild-path' contains wildcard
components."
  (with-accessors
        ((file-name wild-path-file-name) (directories wild-path-directories))
      wild-path
    (or (and file-name (not (stringp file-name)))
        (find-if-not 'stringp directories))))

(defun split-fragments-into-directory-segments (fragments)
  "Given a sequence of fragments, produce a sequence of sequences of
fragments which contain no #\/ characters.

The sequences of fragments are known as segments.  Each segment
represents a directory component.  The final segment always represents
the file name at the end of the path.  If the file name segment is
empty, then the fragments ended in a #\

Ignoring the final segment, empty segments indicate the presence of
consecutive slashes.  Empty segments at the start of the sequence of
segments represent slashes at the start of the path.

The string \"//\" would result in a sequence of three empty segments.
The first two represent the leading slashes and the last one
represents the empty file name."
  (let ((result (fset:empty-seq))
        (part (fset:empty-seq)))
    (loop :while (not (zerop (fset:size fragments))) :do
       (let* ((fragment (fset:pop-first fragments))
              (s (string-fragment-string fragment))
              (position (position #\/ s)))
         (cond
           ((not position)
            (fset:push-last part fragment))

           (t
            (unless (zerop position)
              (let ((f (copy-string-fragment fragment)))
                (setf (string-fragment-string f)
                      (make-array position :element-type (array-element-type s)
                                  :displaced-to s :displaced-index-offset 0))
                (fset:push-last part f)))
            (fset:push-last result part)
            (setf part (fset:empty-seq))
            (unless (equal (1+ position) (length s))
              (let ((f (copy-string-fragment fragment)))
                (setf (string-fragment-string f)
                      (make-array (- (length s) (1+ position))
                                  :element-type (array-element-type s)
                                  :displaced-to s :displaced-index-offset (1+ position)))
                (fset:push-first fragments f)))))))
    (fset:push-last result part)
    result))

(defun handle-leading-slashes (segments)
  "Strip off empty segments from the start of the segment sequence.

See `split-fragments-into-directory-segments'.

Returns two values:
1. A string containing the leading slashes implied by the leading
   empty segments.
2. The remaining segments after the leading non-file empty segments
   are removed."
  (let ((slash-stream (make-string-output-stream)))
    (loop :while (and (< 1 (fset:size segments)) ;; Don't touch the last segment
                      (zerop (fset:size (fset:first segments))))
       :do
       (progn
         (write-char #\/ slash-stream)
         (fset:pop-first segments)))
    (let ((slashes (get-output-stream-string slash-stream)))
      (values
       (when (not (zerop (length slashes))) slashes)
       segments))))

(defstruct fragment-stream
  "A stream-like object that emits characters contained within an
`fset:seq' of fragments.

Use `fragment-stream-read' to read from this stream."
  ;; Until `fragment-stream' becomes more widly used, there's no point
  ;; in making it an actual stream object.
  (fragments (fset:empty-seq))
  (index 0))

(defun fragment-stream-read (fragment-stream)
  "Reads a character from the given `fragment-stream'.

Returns two values: the character and the fragment it came from.
Returns nil on EOF."
  (with-accessors
        ((fragments fragment-stream-fragments)
         (index fragment-stream-index))
      fragment-stream
    (tagbody
     again
       (let ((fragment (fset:first fragments)))
         (unless fragment
           (return-from fragment-stream-read))
         (when (>= index (length (string-fragment-string fragment)))
           (fset:pop-first fragments)
           (setf index 0)
           (go again))
         (let ((char (aref (string-fragment-string fragment) index)))
           (incf index)
           (return-from fragment-stream-read (values char fragment)))))))

(defun parse-char-class-glob (chars first-p)
  (declare (ignore first-p))
  ;; TODO: This isn't what the standard says to do!
  (warn 'not-implemented :feature "[] glob patterns (falling back on hacky (but almost right) behavior)")
  `(:regex ,(concatenate 'string "[" chars "]")))

(defun make-wild-path-component-from-fragments (fragments)
  "Translate a `fset:seq' of fragments into a description of a path
component.

The return value is appropriate for storing in a `wild-path'."
  (let ((fragment-stream (make-fragment-stream :fragments fragments))
        (result (make-extensible-vector)))
    (labels
        ((ingest (char first-p quoted-p)
           (cond
             (quoted-p
              (vector-push-extend char result))

             ((equal char #\?)
              (vector-push-extend
               (if first-p
                   '(:inverted-char-class #\.)
                   :everything)
               result))

             ((equal char #\*)
              (vector-push-extend
               (if first-p
                   '(:greedy-repetition 0 1
                     (:sequence
                      (:inverted-char-class #\.)
                      (:greedy-repetition 0 nil
                       :everything)))
                   '(:greedy-repetition 0 nil :everything))
               result))

             ((equal char #\[)
              (let ((lookahead-stream (copy-fragment-stream fragment-stream))
                    (char-class (make-extensible-vector)))
                (loop
                   (multiple-value-bind (future-char fragment) (fragment-stream-read lookahead-stream)
                     (unless future-char
                       (return))
                     (when (and (equal #\] future-char)
                                (not (string-fragment-quoted fragment)))
                       (setf fragment-stream lookahead-stream)
                       (vector-push-extend (parse-char-class-glob char-class first-p) result)
                       (return-from ingest))
                     (vector-push-extend future-char char-class))))
              (vector-push-extend char result))

             (t
              (vector-push-extend char result)))))
      (loop
         (multiple-value-bind (char fragment) (fragment-stream-read fragment-stream)
           (unless char
             (return))
           (assert (not (equal #\/ char)))
           (let ((quoted-p (string-fragment-quoted fragment))
                 (first-p (equal 1 (fragment-stream-index fragment-stream))))
             (ingest char first-p quoted-p)))))
    (when (zerop (length result))
      (return-from make-wild-path-component-from-fragments))

    (if (find-if-not 'characterp result)
        (create-scanner
         (nconc (list :sequence :start-anchor) (coerce result 'list) (list :end-anchor))
         :multi-line-mode t)
        (coerce result 'simple-string))))

(defun make-wild-path-from-fragments (fragments)
  "Translate a `fset:seq' of fragments into a `wild-path'."
  (multiple-value-bind (absolute-part remaining-segments)
      (handle-leading-slashes (split-fragments-into-directory-segments fragments))
    (let ((path (make-wild-path)))
      (with-accessors
            ((directories wild-path-directories)
             (file-name wild-path-file-name))
          path
        (when absolute-part
          (vector-push-extend absolute-part directories))
        (assert (<= 1 (fset:size remaining-segments)))
        (let* ((components (fset:image #'make-wild-path-component-from-fragments remaining-segments))
               (file-component (fset:pop-last components)))
          (when file-component
            (setf file-name file-component))
          (fset:do-seq (dir components)
            (vector-push-extend (if dir dir "") directories))))
      path)))

(defun directory-contents-iterator (dir-ptr)
  "Return an iterator which returns the contents of the given directory.

`dir-ptr' is assumed to remain valid for the lifetime of this
iterator.  Using this iterator after `dir-ptr' has been closed results
in undefined behavior."
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
  "This is a wrapper around
`shcl/core/working-directory:with-local-working-directory' which
evaluates to nil when the given `dir-name' cannot be opened as a
directory."
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
  "The brains of `expand-wild-path'."
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
  "Given a `wild-path', produce an `fset:seq' of strings representing
the paths that were matched.

Returns nil if no matches were found."
  (unless (wild-path-wild-p wild-path)
    (error "Expansion of non-wild wild-paths is not supported"))

  (let ((matches
         (with-local-working-directory (".")
           (%expand-wild-path (wild-path-directories wild-path) 0 (wild-path-file-name wild-path)))))
    (when matches
      (fset:image (lambda (s) (fset:convert 'string s)) matches))))

(defun tilde-expansion (fragments)
  "Attempt to expand leading ~s in the given sequence of string
fragments."
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
        (error 'not-implemented :feature "~~name/"))
      (setf (string-fragment-string replacement-fragment) shortened)
      (fset:with-first (fset:with-first less-first replacement-fragment) new-first))))

(defun expand-pathname (fragments)
  "Perform path-related expansions on the given word (which is
represented as a sequence of string fragments)."
  (setf fragments (tilde-expansion fragments))
  (let ((wild-path (make-wild-path-from-fragments fragments)))
    (or (when (wild-path-wild-p wild-path)
          (expand-wild-path wild-path))
        (fset:seq (concat-fragments fragments)))))

(defun concat-fragments (fragments)
  "Concatenate the given string fragments into a normal string."
  (let ((stream (make-string-output-stream)))
    (fset:do-seq (f fragments)
      (write-string (string-fragment-string f) stream))
    (get-output-stream-string stream)))

(defgeneric expand (thing)
  (:documentation
   "Expand the given token.

This function should return an `fset:seq' of `string-fragment's and
word boundary markers (see `+soft-word-boundary+',
`+hard-word-boundary+', and `*split-fields*').  The properties set on
the returned string fragments will impact subsequent expansion phases.
Adjacent string fragments will be considered to be parts of the same
field.  As a result, the following return values produce identical
results.

    (fset:seq (make-string-fragment \"foobar\"))
    (fset:seq (make-string-fragment \"foo\") (make-string-fragment \"bar\"))

On the other hand, the following return values could produce different
end results.

    (fset:seq (make-string-fragment \"foo\") (make-string-fragment \"bar\"))
    (fset:seq (make-string-fragment \"foo\") (make-string-fragment \"bar\" :literal t))

Use word boundaries responsibly.  Check with `*split-fields*' before
you insert them into your return value."))

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
  (fset:seq (make-string-fragment (single-quote-contents thing) :quoted t :literal t)))

(defmethod expand ((thing double-quote))
  (let ((*split-fields* nil))
    (let* ((parts (double-quote-parts thing))
           (result (fset:seq)))
      (loop :for part :across parts :do
         (let ((expansion (expand part)))
           (fset:do-seq (sub-part expansion)
             (unless (word-boundary-p sub-part)
               ;; Mark the fragment as quoted
               (unless (string-fragment-quoted sub-part)
                 (setf sub-part (copy-string-fragment sub-part))
                 (setf (string-fragment-quoted sub-part) t))
               ;; Add it to the result sequence
               (setf result (fset:with-last result sub-part))))))
      result)))

(defun expand-variable (variable)
  (let ((value
         (cond
           ((or (equal variable "@")
                (equal variable "*"))
            (error 'not-implemented :feature "Special variables"))

           (t
            (env variable)))))
    (if *split-fields*
        (split value)
        (fset:seq (make-string-fragment value)))))

(defmethod expand ((thing variable-expansion-word))
  (expand-variable (variable-expansion-word-variable thing)))

(defmethod expand ((thing variable-expansion-length-word))
  (let* ((*split-fields* nil)
         (fragments (expand-variable (variable-expansion-length-word-variable thing)))
         (length 0))
    (fset:do-seq (fragment fragments)
      (when (string-fragment-p fragment)
        (incf length (length (string-fragment-string fragment)))))
    (fset:seq (make-string-fragment (format nil "~A" length)))))

(defun ifs-parts (ifs)
  "Return two values: a string containing the non-whitespace
characters in `$ifs' and a string containing the whitespace characters
in `$ifs'."
  (labels ((blank (c) (whitespace-p c))
           (not-blank (c) (not (blank c))))
    (values
     (remove-if #'not-blank ifs)
     (remove-if #'blank ifs))))

(defun split (string)
  "Split a string into multiple fields based on its content and the
current value of $ifs.

This function returns a `fset:seq' of string fragments."
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
