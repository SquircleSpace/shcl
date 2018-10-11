;; Copyright 2017 Bradley Jensen
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defpackage :shcl/core/expand
  (:use
   :common-lisp :shcl/core/utility :shcl/core/lexer
   :shcl/core/working-directory :shcl/core/iterator)
  (:import-from :fset)
  (:import-from :cl-ppcre #:create-scanner #:scan)
  (:import-from :cffi #:foreign-string-to-lisp #:foreign-slot-pointer #:null-pointer-p)
  (:import-from :shcl/core/posix #:syscall-error)
  (:import-from :shcl/core/posix-types #:dirent #:d-name)
  (:import-from :shcl/core/fd-table #:with-dir-ptr-for-fd #:retained-fd-openat #:fd-wrapper-release)
  (:import-from :shcl/core/exit-info #:exit-info)
  (:import-from :shcl/core/environment #:env #:$ifs #:$home)
  (:import-from :shcl/core/data #:define-data #:define-cloning-setf-expander)
  (:export
   #:expansion-for-words #:set-alias #:unalias #:expand #:make-string-fragment
   #:string-fragment-string #:string-fragment-quoted-p
   #:string-fragment-literal-p #:string-fragment
   #:word-boundary #:*split-fields* #:split #:*allow-side-effects*
   #:side-effect-violation #:check-side-effects-allowed
   #:permit-side-effects-once))
(in-package :shcl/core/expand)

(optimization-settings)

(defgeneric string-fragment-string (fragment)
  (:documentation
   "Retrieve the sequence of characters that the given string fragment
carries."))

(defgeneric string-fragment-quoted-p (fragment)
  (:documentation
   "Return non-nil iff the given string fragment is representing
quoted characters.

This impacts how the characters are interpreted in some contexts.  For
example, a #\~ appearing in a quoted string fragment should not expand
to the user's home directory."))

(defgeneric string-fragment-literal-p (fragment)
  (:documentation
   "Return non-nil iff the given string fragment is representing
characters that appeared literally in the input text.

Tokens like `shcl/core/lexer:variable-expansion-word' should expand to
non-literal string fragments.  Tokens like `shcl/core/lexer:if-word'
should expand to a literal string fragment."))

(define-data string-fragment ()
  ((string
    :initarg :string
    :initform (required)
    :type string
    :reader string-fragment-string
    :writer unsafe-set-string-fragment-string)
   (quoted-p
    :initarg :quoted-p
    :initform nil
    :type boolean
    :reader string-fragment-quoted-p
    :writer unsafe-set-string-fragment-quoted-p)
   (literal-p
    :initarg :literal-p
    :initform nil
    :type boolean
    :reader string-fragment-literal-p
    :writer unsafe-set-string-fragment-literal-p))
  (:documentation
   "This class represents a part of a string."))

(defmethod print-object ((fragment string-fragment) stream)
  (print-unreadable-object (fragment stream :type t)
    (format stream "~W" (string-fragment-string fragment))
    (when (string-fragment-quoted-p fragment)
      (format stream " :quoted-p t"))
    (when (string-fragment-literal-p fragment)
      (format stream " :literal-p t"))))

(define-cloning-setf-expander string-fragment-string unsafe-set-string-fragment-string)
(define-cloning-setf-expander string-fragment-quoted-p unsafe-set-string-fragment-quoted-p)
(define-cloning-setf-expander string-fragment-literal-p unsafe-set-string-fragment-literal-p)

(defun make-string-fragment (string &key quoted-p literal-p)
  "Create a `string-fragment'."
  (make-instance 'string-fragment :string string :quoted-p quoted-p :literal-p literal-p))

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

(defvar *allow-side-effects* nil
  "This variable controls whether `expand' methods are permitted to
have side effects.

Methods on `expand' are expected to consult this variable and act
accordingly.  Sometimes expansion will be performed speculatively.
When that happens, it is inappropriate to expand tokens that have side
effects.  If you are writing a method on `expand' that has side
effects, you should ensure that your method consults this variable.
If your method requires the ability to have side effects, you should
call `check-side-effects-allowed'.")

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
  "Perform alias expansion on the given token sequence and return an
iterable sequence of alternate tokens."
  (let* ((remaining (iterable-values tokens 'fset:seq))
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

(defun ensure-exit-info-seq (value)
  (etypecase value
    (null
     (fset:empty-seq))
    (exit-info
     (fset:seq value))
    (fset:seq
     value)))

(defun expand-token-sequence (token-sequence)
  (let ((result (fset:empty-seq))
        (exit-infos (fset:empty-seq)))
    (do-sequence (token token-sequence)
      (multiple-value-bind (value exit-info) (expand-token token)
        (fset:appendf result value)
        (fset:appendf exit-infos exit-info)))
    (values result exit-infos)))

(defun expand-token (token)
  (multiple-value-bind (expansion-fragments exit-info) (expand token)
    (let ((result (fset:empty-seq))
          next-word)
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
        (fset:do-seq (fragment expansion-fragments)
          (if (word-boundary-p fragment)
              (boundary fragment)
              (observe fragment)))
        (boundary +soft-word-boundary+)
        (values result (ensure-exit-info-seq exit-info))))))

(defun run-expansion-pipeline (things pipeline)
  (let ((last-result things)
        (exit-infos (make-extensible-vector)))
    (loop :for fn :across pipeline :do
       (multiple-value-bind
             (this-result this-exit-infos)
           (funcall fn last-result)
         (setf last-result this-result)
         (vector-push-extend this-exit-infos exit-infos)))
    (values last-result (concatenate-iterable-collection exit-infos))))

(defun compute-expansion-pipeline (&key start-at end-at
                                     (expand-aliases t)
                                     (expand-token-sequence t)
                                     (expand-tilde-words t)
                                     (expand-pathname-words t)
                                     (concatenate-fragments-for-words t))
  (let ((pipeline (make-extensible-vector))
        (started (unless start-at t)))
    (macrolet
        ((consider (name)
           `(progn
              (when (and (not started) (eq start-at ',name))
                (setf started t))
              (when (eq end-at ',name)
                (return-from compute-expansion-pipeline pipeline))
              (when (and started ,name)
                (vector-push-extend ',name pipeline)))))
      (consider expand-aliases)
      (consider expand-token-sequence)
      (consider expand-tilde-words)
      (consider expand-pathname-words)
      (consider concatenate-fragments-for-words)
      pipeline)))

(defun expansion-for-words (things &key expand-aliases expand-pathname-words
                                     pipeline (split-fields t)
                                     (allow-side-effects t))
  "Perform expansion on a sequence of tokens.

This always performs the expansion done by the `expand' generic
function.  Alias expansion and pathname (glob-style) expansion are
optional and enabled by keyword arguments.

The first return value is an `fset:seq' of strings representing the
expanded content.  The second return value is an `fset:seq' of
`exit-info' objects that were generated while expanding the tokens.
The `exit-info' objects are in the order that they were encountered.

For example, expanding \"$(true) stuff* goes{a,b} $(false) here\"
could produce the following return values.
    (fset:seq \"stuffAndThings\" \"goesa\" \"goesb\" \"here\")
    (fset:seq (truthy-exit-info) (falsey-exit-info))

The value of the `split-fields' keyword argument is bound to the
`*split-fields*' special variable.  See that variable's documentation
to understand how it impacts expansion.

The value of the `allow-side-effects' keyword argument is bound to the
`*allow-side-effects*' special variable.  See that variable's
documentation to understand how it impacts expansion."
  (let ((*split-fields* split-fields)
        (*allow-side-effects* allow-side-effects)
        (pipeline (or pipeline
                      (compute-expansion-pipeline
                       :expand-aliases expand-aliases
                       :expand-token-sequence t
                       :expand-pathname-words expand-pathname-words
                       :concatenate-fragments-for-words t))))
    (run-expansion-pipeline things pipeline)))

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
empty, then the fragments ended in a #\/.

Ignoring the final segment, empty segments indicate the presence of
consecutive slashes.  Empty segments at the start of the sequence of
segments represent slashes at the start of the path.

The string \"//\" would result in a sequence of three empty segments.
The first two represent the leading slashes and the last one
represents the empty file name."
  (let ((result (fset:empty-seq))
        (part (fset:empty-seq)))
    (labels
        ((consume-fragment (fragment)
           (let* ((s (string-fragment-string fragment))
                  (position (position #\/ s)))
             (cond
               ((not position)
                (fset:push-last part fragment))

               (t
                (unless (zerop position)
                  (let ((new-fragment fragment))
                    (setf (string-fragment-string new-fragment)
                          (make-array position :element-type (array-element-type s)
                                      :displaced-to s :displaced-index-offset 0))
                    (fset:push-last part new-fragment)))

                (fset:push-last result part)
                (setf part (fset:empty-seq))

                (unless (equal (1+ position) (length s))
                  (let* ((new-string (make-array (- (length s) (1+ position))
                                                 :element-type (array-element-type s)
                                                 :displaced-to s
                                                 :displaced-index-offset (1+ position)))
                         (new-fragment (shcl/core/data:clone fragment :string new-string)))
                    (consume-fragment new-fragment))))))))

      (do-sequence (fragment fragments)
        (consume-fragment fragment))
      (fset:push-last result part)
      result)))

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
       (let* ((fragment (fset:first fragments))
              (string (if fragment
                          (string-fragment-string fragment)
                          (return-from fragment-stream-read))))
         (when (>= index (length string))
           (fset:pop-first fragments)
           (setf index 0)
           (go again))
         (let ((char (aref string index)))
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
                                (not (string-fragment-quoted-p fragment)))
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
           (let ((quoted-p (string-fragment-quoted-p fragment))
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
    ;; Figure out what we're looking for and whether it needs to be a
    ;; directory or not.
    (cond
      ((>= index (length directories))
       (unless file-name
         (return-from %expand-wild-path (fset:seq (fset:empty-seq))))
       (setf next file-name)
       (setf as-directory-p nil))
      (t
       (setf next (aref directories index))
       (setf as-directory-p t)))

    (labels
        ((recurse ()
           (%expand-wild-path directories (1+ index) file-name))
         (recurse-and-prefix (string slash-p)
           ;; Recurse and add `string' (optionally followed by a #\/
           ;; character) to the start of all results produced by the
           ;; recursion.
           (let ((rest (recurse)))
             (unless rest
               (return-from recurse-and-prefix))
             (let ((s (fset:convert 'fset:seq string))
                   (slash (if slash-p (fset:seq #\/) (fset:empty-seq))))
               (fset:image (lambda (seq) (fset:concat s slash seq)) rest)))))
      (declare (dynamic-extent #'recurse #'recurse-and-prefix))
      ;; For each invocation of this function, we're only concerned
      ;; with finding matches in the current directory.

      (cond
        ;; Handle absolute paths.  Remember, wild paths represent
        ;; absolute paths by having the first directory component
        ;; contain nothing but slash characters.
        ((and (zerop index)
              (stringp next)
              (not (find-if-not (lambda (c) (equal #\/ c)) next)))
         (assert as-directory-p)
         (with-directory-or-nil (next)
           (recurse-and-prefix next nil)))

        ;; Handle empty directory components (i.e. adjacent,
        ;; non-leading slash characters).
        ((equal next "")
         (assert as-directory-p)
         (recurse-and-prefix "/" nil))

        ;; Handle specifically named directories
        ((and (stringp next) as-directory-p)
         (with-directory-or-nil (next)
           (recurse-and-prefix next t)))

        ;; Handle specifically named files
        ((stringp next)
         (assert (not as-directory-p))
         (handler-case
             (shcl/core/fd-table:receive-ref-counted-fd
                 (fd (retained-fd-openat (get-fd-current-working-directory) next 0))
               (declare (ignore fd))
               (fset:seq (fset:convert 'fset:seq next)))
           (syscall-error ())))

        ;; Oooh.  Exciting.  A scanner.  Iterate the contents of the
        ;; current directory and look for things that match the given
        ;; name.
        ((not (stringp next))
         (let ((matches (fset:empty-seq)))
           (with-dir-ptr-for-fd (dir-ptr (get-fd-current-working-directory))
             (shcl/core/posix:do-directory-contents (file dir-ptr)
               (when (and (not (or (equal file ".") (equal file "..")))
                          (scan next file))
                 (if as-directory-p
                     (with-directory-or-nil (file)
                       (let ((rest (recurse-and-prefix file t)))
                         (when rest
                           (fset:appendf matches rest))))
                     (fset:push-last matches (fset:convert 'fset:seq file))))))
           (unless (zerop (fset:size matches))
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
      (labels
          ((prepare-match (match)
             (fset:seq (make-string-fragment (fset:convert 'string match) :quoted-p t))))
        (declare (dynamic-extent #'prepare-match))
        (fset:image #'prepare-match matches)))))

(defun expand-tilde-words (fragment-seqs)
  (sequence-map fragment-seqs 'expand-tilde))

(defun expand-tilde (fragments)
  "Attempt to expand leading ~s in the given sequence of string
fragments."
  (setf fragments (forkable-wrapper-iterator (iterator fragments)))
  (let ((untouched-fragments (fork fragments))
        tilde-seen)
    (do-sequence (fragment untouched-fragments)
        (unless (string-fragment-literal-p fragment)
          (return-from expand-tilde fragments))
        (loop :with string = (string-fragment-string fragment)
           :for index :below (length string)
           :for char = (aref string index) :do
           (cond

             ((and tilde-seen (not (equal char #\/)))
              ;; This might be a little overly strict, but...
              (error 'not-implemented :feature "~name expansion"))

             (tilde-seen
              (assert (equal char #\/))
              (let* ((new-string (make-array (- (length string) index)
                                             :element-type 'character
                                             :displaced-to string
                                             :displaced-index-offset index))
                     (new-fragment (shcl/core/data:clone fragment :string new-string)))
                (return-from expand-tilde
                  (concatenate-iterables
                   (list (make-string-fragment $home :quoted-p t) new-fragment)
                   untouched-fragments))))

             ((equal char #\~)
              (setf tilde-seen t))

             (t
              (return-from expand-tilde fragments)))))
    (if tilde-seen
        (list-iterator (list (make-string-fragment $home :quoted-p t)))
        fragments)))

(defvar *error-on-failed-glob* nil
  "If this variable is non-nil, then an error will be signaled when
pathname expansion cannot find any matches for a glob pattern.

POSIX says that if a glob doesn't match any files then pathname
expansion should simply produce the string the user typed.  So, if the
user runs
    rm [fb]oo
in a directory where neither foo nor boo exist, then rm will receive
the string '[fb]oo' as its argument.  That's a bit whacky.  If this
variable is non-nil, then a glob failure aborts the command.")

(defun expand-pathname-words (fragment-seqs)
  (concatmapped-iterator fragment-seqs 'expand-one-pathname))

(defun expand-one-pathname (fragments)
  "Perform path-related expansions on the given word (which is
represented as a sequence of string fragments).

Returns a sequence of expansions.  Each expansion is represented as a
sequence of string fragments."
  (setf fragments (iterable-values fragments))
  (let ((wild-path (make-wild-path-from-fragments fragments)))
    (or (when (wild-path-wild-p wild-path)
          (expand-wild-path wild-path))
        (when *error-on-failed-glob*
          (error "Glob pattern didn't match anything: ~A" (concatenate-fragments fragments)))
        (fset:seq fragments))))

(defun concatenate-fragments-for-words (word-fragment-sequences)
  (mapped-iterator word-fragment-sequences 'concatenate-fragments))

(defun concatenate-fragments (fragments)
  "Concatenate the given string fragments into a normal string."
  (let ((stream (make-string-output-stream)))
    (do-sequence (f fragments)
      (write-string (string-fragment-string f) stream))
    (get-output-stream-string stream)))

(define-condition side-effect-violation (error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "A token expansion was requested that would cause side
effects.  Side effects are currently not permitted.  The value of
`*allow-side-effects*' controls whether this error is signaled or
not.")))
  (:documentation
   "A condition to represent the case where a token expansion wishes
to have side effects, but side effects are currently disallowed by
`*allow-side-effects*'."))

(defun permit-side-effects-once (&optional condition)
  "Invoke the `permit-side-effects-once' restart associated with
`condition'.

The restart is expected to ignore a `side-effect-violation' that
occurred."
  (let ((restart (find-restart 'permit-side-effects-once condition)))
    (when restart
      (invoke-restart restart))))

(defun check-side-effects-allowed ()
  "Signal `side-effect-violation' if side effects are disallowed by
`*allow-side-effects*'."
  (unless *allow-side-effects*
    (restart-case
        (error 'side-effect-violation)
      (permit-side-effects-once ()))))

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
    (fset:seq (make-string-fragment \"foo\") (make-string-fragment \"bar\" :literal-p t))

Use word boundaries responsibly.  Check with `*split-fields*' before
you insert them into your return value.

If the expansion process involves running a command, you may return an
`exit-info' or an `fset:seq' of `exit-info' as the second return
value."))

(defmethod expand ((thing string))
  (fset:seq (make-string-fragment thing :literal-p t)))

(defmethod expand ((thing simple-word))
  (fset:seq (make-string-fragment (simple-word-text thing) :literal-p t)))

(defmethod expand ((thing compound-word))
  (let* ((parts (compound-word-parts thing))
         (result (fset:empty-seq))
         (exit-infos (fset:empty-seq)))
    (labels
        ((ingest (seq)
           (multiple-value-bind (fragments exit-infos) (expand seq)
             (fset:appendf result fragments)
             (fset:appendf exit-infos (ensure-exit-info-seq exit-infos)))))
      (loop :for index :from 0 :below (length parts) :do
         (ingest (aref parts index))))

    (values result exit-infos)))

;; This is not meant to be used to expand the assignment statements at
;; the start of a command.  Those expand differently (in particular,
;; field splitting doesn't occur).
(defmethod expand ((thing assignment-word))
  (with-accessors ((name assignment-word-name) (value assignment-word-value-word)) thing
    (let ((result (fset:seq (make-string-fragment "=")))
          (result-exit-infos (fset:empty-seq)))
      (multiple-value-bind (fragments exit-info) (expand name)
        (fset:prependf result fragments)
        (fset:appendf result-exit-infos (ensure-exit-info-seq exit-info)))
      (multiple-value-bind (fragments exit-info) (expand value)
        (fset:appendf result fragments)
        (fset:appendf result-exit-infos (ensure-exit-info-seq exit-info)))
      (values result result-exit-infos))))

(defmethod expand ((thing literal-token))
  (fset:seq (make-string-fragment (literal-token-string thing) :literal-p t)))

(defmethod expand ((thing single-quote))
  (fset:seq (make-string-fragment (single-quote-contents thing) :quoted-p t :literal-p t)))

(defmethod expand ((thing double-quote))
  (let ((*split-fields* nil))
    (let* ((parts (double-quote-parts thing))
           (result (fset:empty-seq))
           (result-exit-infos (fset:empty-seq)))
      (loop :for part :across parts :do
         (multiple-value-bind (expansion exit-info) (expand part)
           (fset:appendf result-exit-infos (ensure-exit-info-seq exit-info))
           (fset:do-seq (sub-part expansion)
             (unless (word-boundary-p sub-part)
               ;; Mark the fragment as quoted
               (unless (string-fragment-quoted-p sub-part)
                 (setf (string-fragment-quoted-p sub-part) t))
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

(defun expand-variable-length (variable)
  (let* ((*split-fields* nil)
         (fragments (expand-variable variable))
         (length 0))
    (fset:do-seq (fragment fragments)
      (when (typep fragment 'string-fragment)
        (incf length (length (string-fragment-string fragment)))))
    (fset:seq (make-string-fragment (format nil "~A" length)))))

(defmethod expand ((thing variable-expansion-word))
  (if (variable-expansion-word-length-p thing)
      (expand-variable-length (variable-expansion-word-variable thing))
      (expand-variable (variable-expansion-word-variable thing))))

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
    (when (and (zerop (length whitespace)) (zerop (length non-whitespace)))
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
