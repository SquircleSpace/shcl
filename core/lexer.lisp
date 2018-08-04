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

(defpackage :shcl/core/lexer
  (:use
   :common-lisp :shcl/core/utility :shcl/core/dispatch-table
   :shcl/core/iterator)
  (:import-from :shcl/core/positional-stream
   #:make-position-record-from-positional-stream #:position-record)
  (:import-from :shcl/core/data #:define-data)
  (:import-from :trivial-gray-streams)
  (:import-from :fset)
  (:import-from :closer-mop)
  (:export
   ;; Token classes
   #:token #:a-word #:eof #:simple-word #:compound-word
   #:assignment-word #:name #:io-number #:literal-token #:newline
   #:reserved-word #:single-quote #:double-quote #:command-word
   #:variable-expansion-word #:conditional-replacement-policy
   #:default-value-policy #:assign-default-value-policy
   #:error-policy #:alternate-value-policy

   ;; Slot accessors
   #:token-value #:simple-word-text #:compound-word-parts
   #:assignment-word-name #:assignment-word-value-word #:io-number-fd
   #:literal-token-string #:single-quote-contents #:double-quote-parts
   #:command-word-tokens #:command-word-evaluate-fn
   #:variable-expansion-word-variable #:variable-expansion-word-length-p
   #:conditional-replacement-policy-replacement-token
   #:conditional-replacement-policy-include-empty-string-p

   ;; Operators
   #:and-if #:or-if #:dsemi #:dless #:dgreat #:lessand #:greatand
   #:lessgreat #:dlessdash #:clobber #:semi #:par #:pipe #:lparen
   #:rparen #:great #:less

   ;; Reserved words
   #:if-word #:then #:else #:elif #:fi #:do-word #:done #:case-word #:esac
   #:while #:until #:for #:lbrace #:rbrace #:bang #:in

   ;; Functions
   #:tokens-in-string #:tokens-in-stream #:token-iterator #:next-token
   #:token-iterator-symbolic-readtable

   ;; Extensible reading
   #:lexer-context #:lexer-context-consume-p
   #:standard-shell-readtable))
(in-package :shcl/core/lexer)

(optimization-settings)

(defgeneric token-value (token)
  (:documentation
   "Returns the string that was used to build this token."))

(defgeneric token-position (token)
  (:documentation
   "Returns a `position-record' describing where this token began."))

(define-data token ()
  ((value
    :type (or null string)
    :initform nil
    :updater token-value
    :initarg :value)
   (position
    :type (or null position-record)
    :initform nil
    :updater token-position
    :initarg :position))
  (:documentation
   "A class representing a token in the POSIX shell language."))
(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t)
    (format stream "~W" (token-value token))))
(defmethod make-load-form ((token token) &optional environment)
  (let ((slots (mapcar 'closer-mop:slot-definition-name (closer-mop:class-slots (class-of token)))))
    (make-load-form-saving-slots token :slot-names slots :environment environment)))

(define-data eof (token)
  ((value :initform nil))
  (:documentation
   "A token representing end of file."))
(defmethod print-object ((eof eof) stream)
  (print-unreadable-object (eof stream :type t)))
(defparameter *eof*
  (make-instance 'eof)
  "An instance of the `eof' token.")

(define-data a-word (token)
  ()
  (:documentation
   "A base class for tokens that aren't \"special\" in the shell language.

These tokens represent file paths, quoted strings, command names, and
similar tokens.  Its a grab-bag of miscellaneous token types."))

(define-data simple-word (a-word)
  ((text
    :initarg :text
    :updater simple-word-text
    :initform (required)
    :type string))
  (:documentation
   "A class for plain-old sequences of characters.

There's nothing special going on here."))
(defmethod print-object ((simple-word simple-word) stream)
  (print-unreadable-object (simple-word stream :type t)
    (format stream "~W" (simple-word-text simple-word))))

(define-data compound-word (a-word)
  ((parts :type vector
          :initform (required)
          :updater compound-word-parts
          :initarg :parts))
  (:documentation
   "A token class that consists of several tokens combined into a
single aggregate token."))
(defmethod print-object ((compound-word compound-word) stream)
  (print-unreadable-object (compound-word stream :type t)
    (format stream "~W" (compound-word-parts compound-word))))

(defgeneric assignment-word-name (assignment-word)
  (:documentation
   "Retrieve the name portion of the assignment.

The name portion is the part that comes before the #\= character."))

(defgeneric assignment-word-value-word (assignment-word)
  (:documentation
   "Retrieve the value portion of the assignment.

The value portion is the part tat comes after the #\= character."))

(define-data assignment-word (a-word)
  ((name
    :type name
    :initform (required)
    :updater assignment-word-name
    :initarg :name)
   (value-word
    :type a-word
    :initarg :value-word
    :initform (required)
    :updater assignment-word-value-word))
  (:documentation
   "A class to represent words that look like variable assignments.

Whether or not this token actually represents a variable assignment
won't be known until after parsing happens."))
(defmethod print-object ((word assignment-word) stream)
  (print-unreadable-object (word stream :type t)
    (format stream "~W = ~W" (assignment-word-name word) (assignment-word-value-word word))))

(defun make-assignment-word-from-parts (parts &rest initargs)
  (let* ((first-part (aref parts 0))
         (first-word (simple-word-text first-part))
         (name-break-point (assignment-word-p first-word))
         (name (make-instance 'name :text (subseq first-word 0 name-break-point) :value nil))
         (value (subseq first-word (1+ name-break-point)))
         (new-parts (make-extensible-vector
                     :fill-pointer 0
                     :initial-size (if (zerop (length value))
                                       (length parts)
                                       (- (length parts) 1)))))
    (unless (zerop (length value))
      (vector-push-extend (make-instance 'simple-word :text value :value nil) new-parts))
    (loop :for index :from 1 :below (length parts) :do
       (vector-push-extend (aref parts index) new-parts))
    (let ((final-value (if (equal 1 (length new-parts))
                           (aref new-parts 0)
                           (make-instance 'compound-word :parts new-parts :value nil))))
      (apply 'make-instance 'assignment-word :name name :value-word final-value initargs))))

(define-data name (simple-word)
  ()
  (:documentation
   "A class to represent words that look like shell variable names."))

(define-data io-number (token)
  ((fd
    :type integer
    :initform (required)
    :updater io-number-fd
    :initarg :fd))
  (:documentation
   "A class to represent file descriptor identifiers for redirect
operations."))
(defmethod print-object ((io-number io-number) stream)
  (print-unreadable-object (io-number stream :type t)
    (format stream "~W" (io-number-fd io-number))))

(defun name-p (word)
  (labels ((first-okay (char)
             (or (alpha-char-p char)
                 (equal #\_ char)))
           (rest-okay (char)
             (or (first-okay char)
                 (digit-char-p char))))
    (loop :for index :below (length word) :do
       (unless (funcall (if (equal 0 index) #'first-okay #'rest-okay)
                        (aref word index))
         (return-from name-p nil)))
    t))

(defun assignment-word-p (word)
  (let* ((position (loop :for index :below (length word) :do
                      (when (equal (aref word index) #\=)
                        (return index)))))
    (unless position
      (return-from assignment-word-p nil))

    (let ((name (make-array position :element-type 'character :displaced-to word)))
      (when (name-p name)
        position))))

(defgeneric literal-token-string (token)
  (:documentation
   "Return the string that this literal token always represents."))

(define-data literal-token (token)
  ((string
    :type string
    :allocation :class
    :reader literal-token-string))
  (:documentation
   "This class is a base class for tokens that represent very specific
character sequences."))
(defmethod print-object ((literal-token literal-token) stream)
  (print-unreadable-object (literal-token stream :type t)))

(defmacro define-literal-token (name string &optional (superclasses '(literal-token)) slots &body options)
  `(progn
     (define-data ,name ,superclasses
       ((value :initform ,string)
        (string :initform ,string)
        ,@slots)
       ,@(unless (getf options :documentation)
           `((:documentation
              ,(format nil "A token class representing the ~W character sequence." string))))
       ,@options)))

(define-literal-token newline (string #\linefeed))
(defparameter *newline*
  (make-instance 'newline))

(defparameter *operators* (fset:empty-map))

(define-data operator (literal-token)
  ()
  (:documentation
   "A token class for operators.

Operators are treated specially when tokenizing, but they aren't any
different from other literal tokens after tokenization is complete.
All the same, this class exists to help group them all together."))

(defmacro define-operator (name string &optional (superclasses '(operator)) slots &body options)
  (check-type name symbol)
  (check-type string string)
  `(progn
     (define-literal-token ,name ,string ,superclasses ,slots ,@options)
     (fset:adjoinf *operators* ,string ',name)
     ',name))

(define-operator and-if "&&")
(define-operator or-if "||")
(define-operator dsemi ";;")
(define-operator dless "<<")
(define-operator dgreat ">>")
(define-operator lessand "<&")
(define-operator greatand ">&")
(define-operator lessgreat "<>")
(define-operator dlessdash "<<-")
(define-operator clobber ">|")
(define-operator semi ";")
(define-operator par "&")
(define-operator pipe "|")
(define-operator lparen "(")
(define-operator rparen ")")
(define-operator great ">")
(define-operator less "<")

(defun make-operator (string &rest initargs)
  (apply 'make-instance (fset:lookup *operators* string) initargs))

(defun operator-p (word)
  (fset:lookup *operators* word))

(defun prefix-match-p (prefix-word whole-word)
  (when (> (length prefix-word) (length whole-word))
    (return-from prefix-match-p nil))
  (loop :for w-char :across whole-word
     :for char :across prefix-word
     :do (unless (equal w-char char)
           (return-from prefix-match-p nil)))
  t)

(defparameter *reserved-words* (fset:empty-map))

(define-data reserved-word (a-word literal-token)
  ()
  (:documentation
   "A class to represent uses of words that POSIX treats specially.

Reserved words are only reserved in some contexts.  For example,
\"if\" can either be used to start a conditional or it could represent
two letters that should be passed to a command."))

(defmacro define-reserved-word (name string &optional (superclasses '(reserved-word)) slots &body options)
  (check-type name symbol)
  (check-type string string)
  `(progn
     (define-literal-token ,name ,string ,superclasses ,slots ,@options)
     (fset:adjoinf *reserved-words* ,string ',name)
     ',name))

(define-reserved-word if-word "if")
(define-reserved-word then "then")
(define-reserved-word else "else")
(define-reserved-word elif "elif")
(define-reserved-word fi "fi")
(define-reserved-word do-word "do")
(define-reserved-word done "done")
(define-reserved-word case-word "case")
(define-reserved-word esac "esac")
(define-reserved-word while "while")
(define-reserved-word until "until")
(define-reserved-word for "for")
(define-reserved-word lbrace "{")
(define-reserved-word rbrace "}")
(define-reserved-word bang "!")
(define-reserved-word in "in")

(defun make-reserved (string &rest initargs)
  (apply 'make-instance (fset:lookup *reserved-words* string) initargs))

(defun reserved-p (word)
  (fset:lookup *reserved-words* word))

(define-condition eof-error (error)
  ((comment :initarg :comment
            :initform "Unknown context"
            :accessor eof-comment)))
(defun eof-error (&rest rest)
  (apply 'error 'eof-error rest))

(define-data single-quote (a-word)
  ((contents
    :initarg :contents
    :initform (required)
    :type string
    :updater single-quote-contents))
  (:documentation
   "A class to represent characters that are quoted with #\'."))
(defmethod print-object ((single-quote single-quote) stream)
  (print-unreadable-object (single-quote stream :type t)
    (format stream "~W" (single-quote-contents single-quote))))

(defun read-single-quote (stream)
  (let* ((all-chars-stream (make-string-output-stream))
         (stream (make-echo-stream stream all-chars-stream))
         (next-char (peek-char nil stream nil :eof))
         (contents (make-extensible-vector :initial-size 0 :element-type 'character)))
    (labels ((next ()
               (vector-push-extend next-char contents)
               (skip))
             (skip ()
               (read-char stream nil :eof)
               (setf next-char (peek-char nil stream nil :eof))))
      (loop :while (not (equal next-char #\'))
         :do (if (equal :eof next-char)
                 (eof-error :comment "Single quote expected")
                 (next)))
      (assert (equal next-char #\'))
      ;; One more for the close quote
      (skip))
    (make-instance 'single-quote :contents contents
                   :value (get-output-stream-string all-chars-stream))))

(define-data escaped-character (single-quote)
  ()
  (:documentation
   "A class to represent characters that are quoted with #\\."))

(defun make-escaped-character (char)
  (make-instance 'escaped-character :contents (string char) :value (format nil "~C~C" #\\ char)))

(define-data double-quote (compound-word)
  ((parts :type vector
          :initform (required)
          :updater double-quote-parts
          :initarg :parts))
  (:documentation
   "A class to represent tokens that are quoted using #\"."))
(defmethod print-object ((double-quote double-quote) stream)
  (print-unreadable-object (double-quote stream :type t)
    (format stream "~W" (double-quote-parts double-quote))))

(define-condition unexpected-eof (error)
  ())

(defun shell-lexer-context-extensible-read-loop (context readtable)
  "Read using the given context's readtable until end is marked.

EOF results in the `unexpected-eof' error being signaled."
  (loop
     :while (not (lexer-context-end-marked-p context)) :do
     (if (eq :eof (peek-char nil context nil :eof))
         (error 'unexpected-eof)
         (handle-extensible-syntax context :readtable readtable)))
  (shell-lexer-context-delimit context))

(defun read-double-quote (stream)
  (let* ((readtable (subtable (shell-lexer-context-readtable stream) #(double-quote)))
         (inner-context
          (make-instance 'shell-lexer-context :stream stream :readtable (shell-lexer-context-readtable stream)))
         (token (handler-case
                    (shell-lexer-context-extensible-read-loop inner-context readtable)
                  (unexpected-eof (e)
                    (declare (ignore e))
                    (error "Expected double quote to end before EOF")))))

    (make-instance 'double-quote :parts (vector token))))

(defun read-backquote (stream)
  (declare (ignore stream))
  (error 'not-implemented :feature "Backquote (it sucks and you should use $())"))

(defgeneric command-word-tokens (command-word)
  (:documentation
   "Retrieve the tokens contained within a command word.

Command words are tokens that contain entire shell commands.  This
retrieves the token sequence for the inner command."))

(define-data command-word (a-word)
  ((tokens
    :initarg :tokens
    :initform (required)
    :type vector
    :updater command-word-tokens)
   (evaluate-fn
    :initform nil
    :updater command-word-evaluate-fn))
  (:documentation
   "A class to represent uses of command substitution.

During the expansion phase, this token will expand to be the output of
some other command.  See `command-word-tokens' and
`command-word-evaluate-fn'."))
(defmethod print-object ((command-word command-word) stream)
  (print-unreadable-object (command-word stream :type t)
    (format stream "~W" (command-word-tokens command-word))))

(defun read-dollar-paren (stream)
  ;; We need to parse a full command.  The best way to do that is with
  ;; our `NEXT-TOKEN' function, but that returns tokens instead of
  ;; strings.  We need a string containing everything it read!
  ;; Luckily, an echo stream allows us to know exactly what was read
  ;; by `NEXT-TOKEN'.
  (let ((out-stream (make-string-output-stream)))
    (format out-stream "$(")
    (let* ((echo-stream (make-echo-stream stream out-stream))
           (token-iterator (token-iterator echo-stream :readtable (shell-lexer-context-readtable stream)))
           (tokens (make-extensible-vector)))

      (do-iterator (token token-iterator)
        (when (typep token 'rparen)
          (return-from read-dollar-paren (make-instance 'command-word :tokens tokens :value (get-output-stream-string out-stream))))
        (vector-push-extend token tokens))
      (eof-error :comment ") expected")))
  (assert nil nil "This function doesn't return normally"))

(defun read-dollar-paren-paren (stream)
  (error 'not-implemented :feature "$(())")
  (let ((next-char (peek-char nil stream nil :eof))
        (result (make-extensible-vector :element-type 'character)))
    (labels ((take ()
               (vector-push-extend next-char result)
               (read stream nil :eof)
               (setf next-char (peek-char nil stream nil :eof))))
      (assert (equal #\( next-char))
      (take))))

(defun read-name (stream &key error-on-invalid-name-p greedy-digits)
  (let* ((next-char (peek-char nil stream nil :eof))
         (result (make-extensible-vector :element-type 'character)))
    (labels ((any-valid ()
               (or (equal #\_ next-char)
                   (alpha-char-p next-char)
                   (digit-char-p next-char)))
             (take ()
               (vector-push-extend next-char result)
               (read-char stream nil :eof)
               (setf next-char (peek-char nil stream nil :eof))))
      (cond
        ((or (find next-char #(#\@ #\* #\# #\? #\- #\$ #\!))
             (and (not greedy-digits) (digit-char-p next-char)))
         (string (read-char stream)))

        ((and greedy-digits (digit-char-p next-char))
         (loop :do
            (progn
              (vector-push-extend next-char result)
              (read-char stream)
              (setf next-char (peek-char nil stream nil :eof)))
            :while (and (not (eq :eof next-char)) (digit-char-p next-char)))
         result)

        ((any-valid)
         (assert (not (digit-char-p next-char)))
         (loop
            :do (take)
            :while (and (not (eq :eof next-char)) (any-valid)))
         result)

        (error-on-invalid-name-p
         (error "Names can't start with ~S" next-char))

        (t
         nil)))))

(defgeneric variable-expansion-word-variable (token)
  (:documentation
   "Return the variable name that the given token describes access to."))


(defgeneric variable-expansion-word-length-p (token)
  (:documentation
   "Returns non-nil if the length of the variable's value should be accessed."))

(define-data variable-expansion-word (a-word)
  ((variable
    :initarg :variable
    :initform (required)
    :reader variable-expansion-word-variable
    :type string)
   (length-p
    :initarg :length-p
    :initform nil
    :reader variable-expansion-word-length-p
    :type boolean))
  (:documentation
   "A class to represent tokens where a shell variable is being accessed."))
(defmethod print-object ((word variable-expansion-word) stream)
  (print-unreadable-object (word stream :type t)
    (format stream "~S" (variable-expansion-word-variable word))
    (when (variable-expansion-word-length-p word)
      (format stream " :length-p t"))))

(defclass dollar-curly-lexer-context (lexer-context)
  ((base-token
    :initarg :base-token
    :initform (required)
    :accessor dollar-curly-lexer-context-base-token
    :type variable-expansion-word)
   (shell-readtable
    :initarg :shell-readtable
    :initform (required)
    :reader dollar-curly-lexer-context-shell-readtable))
  (:documentation
   "A class to represent the state of the lexer while reading a
variable substitution."))

(defun handle-dollar-curly (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (labels
      ((get-name-token ()
         (when (equal #\# (peek-char nil stream nil :eof))
           (read-char stream nil :eof)
           (let ((name (read-name stream :greedy-digits t :error-on-invalid-name-p nil)))
             (return-from get-name-token
               (if name
                   (make-instance 'variable-expansion-word :variable name :length-p t)
                   (make-instance 'variable-expansion-word :variable "#")))))

         (let ((name (read-name stream :greedy-digits t :error-on-invalid-name-p t)))
           (make-instance 'variable-expansion-word :variable name))))

    (let* ((base-token (get-name-token))
           (readtable (subtable (shell-lexer-context-readtable stream) #(variable-expansion-policy)))
           (inner-context (make-instance 'dollar-curly-lexer-context
                                         :stream stream
                                         :shell-readtable (shell-lexer-context-readtable stream)
                                         :base-token base-token)))
      (dispatch-table-read inner-context readtable nil))))

(defun read-comment (stream)
  (let ((next-char (peek-char nil stream nil :eof)))
    (declare (dynamic-extent next-char))
    (labels ((done-p () (or (equal :eof next-char) (equal #\linefeed next-char))))
      (declare (dynamic-extent #'done-p))
      (assert (equal #\# next-char))
      (loop :while (not (done-p)) :do
         (progn
           (read-char stream nil :eof)
           (setf next-char (peek-char nil stream nil :eof))))
      (assert (done-p)))))

(defun handle-quote (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (read-single-quote stream))

(defun handle-backslash (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (let ((next-char (read-char stream))) ;; Intentionally throwing an error on EOF...
    (unless (equal next-char #\Linefeed)
      (make-escaped-character next-char))))

(defun handle-double-quote (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (read-double-quote stream))

(defun handle-dollar (stream initiation-sequence)
  (let (name)
    (cond
      ((setf name (read-name stream :error-on-invalid-name-p nil))
       (make-instance 'variable-expansion-word
                      :variable name
                      :value (concatenate 'string "$" name)))

      (t
       initiation-sequence))))

(defun handle-dollar-paren (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (read-dollar-paren stream))

(defun handle-dollar-paren-paren (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (read-dollar-paren-paren stream))

(defun handle-backtick (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (read-backquote stream))

(defun handle-double-quote-backslash (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (string (read-char stream)))

(defun handle-double-quote-backslash-newline (stream initiation-sequence)
  (declare (ignore stream initiation-sequence))
  nil)

(defun handle-double-quote-termination (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (close stream)
  nil)

(defun handle-add-next-char (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (string (read-char stream)))

(defun generic-policy-handler (stream initiation-sequence class)
  (let* ((include-empty-string-p (equal #\: (aref initiation-sequence 0)))
         (policy-readtable (subtable (dollar-curly-lexer-context-shell-readtable stream) #(variable-expansion-word)))
         (inner-context
          (make-instance 'shell-lexer-context
                         :stream stream
                         :readtable (dollar-curly-lexer-context-shell-readtable stream)))
         (replacement-token (shell-lexer-context-extensible-read-loop inner-context policy-readtable))
         (base-token (dollar-curly-lexer-context-base-token stream)))
    (make-instance class
                   :variable (variable-expansion-word-variable base-token)
                   :length-p (variable-expansion-word-length-p base-token)
                   :replacement-token replacement-token
                   :include-empty-string-p include-empty-string-p)))

(defgeneric conditional-replacement-policy-include-empty-string-p (policy)
  (:documentation
   "Returns non-nil if the empty string should be covered by this
policy.

The exact meaning of the return value is defined by the policy.  For
POSIX-standard-defined variable expansion policies, a non-nil return
value indicates the use of the #\: character.  For example, POSIX
specifies that variable access of the form \"${FOO:-default}\" should
expand to \"default\" if FOO is either unset or set to the empty
string.  Normally, the #\- expansion
policy (a.k.a. `default-value-policy') would only expand to
\"default\" if FOO is unset.  The presence of the #\: character
indicates that the empty string should be covered as well."))

(defgeneric conditional-replacement-policy-replacement-token (policy)
  (:documentation
   "Returns the token that should be used instead when this expansion
policy decides not to use the variable value.

The exact meaning of the return value is defined by the policy.  For
the POSIX-standard-defined expansion policies, the returned token
represents the content between the policy character (i.e. #\-, #\=,
#\?, or #\+) and the closing curly brace.  For example, in the case of
the #\- policy (a.k.a `default-value-policy'), this represents the
value to be used if the variable is unset."))

(define-data conditional-replacement-policy (variable-expansion-word)
  ((replacement-token
    :initarg :replacement-token
    :reader conditional-replacement-policy-replacement-token
    :initform (required))
   (include-empty-string-p
    :initarg :include-empty-string-p
    :reader conditional-replacement-policy-include-empty-string-p
    :initform nil))
  (:documentation
   "A base class to represent variable expansion with a replacement policy.

This token class represents access to a variable with some extra rules
on top.  For example, if the variable is unset or set to the empty
string a `variable-expansion-word' will usually expand to the empty
string.  If the token were instead an instance of the
`default-value-policy' subclass, the token would expand to some
replacement token.

You don't need to use this class to represent a fancy variable
expansion.

For examples of conditional expansion policies, see
`default-value-policy', `assign-default-value-policy', `error-policy',
and `alternate-value-policy'."))

(defmethod print-object ((policy conditional-replacement-policy) stream)
  (print-unreadable-object (policy stream :type t)
    (format stream "~S" (variable-expansion-word-variable policy))
    (when (variable-expansion-word-length-p policy)
      (format stream " :length-p t"))
    (when (conditional-replacement-policy-include-empty-string-p policy)
      (format stream " :include-empty-string-p t"))
    (format stream " :replacement-token ~S"
            (conditional-replacement-policy-replacement-token policy))))

(define-data default-value-policy (conditional-replacement-policy)
  ()
  (:documentation
   "A token class to represent variable expansion with a default value.

This class represents variable expansion that uses the #\- policy.
For example, \"${FOO-default value}\"."))

(define-data assign-default-value-policy (conditional-replacement-policy)
  ()
  (:documentation
   "A token class to represent variable expansion with a default
value that should be used and then stored in the variable.

This class represents variable expansion that uses the #\= policy.
For example, \"${FOO=default value to assign}\"."))

(define-data error-policy (conditional-replacement-policy)
  ()
  (:documentation
   "A token class to represent variable expansion with an error message.

This class represents variable expansion that uses the #\? policy.
For example, \"${FOO?Message to display if unset}\"."))

(define-data alternate-value-policy (conditional-replacement-policy)
  ()
  (:documentation
   "A token class to represent variable expansion with an error message.

This class represents variable expansion that uses the #\+ policy.
For example, \"${FOO+Alternate value to use}\"."))

(defun handle-use-default-policy (stream initiation-sequence)
  (generic-policy-handler stream initiation-sequence 'default-value-policy))

(defun handle-assign-default-policy (stream initiation-sequence)
  (generic-policy-handler stream initiation-sequence 'assign-default-value-policy))

(defun handle-error-policy (stream initiation-sequence)
  (generic-policy-handler stream initiation-sequence 'error-policy))

(defun handle-alternate-policy (stream initiation-sequence)
  (generic-policy-handler stream initiation-sequence 'alternate-value-policy))

(defun end-variable-expansion-word-with-policy (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (close stream)
  nil)

(defun end-variable-expansion-word-policy-free (stream initiation-sequence)
  (declare (ignore initiation-sequence))
  (close stream)
  (dollar-curly-lexer-context-base-token stream))

(defparameter *quote-table*
  (as-> *empty-dispatch-table* x
    (with-handler x "'" 'handle-quote)
    (with-handler x "\\" 'handle-backslash)
    (with-handler x "\"" 'handle-double-quote)))

(defparameter *substitution-table*
  (as-> *empty-dispatch-table* x
    (with-dispatch-character x "$")
    (with-default-handler x "$" 'handle-dollar)
    (with-dispatch-character x "${")
    (with-default-handler x "${" 'handle-dollar-curly)
    (with-dispatch-character x "$(")
    (with-default-handler x "$(" 'handle-dollar-paren)
    (with-handler x "$((" 'handle-dollar-paren-paren)
    (with-handler x "`" 'handle-backtick)))

(defparameter *double-quote-readtable*
  (as-> *empty-dispatch-table* x
    (use-table x *substitution-table*)
    (with-default-handler x "" 'handle-add-next-char)
    (with-dispatch-character x "\\")
    (with-default-handler x "\\" 'handle-double-quote-backslash)
    (with-handler x #(#\\ #\Linefeed) 'handle-double-quote-backslash-newline)
    (with-handler x "\"" 'handle-double-quote-termination)))

(defparameter *variable-expansion-policy*
  (let ((policies
         (as-> *empty-dispatch-table* x
           (with-handler x "-" 'handle-use-default-policy)
           (with-handler x "=" 'handle-assign-default-policy)
           (with-handler x "?" 'handle-error-policy)
           (with-handler x "+" 'handle-alternate-policy))))
    (as-> *empty-dispatch-table* x
      (with-dispatch-character x ":" :use-table policies)
      (use-table x policies)
      (with-handler x "}" 'end-variable-expansion-word-policy-free))))

(defparameter *variable-expansion-word*
  (as-> *empty-dispatch-table* x
    (use-table x *quote-table*)
    (use-table x *substitution-table*)
    (with-default-handler x "" 'handle-add-next-char)
    (with-handler x "}" 'end-variable-expansion-word-with-policy)))

(defparameter *standard-shell-readtable*
  (as-> *empty-dispatch-table* x
    (use-table x *quote-table*)
    (use-table x *substitution-table*)
    (with-dispatch-character x #(double-quote) :use-table *double-quote-readtable*)
    (with-dispatch-character x #(variable-expansion-policy) :use-table *variable-expansion-policy*)
    (with-dispatch-character x #(variable-expansion-word) :use-table *variable-expansion-word*)))

(defun standard-shell-readtable ()
  "The readtable for run-of-the-mill shell syntax.

This readtable only describes part of the behavior for shell syntax.
The rest is handled by `next-token'."
  *standard-shell-readtable*)

(defun token-iterator-symbolic-readtable (stream readtable-sym)
    "Given a stream and a symbol whose value is a readtable, return an
iterator that produces the tokens found in the stream.

The value of `readtable-sym' will be re-read every time a new token is
needed."
  (make-iterator ()
    (let ((token (next-token stream :readtable (symbol-value readtable-sym))))
      (when (typep token 'eof)
        (stop))
      (emit token))))

(defun token-iterator (stream &key (readtable (standard-shell-readtable)))
  "Given a stream and a readtable, return an iterator that produces
the tokens found in the stream."
  (make-iterator ()
    (let ((token (next-token stream :readtable readtable)))
      (when (typep token 'eof)
        (stop))
      (emit token))))

(defun tokens-in-string (string &key (readtable (standard-shell-readtable)))
  "Return a vector containing the tokens present in the given string.

See `next-token'."
  (tokens-in-stream
   (make-instance 'shcl/core/positional-stream:positional-input-stream
                  :underlying-stream (make-string-input-stream string))
   :readtable readtable))

(defun tokens-in-stream (stream &key (readtable (standard-shell-readtable)))
  "Return a vector containing the tokens present in the given stream.

See `next-token'."
  (iterator-values (token-iterator stream :readtable readtable)))

(defgeneric lexer-context-first-consumed-character-position (context))

(defgeneric lexer-context-consume-p (context)
  (:documentation
   "Returns non-nil iff the lexer context will consume the next character.

If a character is consumed then it is included in the `token-value' of
the token that is ultimately delimited."))

(defclass lexer-context (trivial-gray-streams:fundamental-character-input-stream)
  ((all-chars
    :type string
    :initform (make-extensible-vector :element-type 'character))
   (consumed-chars
    :type string
    :initform (make-extensible-vector :element-type 'character))
   (consume-p
    :type boolean
    :initform t
    :accessor lexer-context-consume-p)
   (last-char-status
    :initform nil)
   (raw-stream
    :type stream
    :initarg :stream
    :initform (required))
   (end-marked
    :type boolean
    :initform nil
    :reader lexer-context-end-marked-p)
   (starting-position
    :type (or null position-record))
   (first-consumed-character-position
    :type (or null position-record)))
  (:documentation
   "A class to contain the state for a generic lexer."))

(defmethod shared-initialize :after ((instance lexer-context) slots &rest args &key &allow-other-keys)
  (declare (ignore args slots))
  (with-slots (raw-stream starting-position) instance
    (unless (slot-boundp instance 'starting-position)
      (setf starting-position (make-position-record-from-positional-stream raw-stream)))))

(defmethod print-object ((lc lexer-context) stream)
  (print-unreadable-object (lc stream :type t :identity t)))

(defmethod lexer-context-first-consumed-character-position ((context lexer-context))
  (with-slots (first-consumed-character-position) context
    (if (slot-boundp context 'first-consumed-character-position)
        first-consumed-character-position
        nil)))

(defmethod make-position-record-from-positional-stream ((stream lexer-context))
  (with-slots (raw-stream) stream
    (make-position-record-from-positional-stream raw-stream)))

(defun lexer-context-read-char (stream hang-p)
  (with-slots
        (raw-stream all-chars consumed-chars last-char-status
                    first-consumed-character-position)
      stream
    (let* ((consume-p (lexer-context-consume-p stream))
           (needs-position-record
            (and
             (not (slot-boundp stream 'first-consumed-character-position))
             consume-p))
           (position-record
            (when needs-position-record
              (make-position-record-from-positional-stream raw-stream)))
           (char (if hang-p
                     (read-char raw-stream nil :eof)
                     (read-char-no-hang raw-stream nil :eof))))
      (when needs-position-record
        (if (eq char :eof)
            (setf first-consumed-character-position nil)
            (setf first-consumed-character-position position-record)))
      (cond
        ((and (not hang-p)
              (not char)))
        ((eq char :eof)
         (setf last-char-status nil))
        (consume-p
         (vector-push-extend char all-chars)
         (vector-push-extend char consumed-chars)
         (setf last-char-status 'consumed))
        (t
         (vector-push-extend char all-chars)
         (setf last-char-status 'not-consumed)))
      char)))

(defmethod trivial-gray-streams:stream-read-char ((stream lexer-context))
  (lexer-context-read-char stream t))

(defmethod trivial-gray-streams:stream-unread-char ((stream lexer-context) char)
  (labels
      ((unread-from (str char)
         (assert (<= 1 (length str)))
         (assert (equal char (aref str (1- (length str)))))
         (vector-pop str)))
    (with-slots (all-chars consumed-chars last-char-status raw-stream) stream
      (prog1 (unread-char char raw-stream)
        (ecase last-char-status
          (consumed
           (unread-from consumed-chars char))
          (not-consumed)
          (nil
           (error "Cannot unread that far")))
        (setf last-char-status nil)
        (unread-from all-chars char)))))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream lexer-context))
  (lexer-context-read-char stream nil))

(defmethod trivial-gray-streams:stream-peek-char ((stream lexer-context))
  (with-slots (raw-stream) stream
    (peek-char nil raw-stream nil :eof)))

(defmethod trivial-gray-streams:stream-listen ((stream lexer-context))
  (with-slots (raw-stream) stream
    (listen raw-stream)))

(defgeneric shell-lexer-context-readtable (context)
  (:documentation
   "Return the readtable that is being used for the given lexer context."))

(defclass shell-lexer-context (lexer-context)
  ((pending-word
    :type (or null string)
    :initform nil)
   (parts
    :type array
    :initform (make-extensible-vector))
   (readtable
    :type dispatch-table
    :initarg :readtable
    :initform (required)
    :reader shell-lexer-context-readtable))
  (:documentation
   "A class that contains the state of the lexer for the POSIX shell
language.

This class is used by `next-token' when it tries to read a token from
a stream.  See `next-token'.

When it is time to delimit a token, this class automatically picks a
token class based on the content it has been provided using
`shell-lexer-context-add-part'."))

(defmethod print-object ((lc shell-lexer-context) stream)
  (with-slots (pending-word parts) lc
    (print-unreadable-object (lc stream :type t :identity t)
      (format stream " parts ~W pending-word ~W>" parts pending-word))))

(defun shell-lexer-context-add-pending-word (context)
  "Ensure that the `pending-word' slot of the lexer context is ready
for use."
  (with-slots (pending-word parts) context
    (when pending-word
      (return-from shell-lexer-context-add-pending-word))
    (setf pending-word (make-extensible-vector :element-type 'character))))

(defun shell-lexer-context-no-content-p (context)
  "Returns non-nil iff the lexer context hasn't read any real content
yet.

The lexer context may have consumed some characters, but if this
returns non-nil then it hasn't yet consumed characters that would be
part of a token."
  (with-slots (parts pending-word) context
    (and (equal 0 (length parts))
         (equal 0 (length pending-word)))))

(defun shell-lexer-context-assignment-p (context)
  "Returns non-nil if the current token content looks like it might be
a variable assignment expression."
  (with-slots (parts pending-word) context
    (if (equal 0 (length parts))
        (assignment-word-p pending-word)
        (and (typep (aref parts 0) 'simple-word)
             (assignment-word-p (simple-word-text (aref parts 0)))))))

(defun shell-lexer-context-add-chars (context chars)
  "Add the given characters to the lexer context's pending word."
  (shell-lexer-context-add-pending-word context)
  (with-slots (pending-word) context
    (loop :for char :across chars :do
       (vector-push-extend char pending-word))))

(defun shell-lexer-context-extend-word (context)
  "Consume the next character and add it to the pending word."
  (shell-lexer-context-add-chars context (string (read-char context))))

(defun shell-lexer-context-word-boundary (context)
  "Take the context's current pending word and add it to the vector of
token parts."
  (with-slots (parts pending-word) context
    (when pending-word
      (vector-push-extend (make-instance 'simple-word :text pending-word) parts)
      (setf pending-word nil))))

(defun shell-lexer-context-add-part (context part)
  "Add content to the end of the token that the lexer context is building.

If `part' is a string, then the characters are given to the lexer
context to deal with as it pleases.  When it is time to delimit a
token, adjacent characters will be combined into a single token.
Depending on the character sequence, the class of that token may
varry.  For example, if you provide the string content \"FOO=bar\",
then the token class may be `assignment-word'.

If `part' is any other type, it is assumed to be a token.  Tokens will
not be combined with each other or adjacent string content.

See `shell-lexer-context-delimit'."
  (check-type part (or string token null))
  (unless part
    (return-from shell-lexer-context-add-part
      (values)))
  (when (stringp part)
    (shell-lexer-context-add-chars context part)
    (return-from shell-lexer-context-add-part
      (values)))
  (shell-lexer-context-word-boundary context)
  (with-slots (parts) context
    (vector-push-extend part parts))
  (values))

(defun shell-lexer-context-simple-word (context)
  (with-slots (parts pending-word) context
    (cond
      ((equal 0 (length parts))
       pending-word)

      ((and (equal 1 (length parts))
            (not pending-word)
            (typep (aref parts 0) 'simple-word))
       (simple-word-text (aref parts 0)))

      (t
       nil))))

(defmethod close ((stream lexer-context) &key abort)
  (declare (ignore abort))
  (with-slots (raw-stream end-marked) stream
    (setf raw-stream (make-string-input-stream ""))
    (setf end-marked t)))

(defmethod open-stream-p ((stream lexer-context))
  (with-slots (end-marked) stream
    (not end-marked)))

(defun shell-lexer-context-delimit (context &key (if-empty :error))
  "Produce a token containing all the parts previously provided.

This function should be called at most once on a given context object.
If the lexer context has no content, then the `if-empty' parameter
controls the behavior of this function.  `if-empty' can be either
`:error' or `nil'.

See `shell-lexer-context-add-part'."
  (unless (or (eq if-empty :error) (eq if-empty nil))
    (error "if-empty arg must be :error or nil"))

  (shell-lexer-context-word-boundary context)
  (with-slots (parts consumed-chars) context
    (let* ((part-count (length parts))
           (simple-word (shell-lexer-context-simple-word context))
           (position (lexer-context-first-consumed-character-position context))
           (initargs `(:value ,consumed-chars :position ,position)))
      (cond ((and simple-word
                  (operator-p simple-word))
             (apply 'make-operator simple-word initargs))

            ((and simple-word
                  (reserved-p simple-word))
             (apply 'make-reserved simple-word initargs))

            ((and simple-word
                  (not (find-if-not #'digit-char-p simple-word))
                  (or (equal #\< (peek-char nil context nil :eof))
                      (equal #\> (peek-char nil context nil :eof))))
             (apply 'make-instance 'io-number :fd (parse-integer simple-word) initargs))

            ((and simple-word
                  (name-p simple-word))
             (apply 'make-instance 'name :text simple-word initargs))

            ((shell-lexer-context-assignment-p context)
             (apply 'make-assignment-word-from-parts parts initargs))

            ((equal 1 part-count)
             (let ((part (aref parts 0)))
               (setf (token-value part) consumed-chars)
               (setf (token-position part) position)
               part))

            ((< 1 part-count)
             (apply 'make-instance 'compound-word :parts parts initargs))

            ((equal 0 part-count)
             (when (eq :error if-empty)
               (error "Empty token detected")))

            (t
             (error "All cases should be covered above"))))))

(defun handle-extensible-syntax (context &key (readtable (shell-lexer-context-readtable context)))
  (let* ((no-match-value '#:no-match-value)
         (result (dispatch-table-read context readtable no-match-value))
         (matched-p (not (eq result no-match-value)))
         (value (when matched-p result)))
    (when matched-p
      (shell-lexer-context-add-part context value)
      t)))

(defun next-token (stream &key (readtable (standard-shell-readtable)))
  "Read the next shell token from the given stream.

The `readtable' gives you a way to influence the tokens that this
function produces.  Many of the rules specified in the POSIX standard
are encoded in the default readtable.

The following behaviors are handled by this function specially.  You
cannot override these behaviors using `readtable'.
- Handling of EOF
- Handling of operators (e.g. redirect operators like >)
- Handling of whitespace
- Handling of comments
- Handling of all non-special characters

The remaining lexer behaviors specified by POSIX are simply rules in
the standard shell readtable.  So, for example, quotes are handled by
the readtable.  See the `:shcl/core/dispatch-table' package for more
information about readtables.  See also `standard-shell-readtable'.
The context object passed to readtable handlers is an instance of
`shell-lexer-context'.

Note that SHCL's lexer deviates from the POSIX standard.  For example,
POSIX says that the lexer should produce tokens that includes the
quote characters.  POSIX says that quote characters should remain in
the token until much further along in the process of evaluating a
command.  SHCL discards quote characters immediately and instead
returns a token whose type attests to the fact that the characters
were quoted.  A similar thing happens for command substitution,
variable expansion, etc."
  (let ((context (make-instance 'shell-lexer-context :stream stream :readtable readtable)))
    (loop
       (block again
         (labels ((next-char () (peek-char nil context nil :eof))
                  (again () (return-from again))
                  (delimit () (return-from next-token (shell-lexer-context-delimit context))))
           (cond
             ;; If the end of input is recognized, the current token
             ;; shall be delimited. If there is no current token, the
             ;; end-of-input indicator shall be returned as the token.
             ((eq :eof (next-char))
              (when (shell-lexer-context-no-content-p context)
                (shell-lexer-context-add-part context *eof*))
              (delimit))

             ;; If the previous character was used as part of an
             ;; operator and the current character is not quoted and
             ;; can be used with the current characters to form an
             ;; operator, it shall be used as part of that (operator)
             ;; token.
             ((let ((simple-word (shell-lexer-context-simple-word context)))
                (and simple-word
                     (operator-p simple-word)
                     (operator-p (concatenate 'string simple-word (string (next-char))))))
              (shell-lexer-context-extend-word context)
              (again))

             ;; If the previous character was used as part of an
             ;; operator and the current character cannot be used with
             ;; the current characters to form an operator, the
             ;; operator containing the previous character shall be
             ;; delimited.
             ((let ((simple-word (shell-lexer-context-simple-word context)))
                (and simple-word
                     (operator-p simple-word)
                     (not (operator-p (concatenate 'string simple-word (string (next-char)))))))
              (delimit))

             ((handle-extensible-syntax context)
              (again))

             ;; If the current character is not quoted and can be used
             ;; as the first character of a new operator, the current
             ;; token (if any) shall be delimited. The current
             ;; character shall be used as the beginning of the next
             ;; (operator) token.
             ((operator-p (string (next-char)))
              (unless (shell-lexer-context-no-content-p context)
                (delimit))
              (shell-lexer-context-extend-word context)
              (again))

             ;; If the current character is an unquoted <newline>, the
             ;; current token shall be delimited.
             ((equal #\linefeed (next-char))
              (when (shell-lexer-context-no-content-p context)
                (read-char context nil :eof)
                (shell-lexer-context-add-part context *newline*))
              (delimit))

             ;; If the current character is an unquoted <blank>, any
             ;; token containing the previous character is delimited
             ;; and the current character shall be discarded.
             ((whitespace-p (next-char))
              (unless (shell-lexer-context-no-content-p context)
                (delimit))
              (let ((consume-p (lexer-context-consume-p context)))
                (setf (lexer-context-consume-p context) nil)
                (read-char context nil :eof)
                (setf (lexer-context-consume-p context) consume-p))
              (again))

             ;; If the previous character was part of a word, the
             ;; current character shall be appended to that word.
             ((not (shell-lexer-context-no-content-p context))
              (shell-lexer-context-extend-word context)
              (again))

             ;; If the current character is a '#', it and all
             ;; subsequent characters up to, but excluding, the next
             ;; <newline> shall be discarded as a comment. The
             ;; <newline> that ends the line is not considered part of
             ;; the comment.
             ((equal #\# (next-char))
              (read-comment stream)
              (again))

             ;; The current character is used as the start of a new
             ;; word.
             (t
              (shell-lexer-context-extend-word context)
              (again))))))))
