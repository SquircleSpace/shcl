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
   :common-lisp :shcl/core/utility :shcl/core/shell-readtable
   :shcl/core/iterator)
  (:import-from :fset)
  (:import-from :closer-mop)
  (:export
   ;; Token classes
   #:token #:a-word #:eof #:simple-word #:compound-word
   #:assignment-word #:name #:io-number #:literal-token #:newline
   #:reserved-word #:single-quote #:double-quote #:command-word
   #:variable-expansion-word #:variable-expansion-length-word

   ;; Slot accessors
   #:token-value #:simple-word-text #:compound-word-parts
   #:assignment-word-name #:assignment-word-value-word #:io-number-fd
   #:literal-token-string #:single-quote-contents #:double-quote-parts
   #:command-word-tokens #:command-word-evaluate-fn
   #:variable-expansion-word-variable #:variable-expansion-length-word-variable

   ;; Operators
   #:and-if #:or-if #:dsemi #:dless #:dgreat #:lessand #:greatand
   #:lessgreat #:dlessdash #:clobber #:semi #:par #:pipe #:lparen
   #:rparen #:great #:less

   ;; Reserved words
   #:if-word #:then #:else #:elif #:fi #:do-word #:done #:case-word #:esac
   #:while #:until #:for #:lbrace #:rbrace #:bang #:in

   ;; Functions
   #:tokenize #:token-iterator #:tokens-in-string #:tokens-in-stream
   #:next-token #:token-iterator-symbolic-readtable

   ;; Extensible reading
   #:lexer-context-mark-end-of-token #:shell-lexer-context-add-part
   #:shell-lexer-context-delimit #:shell-lexer-context
   #:lexer-context-shell-extensible-read #:lexer-context-readtable
   #:lexer-context-stream #:+standard-shell-readtable+))
(in-package :shcl/core/lexer)

(optimization-settings)

(defclass token ()
  ((value :type (or null string)
          :initform nil
          :accessor token-value
          :initarg :value)))
(defmethod print-object ((token token) stream)
  (format stream "#<~A ~W>" (class-name (class-of token)) (token-value token)))
(defmethod make-load-form ((token token) &optional environment)
  (let ((slots (mapcar 'closer-mop:slot-definition-name (closer-mop:class-slots (class-of token)))))
    (make-load-form-saving-slots token :slot-names slots :environment environment)))

(defclass eof (token)
  ((value :initform "<EOF>")))
(defmethod print-object ((eof eof) stream)
  (format stream "#<EOF>"))
(define-once-global +eof+ (make-instance 'eof))

(defun plusify (symbol)
  (intern (concatenate 'string "+" (symbol-name symbol) "+")))

(defclass a-word (token)
  ())

(defclass simple-word (a-word)
  ((text
    :initarg :text
    :accessor simple-word-text
    :initform (required)
    :type string)))
(defmethod print-object ((simple-word simple-word) stream)
  (format stream "#<~A ~S>" (class-name (class-of simple-word)) (simple-word-text simple-word)))

(defclass compound-word (a-word)
  ((parts :type vector
          :initform (required)
          :accessor compound-word-parts
          :initarg :parts)))
(defmethod print-object ((compound-word compound-word) stream)
  (format stream "#<~A ~S>" (class-name (class-of compound-word)) (compound-word-parts compound-word)))

(defclass assignment-word (a-word)
  ((name
    :type name
    :initform (required)
    :accessor assignment-word-name
    :initarg :name)
   (value-word
    :type a-word
    :initarg :value-word
    :initform (required)
    :accessor assignment-word-value-word)))
(defmethod print-object ((word assignment-word) stream)
  (format stream "#<~A ~S = ~S>" (class-name (class-of word)) (assignment-word-name word) (assignment-word-value-word word)))

(defun make-assignment-word-from-parts (parts raw)
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
      (make-instance 'assignment-word :name name :value-word final-value :value raw))))

(defclass name (simple-word)
  ())

(defclass io-number (token)
  ((fd
    :type integer
    :initform (required)
    :accessor io-number-fd
    :initarg :fd)))
(defmethod print-object ((io-number io-number) stream)
  (format stream "#<~A ~S>" (class-name (class-of io-number)) (io-number-fd io-number)))

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

(defparameter *print-literals-by-name* t)
(defclass literal-token (token)
  ((string
    :type string
    :allocation :class
    :accessor literal-token-string)))
(defmethod print-object ((literal-token literal-token) stream)
  (if *print-literals-by-name*
      (format stream "#<~A>" (class-name (class-of literal-token)))
      (format stream "#<LITERAL-TOKEN ~W>" (token-value literal-token))))

(defmacro define-literal-token (name string &optional (superclasses '(literal-token)) slots &body options)
  `(progn
     (defclass ,name ,superclasses
       ((value :initform ,string)
        (string :initform ,string)
        ,@slots)
       ,@options)))

(define-literal-token newline (string #\linefeed))
(define-once-global +newline+ (make-instance 'newline))

(defparameter *operators* (fset:empty-map))

(defclass operator (literal-token)
  ())

(defmacro define-operator (name string &optional (superclasses '(operator)) slots &body options)
  (check-type name symbol)
  (check-type string string)
  `(progn
     (define-literal-token ,name ,string ,superclasses ,slots ,@options)
     (fset:adjoinf *operators* ,string ',name)))

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

(defun make-operator (string raw)
  (make-instance (fset:lookup *operators* string) :value raw))

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

(defclass reserved-word (a-word literal-token)
  ())

(defmacro define-reserved-word (name string &optional (superclasses '(reserved-word)) slots &body options)
  (check-type name symbol)
  (check-type string string)
  `(progn
     (define-literal-token ,name ,string ,superclasses ,slots ,@options)
     (fset:adjoinf *reserved-words* ,string ',name)))

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

(defun make-reserved (string raw)
  (make-instance (fset:lookup *reserved-words* string) :value raw))

(defun reserved-p (word)
  (fset:lookup *reserved-words* word))

(define-condition eof-error (error)
  ((comment :initarg :comment
            :initform "Unknown context"
            :accessor eof-comment)))
(defun eof-error (&rest rest)
  (apply 'error 'eof-error rest))

(defclass single-quote (a-word)
  ((contents
    :initarg :contents
    :initform (required)
    :type string
    :accessor single-quote-contents)))
(defmethod print-object ((single-quote single-quote) stream)
  (format stream "#<~A ~S>" (class-name (class-of single-quote)) (single-quote-contents single-quote)))

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

(defclass escaped-character (single-quote)
  ())

(defun make-escaped-character (char)
  (make-instance 'escaped-character :contents (string char) :value (format nil "~C~C" #\\ char)))

(defclass double-quote (a-word)
  ((parts :type vector
          :initform (required)
          :accessor double-quote-parts
          :initarg :parts)))
(defmethod print-object ((double-quote double-quote) stream)
  (format stream "#<~A ~S>" (class-name (class-of double-quote)) (double-quote-parts double-quote)))

(define-condition unexpected-eof (error)
  ())

(defun lexer-context-extensible-read-loop (context readtable)
  "Read using the given context's readtable until end is marked.

EOF results in the `unexpected-eof' error being signaled."
  (loop
     :while (not (lexer-context-end-marked-p context)) :do
     (if (eq :eof (lexer-context-next-char context))
         (error 'unexpected-eof)
         (lexer-context-shell-extensible-read context :readtable readtable)))
  (shell-lexer-context-delimit context))

(defun read-double-quote (context)
  (let* ((stream (lexer-context-stream context))
         (readtable (subtable (lexer-context-readtable context) #(double-quote)))
         (inner-context
          (make-instance 'shell-lexer-context :stream stream :readtable (lexer-context-readtable context)))
         (token (handler-case
                    (lexer-context-extensible-read-loop inner-context readtable)
                  (unexpected-eof (e)
                    (declare (ignore e))
                    (error "Expected double quote to end before EOF")))))

    (make-instance 'double-quote :parts (vector token))))

(defun read-backquote (stream)
  (declare (ignore stream))
  (error 'not-implemented :feature "Backquote (it sucks and you should use $())"))

(defclass command-word (a-word)
  ((tokens
    :initarg :tokens
    :initform (required)
    :type vector
    :accessor command-word-tokens)
   (evaluate-fn
    :initform nil
    :accessor command-word-evaluate-fn)))
(defmethod print-object ((command-word command-word) stream)
  (format stream "#<~A ~S>" (class-name (class-of command-word)) (command-word-tokens command-word)))

(defun read-dollar-paren (context)
  ;; We need to parse a full command.  The best way to do that is with
  ;; our `NEXT-TOKEN' function, but that returns tokens instead of
  ;; strings.  We need a string containing everything it read!
  ;; Luckily, an echo stream allows us to know exactly what was read
  ;; by `NEXT-TOKEN'.
  (let ((stream (lexer-context-stream context))
        (out-stream (make-string-output-stream)))
    (format out-stream "$(")
    (let* ((echo-stream (make-echo-stream stream out-stream))
           (token-iterator (token-iterator echo-stream :readtable (lexer-context-readtable context)))
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

(defclass variable-expansion-word (a-word)
  ((variable
    :initarg :variable
    :initform (required)
    :reader variable-expansion-word-variable
    :type string)))
(defmethod print-object ((word variable-expansion-word) stream)
  (print-unreadable-object (word stream :type t)
    (format stream "~S" (variable-expansion-word-variable word))))

(defclass variable-expansion-length-word (a-word)
  ((variable
    :initarg :variable
    :initform (required)
    :reader variable-expansion-length-word-variable
    :type string)))
(defmethod print-object ((word variable-expansion-length-word) stream)
  (print-unreadable-object (word stream :type t)
    (format stream "~S" (variable-expansion-length-word-variable word))))

(defclass dollar-curly-lexer-context (lexer-context)
  ((base-token
    :initarg :base-token
    :initform (required)
    :accessor dollar-curly-lexer-context-base-token
    :type token)))

(defun handle-dollar-curly (stream initiation-sequence context)
  (declare (ignore initiation-sequence))
  (labels
      ((get-name-token ()
         (when (equal #\# (lexer-context-next-char context))
           (lexer-context-consume-character context)
           (let ((name (read-name stream :greedy-digits t :error-on-invalid-name-p nil)))
             (return-from get-name-token
               (if name
                   (make-instance 'variable-expansion-length-word :variable name)
                   (make-instance 'variable-expansion-word :variable "#")))))

         (let ((name (read-name stream :greedy-digits t :error-on-invalid-name-p t)))
           (make-instance 'variable-expansion-word :variable name))))

    (let* ((base-token (get-name-token))
           (captured-chars (make-string-output-stream))
           (wrapped-stream (make-echo-stream stream captured-chars))
           (readtable (subtable (lexer-context-readtable context) #(variable-expansion-policy)))
           (inner-context (make-instance 'dollar-curly-lexer-context
                                         :stream wrapped-stream
                                         :readtable (lexer-context-readtable context)
                                         :base-token base-token))
           (read-value (shell-extensible-read stream inner-context readtable)))
      (shell-lexer-context-add-part context read-value)
      t)))

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

(defun handle-quote (stream initiation-sequence context)
  (declare (ignore initiation-sequence))
  (shell-lexer-context-add-part context (read-single-quote stream))
  t)

(defun handle-backslash (stream initiation-sequence context)
  (declare (ignore stream initiation-sequence))
  (let ((next-char (lexer-context-next-char context)))
    (unless (equal next-char #\Linefeed)
      (shell-lexer-context-add-part context (make-escaped-character next-char))))
  (lexer-context-consume-character context)
  t)

(defun handle-double-quote (stream initiation-sequence context)
  (declare (ignore stream initiation-sequence))
  (shell-lexer-context-add-part context (read-double-quote context))
  t)

(defun handle-dollar (stream initiation-sequence context)
  (let (name)
    (cond
      ((setf name (read-name stream :error-on-invalid-name-p nil))
       (let ((part (make-instance 'variable-expansion-word
                                  :variable name
                                  :value (concatenate 'string "$" name))))
         (shell-lexer-context-add-part context part)))

      (t
       (shell-lexer-context-add-chars context initiation-sequence))))
  t)

(defun handle-dollar-paren (stream initiation-sequence context)
  (declare (ignore stream initiation-sequence))
  (shell-lexer-context-add-part context (read-dollar-paren context))
  t)

(defun handle-dollar-paren-paren (stream initiation-sequence context)
  (declare (ignore initiation-sequence))
  (shell-lexer-context-add-part context (read-dollar-paren-paren stream))
  t)

(defun handle-backtick (stream initiation-sequence context)
  (declare (ignore initiation-sequence))
  (shell-lexer-context-add-part context (read-backquote stream))
  t)

(defun handle-double-quote-backslash (stream initiation-sequence context)
  (declare (ignore stream initiation-sequence))
  (shell-lexer-context-add-chars context (string (lexer-context-consume-character context)))
  t)

(defun handle-double-quote-backslash-newline (stream initiation-sequence context)
  (declare (ignore stream initiation-sequence context))
  ;; This never happened.
  t)

(defun handle-double-quote-termination (stream initiation-sequence context)
  (declare (ignore stream initiation-sequence))
  (lexer-context-mark-end-of-token context)
  t)

(defun handle-add-next-char (stream initiation-sequence context)
  (declare (ignore stream initiation-sequence))
  (shell-lexer-context-add-chars context (string (lexer-context-consume-character context)))
  t)

(defun generic-policy-handler (context initiation-sequence class)
  (let* ((include-null-p (equal #\: (aref initiation-sequence 0)))
         (policy-readtable (subtable (lexer-context-readtable context) #(variable-expansion-word)))
         (inner-context
          (make-instance 'lexer-context
                         :stream (lexer-context-stream context)
                         :readtable (lexer-context-readtable context)))
         (token (lexer-context-extensible-read-loop inner-context policy-readtable))
         (part (make-instance class
                              :original-token (dollar-curly-lexer-context-base-token context)
                              :replacement-token token
                              :include-null-p include-null-p)))
    part))

(defclass conditional-replacement-policy (token)
  ((original-token
    :initarg :original-token
    :reader conditional-replacement-policy-original-token
    :initform (required))
   (replacement-token
    :initarg :replacement-token
    :reader conditional-replacement-policy-replacement-token
    :initform (required))
   (include-null-p
    :initarg :include-null-p
    :reader conditional-replacement-policy-include-null-p
    :initform nil)))
(defmethod print-object ((policy conditional-replacement-policy) stream)
  (print-unreadable-object (policy stream :type t)
    (when-let ((include-null-p (conditional-replacement-policy-include-null-p policy)))
      (declare (ignore include-null-p))
      (format stream ":include-null-p "))
    (format stream "~S ~S" (conditional-replacement-policy-original-token policy)
            (conditional-replacement-policy-replacement-token policy))))

(defclass default-value-policy (conditional-replacement-policy)
  ())

(defclass assign-default-value-policy (conditional-replacement-policy)
  ())

(defclass error-policy (conditional-replacement-policy)
  ())

(defclass alternate-value-policy (conditional-replacement-policy)
  ())

(defun handle-use-default-policy (stream initiation-sequence context)
  (declare (ignore stream))
  (generic-policy-handler context initiation-sequence 'default-value-policy))

(defun handle-assign-default-policy (stream initiation-sequence context)
  (declare (ignore stream))
  (generic-policy-handler context initiation-sequence 'assign-default-value-policy))

(defun handle-error-policy (stream initiation-sequence context)
  (declare (ignore stream))
  (generic-policy-handler context initiation-sequence 'error-policy))

(defun handle-alternate-policy (stream initiation-sequence context)
  (declare (ignore stream))
  (generic-policy-handler context initiation-sequence 'alternate-value-policy))

(defun end-variable-expansion-word (stream initiation-sequence context)
  (declare (ignore stream initiation-sequence))
  (lexer-context-mark-end-of-token context)
  (dollar-curly-lexer-context-base-token context))

(define-once-global +quote-table+
    (as-> *empty-shell-readtable* x
      (with-handler x "'" 'handle-quote)
      (with-handler x "\\" 'handle-backslash)
      (with-handler x "\"" 'handle-double-quote)))

(define-once-global +substitution-table+
    (as-> *empty-shell-readtable* x
      (with-dispatch-character x "$")
      (with-default-handler x "$" 'handle-dollar)
      (with-dispatch-character x "${")
      (with-default-handler x "${" 'handle-dollar-curly)
      (with-dispatch-character x "$(")
      (with-default-handler x "$(" 'handle-dollar-paren)
      (with-handler x "$((" 'handle-dollar-paren-paren)
      (with-handler x "`" 'handle-backtick)))

(define-once-global +double-quote-readtable+
    (as-> *empty-shell-readtable* x
      (use-table x +substitution-table+)
      (with-default-handler x "" 'handle-add-next-char)
      (with-dispatch-character x "\\")
      (with-default-handler x "\\" 'handle-double-quote-backslash)
      (with-handler x #(#\\ #\Linefeed) 'handle-double-quote-backslash-newline)
      (with-handler x "\"" 'handle-double-quote-termination)))

(define-once-global +variable-expansion-policy+
    (let ((policies
           (as-> *empty-shell-readtable* x
             (with-handler x "-" 'handle-use-default-policy)
             (with-handler x "=" 'handle-assign-default-policy)
             (with-handler x "?" 'handle-error-policy)
             (with-handler x "+" 'handle-alternate-policy))))
      (as-> *empty-shell-readtable* x
        (with-dispatch-character x ":" :use-table policies)
        (use-table x policies)
        (with-handler x "}" 'end-variable-expansion-word))))

(define-once-global +variable-expansion-word+
    (as-> *empty-shell-readtable* x
      (use-table x +quote-table+)
      (use-table x +substitution-table+)
      (with-default-handler x "" 'handle-add-next-char)
      (with-handler x "}" 'end-variable-expansion-word)))

(define-once-global +standard-shell-readtable+
    (as-> *empty-shell-readtable* x
      (use-table x +quote-table+)
      (use-table x +substitution-table+)
      (with-dispatch-character x #(double-quote) :use-table +double-quote-readtable+)
      (with-dispatch-character x #(variable-expansion-policy) :use-table +variable-expansion-policy+)
      (with-dispatch-character x #(variable-expansion-word) :use-table +variable-expansion-word+)))

(defun token-iterator-symbolic-readtable (stream readtable-sym)
  (make-iterator ()
    (let ((token (next-token stream :readtable (symbol-value readtable-sym))))
      (when (typep token 'eof)
        (stop))
      (emit token))))

(defun token-iterator (stream &key (readtable +standard-shell-readtable+))
  (make-iterator ()
    (let ((token (next-token stream :readtable readtable)))
      (when (typep token 'eof)
        (stop))
      (emit token))))

(defgeneric tokenize (source))

(defmethod tokenize ((stream stream))
  (tokens-in-stream stream))

(defmethod tokenize ((string string))
  (tokens-in-string string))

(defun tokens-in-string (string &key (readtable +standard-shell-readtable+))
  (tokens-in-stream (make-string-input-stream string) :readtable readtable))

(defun tokens-in-stream (stream &key (readtable +standard-shell-readtable+))
  (let ((result (make-extensible-vector)))
    (do-iterator (value (token-iterator stream :readtable readtable))
      (vector-push-extend value result))
    result))

(defclass lexer-context ()
  ((all-chars-stream
    :type stream
    :initform (make-string-output-stream))
   (stream
    :type stream
    :reader lexer-context-stream)
   (raw-stream
    :type stream
    :initarg :stream
    :initform (required))
   (end-marked
    :type boolean
    :initform nil
    :reader lexer-context-end-marked-p)
   (readtable
    :type dispatch-table
    :initarg :readtable
    :initform (required)
    :reader lexer-context-readtable)))
(defmethod print-object ((lc lexer-context) stream)
  (format stream "#<~A>" (class-name (class-of lc))))

(defmethod shared-initialize :around ((instance lexer-context) slots &rest args &key &allow-other-keys)
  (declare (ignore args))
  (with-slots (all-chars-stream stream raw-stream) instance
    (let ((before (if (slot-boundp instance 'raw-stream) raw-stream '#:before))
          (result (call-next-method))
          (after (if (slot-boundp instance 'raw-stream) raw-stream '#:after)))
      (unless (eq before after)
        (setf stream (make-echo-stream raw-stream all-chars-stream)))
      result)))

(defclass shell-lexer-context (lexer-context)
  ((pending-word
    :type (or null string)
    :initform nil)
   (parts
    :type array
    :initform (make-extensible-vector))))
(defmethod print-object ((lc shell-lexer-context) stream)
  (with-slots (pending-word parts) lc
    (format stream "#<~A parts ~A pending-word ~A>" (class-name (class-of lc)) parts pending-word)))

(defun shell-lexer-context-add-pending-word (context)
  (with-slots (pending-word parts) context
    (when pending-word
      (return-from shell-lexer-context-add-pending-word))
    (setf pending-word (make-extensible-vector :element-type 'character))))

(defun lexer-context-next-char (context)
  (with-slots (stream) context
    (peek-char nil stream nil :eof)))

(defun shell-lexer-context-no-content-p (context)
  (with-slots (parts pending-word) context
    (and (equal 0 (length parts))
         (equal 0 (length pending-word)))))

(defun shell-lexer-context-assignment-p (context)
  (with-slots (parts pending-word) context
    (if (equal 0 (length parts))
        (assignment-word-p pending-word)
        (and (typep (aref parts 0) 'simple-word)
             (assignment-word-p (simple-word-text (aref parts 0)))))))

(defun lexer-context-consume-character (context)
  (with-slots (stream) context
    (read-char stream nil :eof)))

(defun shell-lexer-context-add-chars (context chars)
  (shell-lexer-context-add-pending-word context)
  (with-slots (pending-word) context
    (loop :for char :across chars :do
       (vector-push-extend char pending-word))))

(defun shell-lexer-context-extend-word (context)
  (shell-lexer-context-add-chars context (string (lexer-context-next-char context)))
  (lexer-context-consume-character context))

(defun shell-lexer-context-word-boundary (context)
  (with-slots (parts pending-word) context
    (when pending-word
      (vector-push-extend (make-instance 'simple-word :text pending-word) parts)
      (setf pending-word nil))))

(defun shell-lexer-context-add-part (context part)
  (when (stringp part)
    (return-from shell-lexer-context-add-part
      (shell-lexer-context-add-chars context part)))
  (shell-lexer-context-word-boundary context)
  (with-slots (parts) context
    (vector-push-extend part parts)))

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

(defun lexer-context-mark-end-of-token (context)
  (with-slots (stream end-marked) context
    (setf stream (make-string-input-stream ""))
    (setf end-marked t)))

(defun shell-lexer-context-delimit (context &key (if-empty :error))
  (unless (or (eq if-empty :error) (eq if-empty nil))
    (error "if-empty arg must be :error or nil"))

  (shell-lexer-context-word-boundary context)
  (with-slots (parts all-chars-stream) context
    (let* ((part-count (length parts))
           (simple-word (shell-lexer-context-simple-word context)))
      (cond ((and simple-word
                  (operator-p simple-word))
             (make-operator simple-word (get-output-stream-string all-chars-stream)))

            ((and simple-word
                  (reserved-p simple-word))
             (make-reserved simple-word (get-output-stream-string all-chars-stream)))

            ((and simple-word
                  (not (find-if-not #'digit-char-p simple-word))
                  (or (equal #\< (lexer-context-next-char context))
                      (equal #\> (lexer-context-next-char context))))
             (make-instance 'io-number :value (get-output-stream-string all-chars-stream) :fd (parse-integer simple-word)))

            ((and simple-word
                  (name-p simple-word))
             (make-instance 'name :value (get-output-stream-string all-chars-stream) :text simple-word))

            ((shell-lexer-context-assignment-p context)
             (make-assignment-word-from-parts parts (get-output-stream-string all-chars-stream)))

            ((equal 1 part-count)
             (aref parts 0))

            ((< 1 part-count)
             (make-instance 'compound-word :parts parts :value (get-output-stream-string all-chars-stream)))

            ((equal 0 part-count)
             (when (eq :error if-empty)
               (error "Empty token detected")))

            (t
             (error "All cases should be covered above"))))))

(defun lexer-context-shell-extensible-read (context &key readtable)
  (with-slots (stream (context-readtable readtable)) context
    (unless readtable
      (setf readtable context-readtable))
    (let* ((no-match-value '#:no-match-value)
           (result (shell-extensible-read stream context readtable :no-match-value no-match-value)))
      (not (eq result no-match-value)))))

(defun handle-extensible-syntax (context)
  (lexer-context-shell-extensible-read context))

(defun next-token (stream &key (readtable +standard-shell-readtable+))
  (let* ((context (make-instance 'shell-lexer-context :stream stream :readtable readtable)))
    (labels ((next-char () (lexer-context-next-char context)))

      ;; The lexing rules depend on whether the current character
      ;; was quoted.  We will always deal with quoting upfront, and
      ;; so we can assume that the current character is never quoted
      (loop
         (block again
           (labels ((again () (return-from again))
                    (delimit () (return-from next-token (shell-lexer-context-delimit context))))
             (cond
               ;; If the end of input is recognized, the current token
               ;; shall be delimited. If there is no current token, the
               ;; end-of-input indicator shall be returned as the token.
               ((eq :eof (next-char))
                (when (shell-lexer-context-no-content-p context)
                  (shell-lexer-context-add-part context +eof+))
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
                  (lexer-context-consume-character context)
                  (shell-lexer-context-add-part context +newline+))
                (delimit))

               ;; If the current character is an unquoted <blank>, any
               ;; token containing the previous character is delimited
               ;; and the current character shall be discarded.
               ((whitespace-p (next-char))
                (unless (shell-lexer-context-no-content-p context)
                  (delimit))
                (lexer-context-consume-character context)
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
                (again)))))))))
