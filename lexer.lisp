(in-package :shcl.lexer)

(optimization-settings)

(defmacro define-make-load-form-for-class (class-name)
  (let ((object (gensym "OBJECT"))
        (environment (gensym "ENVIRONMENT"))
        (slots (gensym "SLOTS")))
    `(defmethod make-load-form ((,object ,class-name) &optional ,environment)
       (assert (eq ',class-name (class-name (class-of ,object))))
       (let ((,slots (mapcar 'closer-mop:slot-definition-name (closer-mop:class-direct-slots (find-class ',class-name)))))
         (make-load-form-saving-slots ,object :slot-names ,slots :environment ,environment)))))

(defclass token ()
  ((value :type (or null string)
          :initform nil
          :accessor token-value
          :initarg :value)))
(defmethod print-object ((token token) stream)
  (format stream "#<~A ~W>" (class-name (class-of token)) (token-value token)))
(define-make-load-form-for-class token)

(defclass eof (token)
  ((value :initform "<EOF>")))
(defmethod print-object ((eof eof) stream)
  (format stream "#<EOF>"))
(define-make-load-form-for-class eof)
(define-once-global +eof+ (make-instance 'eof))

(defun plusify (symbol)
  (intern (concatenate 'string "+" (symbol-name symbol) "+")))

(defclass a-word (token)
  ())
(define-make-load-form-for-class a-word)

(defclass simple-word (a-word)
  ((text
    :initarg :text
    :accessor simple-word-text
    :initform (required)
    :type string)))
(defmethod print-object ((simple-word simple-word) stream)
  (format stream "#<~A ~S>" (class-name (class-of simple-word)) (simple-word-text simple-word)))
(define-make-load-form-for-class simple-word)

(defclass compound-word (a-word)
  ((parts :type vector
          :initform (required)
          :accessor compound-word-parts
          :initarg :parts)))
(defmethod print-object ((compound-word compound-word) stream)
  (format stream "#<~A ~S>" (class-name (class-of compound-word)) (compound-word-parts compound-word)))
(define-make-load-form-for-class compound-word)

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
(define-make-load-form-for-class assignment-word)

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
(define-make-load-form-for-class name)

(defclass io-number (token)
  ((fd
    :type integer
    :initform (required)
    :accessor io-number-fd
    :initarg :fd)))
(defmethod print-object ((io-number io-number) stream)
  (format stream "#<~A ~S>" (class-name (class-of io-number)) (io-number-fd io-number)))
(define-make-load-form-for-class io-number)

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
(define-make-load-form-for-class literal-token)

(defmacro define-literal-token (name string &optional superclasses)
  `(progn
     (defclass ,name (,@superclasses literal-token)
       ((value :initform ,string)
        (string :initform ,string)))
     (define-make-load-form-for-class ,name)))

(define-literal-token newline (string #\linefeed))
(define-once-global +newline+ (make-instance 'newline))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *operators*
    (fset:seq
     '(and-if . "&&")
     '(or-if . "||")
     '(dsemi . ";;")
     '(dless . "<<")
     '(dgreat . ">>")
     '(lessand . "<&")
     '(greatand . ">&")
     '(lessgreat . "<>")
     '(dlessdash . "<<-")
     '(clobber . ">|")
     '(semi . ";")
     '(par . "&")
     '(pipe . "|")
     '(lparen . "(")
     '(rparen . ")")
     '(great . ">")
     '(less . "<"))))

(defun make-operator (string raw)
  (make-instance (car (fset:find string *operators* :test #'equal :key #'cdr)) :value raw))

(defun operator-p (word)
  (fset:find word *operators* :test #'equal :key #'cdr))

(defun prefix-match-p (prefix-word whole-word)
  (when (> (length prefix-word) (length whole-word))
    (return-from prefix-match-p nil))
  (loop :for w-char :across whole-word
     :for char :across prefix-word
     :do (unless (equal w-char char)
           (return-from prefix-match-p nil)))
  t)

(defun operator-prefix-p (word)
  (fset:find-if (lambda (o-word) (prefix-match-p word o-word)) *operators* :key #'cdr))

(defmacro define-literal-tokens ()
  (labels
      ((transform (pair) `(define-literal-token ,(car pair) ,(cdr pair))))
    `(progn ,@(fset:convert 'list (fset:image #'transform *operators*)))))
(define-literal-tokens)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *reserved-words*
    (fset:seq
     '(if-word . "if")
     '(then . "then")
     '(else . "else")
     '(elif . "elif")
     '(fi . "fi")
     '(do-word . "do")
     '(done . "done")
     '(case-word . "case")
     '(esac . "esac")
     '(while . "while")
     '(until . "until")
     '(for . "for")
     '(lbrace . "{")
     '(rbrace . "}")
     '(bang . "!")
     '(in . "in"))))

(defun make-reserved (string raw)
  (make-instance (car (fset:find string *reserved-words* :test #'equal :key #'cdr)) :value raw))

(defclass reserved-word (a-word)
  ())
(define-make-load-form-for-class reserved-word)

(defmacro define-reserved-words ()
  (labels
      ((transform (pair) `(define-literal-token ,(car pair) ,(cdr pair) (reserved-word))))
    `(progn ,@(fset:convert 'list (fset:image #'transform *reserved-words*)))))
(define-reserved-words)

(defun reserved-p (word)
  (fset:find word *reserved-words* :test #'equal :key #'cdr))

(defun reserved-prefix-p (word)
  (fset:find-if (lambda (r-word) (prefix-match-p word r-word)) *reserved-words*))

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
(define-make-load-form-for-class single-quote)

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
      (assert (equal next-char #\'))
      (skip)
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
(define-make-load-form-for-class escaped-character)

(defun make-escaped-character (char)
  (make-instance 'escaped-character :contents (string char) :value (format nil "~C~C" #\backslash char)))

(defclass double-quote (a-word)
  ((parts :type vector
          :initform (required)
          :accessor double-quote-parts
          :initarg :parts)))
(defmethod print-object ((double-quote double-quote) stream)
  (format stream "#<~A ~S>" (class-name (class-of double-quote)) (double-quote-parts double-quote)))
(define-make-load-form-for-class double-quote)

(defun read-double-quote (stream)
  (let ((token-value (make-extensible-vector :element-type 'character))
        (parts (make-extensible-vector :element-type 'token))
        (literal-string (make-extensible-vector :element-type 'character))
        (next-char (peek-char nil stream nil :eof)))
    (assert (equal next-char #\"))
    (labels ((keep ()
               (vector-push-extend next-char literal-string)
               (skip))
             (skip ()
               (vector-push-extend next-char token-value)
               (read-char stream nil :eof)
               (reset-next-char))
             (reset-next-char ()
               (setf next-char (peek-char nil stream nil :eof)))
             (take-literal ()
               (unless (equal 0 (length literal-string))
                 (vector-push-extend literal-string parts)
                 (setf literal-string (make-extensible-vector :element-type 'character)))))
      (skip)
      (loop
         (reset-next-char)
         (cond ((equal :eof next-char)
                (eof-error :comment "Double quote expected"))

               ((equal #\" next-char)
                (skip)
                (take-literal)
                (return-from read-double-quote (make-instance 'double-quote :parts parts :value token-value)))

               ((equal #\Backslash next-char)
                (skip)
                (if (find next-char '(#\$ #\` #\" #\Backslash #\Linefeed))
                    (keep)))

               ((equal #\` next-char)
                (take-literal)
                (let* ((backquote (read-backquote stream))
                       (backquote-string (token-value backquote)))
                  (vector-push-extend backquote parts)
                  (loop :for char :across backquote-string
                     :do (vector-push-extend char token-value))))

               ((equal #\$ next-char)
                (take-literal)
                (let* ((dollar (read-dollar stream))
                       (dollar-string (token-value dollar)))
                  (vector-push-extend dollar parts)
                  (loop :for char :across dollar-string
                     :do (vector-push-extend char token-value))))

               (t
                (keep)))))
    (assert nil nil "This function shouldn't return via this path")))

(defun read-backquote (stream)
  (declare (ignore stream))
  (error "Backquote is not implemented.  It sucks anyway.  Use $()"))

(defclass command-word (a-word)
  ((tokens
    :initarg :tokens
    :initform (required)
    :type vector
    :accessor command-word-tokens)))
(defmethod print-object ((command-word command-word) stream)
  (format stream "#<~A ~S>" (class-name (class-of command-word)) (command-word-tokens command-word)))
(define-make-load-form-for-class command-word)

(defun read-dollar-paren (stream)
  (let ((next-char (peek-char nil stream nil :eof)))
    (assert (equal #\$ next-char))
    (read-char stream nil :eof)
    (setf next-char (peek-char nil stream nil :eof))
    (assert (equal #\( next-char))
    (read-char stream nil :eof)
    (setf next-char (peek-char nil stream nil :eof))
    (when (equal #\( next-char)
      (return-from read-dollar-paren
        (read-dollar-paren-paren
         (make-concatenated-stream (make-string-input-stream "$(")
                                   stream)))))
  ;; We need to parse a full command.  The best way to do that is with
  ;; our `NEXT-TOKEN' function, but that returns tokens instead of
  ;; strings.  We need a string containing everything it read!
  ;; Luckily, an echo stream allows us to know exactly what was read
  ;; by `NEXT-TOKEN'.
  (let ((out-stream (make-string-output-stream)))
    (format out-stream "$(")
    (let* ((echo-stream (make-echo-stream stream out-stream))
           (token-iterator (token-iterator echo-stream))
           (tokens (make-extensible-vector)))

      (do-iterator (token token-iterator)
        (when (typep token 'rparen)
          (return-from read-dollar-paren (make-instance 'command-word :tokens tokens :value (get-output-stream-string out-stream))))
        (vector-push-extend token tokens))
      (eof-error ") expected")))
  (assert nil nil "This function doesn't return normally"))

(defun read-dollar-paren-paren (stream)
  (error "$(()) is not implemented")
  (let ((next-char (peek-char nil stream nil :eof))
        (result (make-extensible-vector :element-type 'character)))
    (labels ((take ()
               (vector-push-extend next-char result)
               (read stream nil :eof)
               (setf next-char (peek-char nil stream nil :eof))))
      (assert (equal #\( next-char))
      (take))))

(defun read-name (stream)
  (let ((next-char (peek-char nil stream nil :eof))
        (result (make-extensible-vector :element-type 'character)))
    (labels ((non-digit ()
               (or (equal #\_ next-char)
                   (alpha-char-p next-char)))
             (any-valid ()
               (or (non-digit)
                   (digit-char-p next-char)))
             (take ()
               (vector-push-extend next-char result)
               (read-char stream nil :eof)
               (setf next-char (peek-char nil stream nil :eof))))
      (assert (non-digit) nil "Names can't start with ~S" next-char)
      (loop :while (and (not (eq :eof next-char)) (any-valid))
         :do (take))
      result)))

(defclass variable-expansion-word (a-word)
  ((variable
    :initarg :variable
    :initform (required)
    :accessor variable-expansion-word-variable
    :type string)))
(defmethod print-object ((word variable-expansion-word) stream)
  (format stream "#<~A ~S>" (class-name (class-of word)) (variable-expansion-word-variable word)))
(define-make-load-form-for-class variable-expansion-word)

(defun read-dollar-curly (stream)
  (declare (ignore stream))
  (error "${} is not implemented"))

(defun read-dollar-word (stream)
  (assert (equal #\$ (read-char stream nil :eof)))
  (let ((next-char (peek-char nil stream nil :eof)))
    (when (or (find next-char #(#\@ #\* #\# #\? #\- #\$ #\!))
              (digit-char-p next-char))
      (read-char stream nil :eof)
      (return-from read-dollar-word
        (make-instance 'variable-expansion-word
                       :variable (string next-char)
                       :value (concatenate 'string "$" (string next-char))))))
  (let ((name (read-name stream)))
    (make-instance 'variable-expansion-word :variable name :value (concatenate 'string "$" name))))

(defun read-dollar (stream)
  (let ((next-char (peek-char nil stream nil :eof)))
    (assert (equal #\$ next-char))
    (read-char stream nil :eof)
    (setf next-char (peek-char nil stream nil :eof))
    (let ((new-stream (make-concatenated-stream (make-string-input-stream "$") stream)))
      (cond ((equal #\{ next-char)
             (read-dollar-curly new-stream))

            ((equal #\( next-char)
             (read-dollar-paren new-stream))

            (t
             (read-dollar-word new-stream))))))

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

(defun blank-p (char)
  (cl-unicode:has-binary-property char "White_Space"))

(defclass token-iterator (lookahead-iterator)
  ())

(defun token-iterator (stream)
  (let ((readtable *shell-readtable*))
    (make-iterator (:type 'token-iterator)
      (let ((*shell-readtable* readtable)
            (token (next-token stream)))
        (when (typep token 'eof)
          (stop))
        (emit token)))))

(defgeneric tokenize (source))

(defmethod tokenize ((stream stream))
  (tokens-in-stream stream))

(defmethod tokenize ((string string))
  (tokens-in-string string))

(defun tokens-in-string (string)
  (tokens-in-stream (make-string-input-stream string)))

(defun tokens-in-stream (stream)
  (let ((result (make-extensible-vector)))
    (do-iterator (value (token-iterator stream))
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
   (pending-word
    :type (or null string)
    :initform nil)
   (parts
    :type array
    :initform (make-extensible-vector))))
(defmethod print-object ((lc lexer-context) stream)
  (with-slots (pending-word parts) lc
    (format stream "#<~A parts ~A pending-word ~A>" (class-name (class-of lc)) parts pending-word)))

(defmethod shared-initialize :around ((instance lexer-context) slots &rest args &key &allow-other-keys)
  (declare (ignore args))
  (with-slots (all-chars-stream stream raw-stream) instance
    (let ((before (if (slot-boundp instance 'raw-stream) raw-stream (gensym)))
          (result (call-next-method))
          (after (if (slot-boundp instance 'raw-stream) raw-stream (gensym))))
      (unless (eq before after)
        (setf stream (make-echo-stream raw-stream all-chars-stream)))
      result)))

(defun lexer-context-add-pending-word (context)
  (with-slots (pending-word parts) context
    (when pending-word
      (return-from lexer-context-add-pending-word))
    (setf pending-word (make-extensible-vector :element-type 'character))))

(defun lexer-context-next-char (context)
  (with-slots (stream) context
    (peek-char nil stream nil :eof)))

(defun lexer-context-no-content-p (context)
  (with-slots (parts pending-word) context
    (and (equal 0 (length parts))
         (equal 0 (length pending-word)))))

(defun lexer-context-assignment-p (context)
  (with-slots (parts pending-word) context
    (if (equal 0 (length parts))
        (assignment-word-p pending-word)
        (and (typep (aref parts 0) 'simple-word)
             (assignment-word-p (simple-word-text (aref parts 0)))))))

(defun lexer-context-consume-character (context)
  (with-slots (stream) context
    (read-char stream nil :eof)))

(defun lexer-context-add-chars (context chars)
  (lexer-context-add-pending-word context)
  (with-slots (pending-word) context
    (loop :for char :across chars :do
       (vector-push-extend char pending-word))))

(defun lexer-context-extend-word (context)
  (lexer-context-add-chars context (string (lexer-context-next-char context)))
  (lexer-context-consume-character context))

(defun lexer-context-word-boundary (context)
  (with-slots (parts pending-word) context
    (when pending-word
      (vector-push-extend (make-instance 'simple-word :text pending-word) parts)
      (setf pending-word nil))))

(defun lexer-context-add-part (context part)
  (lexer-context-word-boundary context)
  (with-slots (parts) context
    (vector-push-extend part parts)))

(defun lexer-context-simple-word (context)
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
  (with-slots (stream) context
    (setf stream (make-string-input-stream ""))))

(defun lexer-context-delimit (context)
  (lexer-context-word-boundary context)
  (with-slots (parts all-chars-stream) context
    (let* ((part-count (length parts))
           (simple-word (lexer-context-simple-word context)))
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

            ((lexer-context-assignment-p context)
             (make-assignment-word-from-parts parts (get-output-stream-string all-chars-stream)))

            ((equal 1 part-count)
             (aref parts 0))

            ((< 1 part-count)
             (make-instance 'compound-word :parts parts :value (get-output-stream-string all-chars-stream)))

            (t
             (error "All cases should be covered above"))))))

(define-once-global +empty-shell-readtable+ (fset:empty-map))
(defparameter *shell-readtable* +empty-shell-readtable+)
(defun reset-shell-readtable ()
  (setf *shell-readtable* +empty-shell-readtable+))

(defclass dispatch-char ()
  ((table
    :type fset:map
    :initform (fset:empty-map)
    :initarg :table
    :reader dispatch-char-table)
   (fallback
    :type (or null symbol function)
    :initform nil
    :initarg :fallback
    :reader dispatch-char-fallback)))

(define-once-global +default-dispatch-char+ (make-instance 'dispatch-char))

(defun make-dispatch-char-description
    (&key (based-on +default-dispatch-char+)
       (table nil table-p) (fallback nil fallback-p))
  (unless (or table-p fallback-p)
    (return-from make-dispatch-char-description based-on))

  (make-instance
   'dispatch-char
   :table (if table-p table (dispatch-char-table based-on))
   :fallback (if fallback-p fallback (dispatch-char-fallback based-on))))

(define-condition character-already-set (error)
  ((char
    :accessor character-already-set-char
    :initarg :char
    :type character
    :initform (required))
   (value
    :accessor character-already-set-value
    :initarg :value
    :initform (required)))
  (:report (lambda (c s) (format s "~W is already set to ~A" (character-already-set-char c) (character-already-set-value c)))))

(define-condition character-not-dispatch (error)
  ((char
    :accessor character-not-dispatch-char
    :initform (required)
    :initarg :char
    :type character))
  (:report (lambda (c s) (format s "~W is not a dispatch character" (character-not-dispatch-char c)))))

(defun error-if-exists (character map)
  (multiple-value-bind (old-value found) (fset:lookup map character)
    (when found
      (cerror "Continue and replace" 'character-already-set :char character :value old-value))))

(defun set-character-handler (character fn &key (if-exists :error))
  (assert (find if-exists #(:error :replace)))
  (when (eq if-exists :error)
    (error-if-exists character *shell-readtable*))
  (setf *shell-readtable* (fset:with *shell-readtable* character fn)))

(defun make-shell-dispatch-character (first-character &key (if-exists :error) (default-handler nil default-handler-p))
  (assert (find if-exists #(:error :replace)))
  (when (eq if-exists :error)
    (error-if-exists first-character *shell-readtable*))
  (multiple-value-bind (value found) (fset:lookup *shell-readtable* first-character)
    (when (and found 'dispatch-char)
      (when default-handler-p
        (let ((replacement (make-dispatch-char-description
                            :based-on value :fallback default-handler)))
          (setf (fset:lookup *shell-readtable* first-character) replacement)))
      (return-from make-shell-dispatch-character))
    (setf (fset:lookup *shell-readtable* first-character) (make-dispatch-char-description :fallback default-handler))))

(defun set-shell-dispatch-character (first-character second-character fn &key (if-exists :error))
  (assert (find if-exists #(:error :replace)))
  (macrolet
      ((first-entry () '(fset:lookup *shell-readtable* first-character)))
    (multiple-value-bind (first found) (first-entry)
      (when (and found (not (typep first 'dispatch-char)))
        (error 'character-not-dispatch :char first-character))
      (when (eq if-exists :error)
        (error-if-exists second-character (dispatch-char-table first)))
      (let ((new-sub-table (fset:with (dispatch-char-table first) second-character fn)))
        (setf (first-entry) (make-dispatch-char-description :based-on first :table new-sub-table))
        t))))

(defun %shell-extensible-read (stream map initiation-sequence fallback context)
  (let ((next-char (peek-char nil stream nil :eof)))
    (vector-push-extend next-char initiation-sequence)
    (multiple-value-bind (value found) (fset:lookup map next-char)
      (when (not found)
        (return-from %shell-extensible-read (funcall fallback stream initiation-sequence context)))

    (read-char stream nil :eof)
    (let ((result
           (typecase value
             (dispatch-char
              (let ((inner-fallback (dispatch-char-fallback value)))
                (%shell-extensible-read stream
                                        (dispatch-char-table value)
                                        initiation-sequence
                                        (if inner-fallback
                                            inner-fallback
                                            (lambda (s i c)
                                              (declare (ignore s c))
                                              (error "Unhandled dispatch character sequence ~A" i)))
                                        context)))

             (t
              (funcall value stream initiation-sequence context)))))

      (unless (or (eq result t) (typep result '(or string token)))
        (error "Lexer extensions must return a token, got ~A" result))
      result))))

(defun shell-extensible-read-ingest (context)
  (with-slots (stream) context
    (labels
        ((fallback (s initiation-sequence c)
           (declare (ignore s c))
           (assert (equal 1 (length initiation-sequence)) nil
                   "This function should only run when the first table had no matches, but we had ~A" initiation-sequence)
           (return-from shell-extensible-read-ingest nil)))
      (%shell-extensible-read stream *shell-readtable* (make-extensible-vector) #'fallback context))))

(defun shell-extensible-read (stream)
  (let ((c (make-instance 'lexer-context :stream stream)))
    (declare (dynamic-extent c))
    (shell-extensible-read-ingest c)))

(defun handle-extensible-syntax (context)
  (let ((value (shell-extensible-read-ingest context)))
    (unless value
      (return-from handle-extensible-syntax nil))
    (when (eq t value)
      (return-from handle-extensible-syntax t))

    (typecase value
      (string (lexer-context-add-chars context value))
      (token (lexer-context-add-part context value))))
  t)

(defun next-token (stream)
  (let* ((context (make-instance 'lexer-context :stream stream)))
    (labels ((next-char () (lexer-context-next-char context)))

      ;; The lexing rules depend on whether the current character
      ;; was quoted.  We will always deal with quoting upfront, and
      ;; so we can assume that the current character is never quoted
      (loop
         (block again
           (labels ((again () (return-from again))
                    (delimit () (return-from next-token (lexer-context-delimit context))))
             (cond
               ;; If the end of input is recognized, the current token
               ;; shall be delimited. If there is no current token, the
               ;; end-of-input indicator shall be returned as the token.
               ((eq :eof (next-char))
                (when (lexer-context-no-content-p context)
                  (lexer-context-add-part context +eof+))
                (delimit))

               ;; If the previous character was used as part of an
               ;; operator and the current character is not quoted and
               ;; can be used with the current characters to form an
               ;; operator, it shall be used as part of that (operator)
               ;; token.
               ((let ((simple-word (lexer-context-simple-word context)))
                  (and simple-word
                       (operator-p simple-word)
                       (operator-p (concatenate 'string simple-word (string (next-char))))))
                (lexer-context-extend-word context)
                (again))

               ;; If the previous character was used as part of an
               ;; operator and the current character cannot be used with
               ;; the current characters to form an operator, the
               ;; operator containing the previous character shall be
               ;; delimited.
               ((let ((simple-word (lexer-context-simple-word context)))
                  (and simple-word
                       (operator-p simple-word)
                       (not (operator-p (concatenate 'string simple-word (string (next-char)))))))
                (delimit))

               ;; If the current character is backslash, single-quote,
               ;; or double-quote ( '\', '", or ' )' and it is not
               ;; quoted, it shall affect quoting for subsequent
               ;; characters up to the end of the quoted text. The rules
               ;; for quoting are as described in Quoting. During token
               ;; recognition no substitutions shall be actually
               ;; performed, and the result token shall contain exactly
               ;; the characters that appear in the input (except for
               ;; <newline> joining), unmodified, including any embedded
               ;; or enclosing quotes or substitution operators, between
               ;; the quote mark and the end of the quoted text. The
               ;; token shall not be delimited by the end of the quoted
               ;; field.
               ((equal (next-char) #\')
                (lexer-context-add-part context (read-single-quote stream))
                (again))
               ((equal (next-char) #\Backslash)
                (lexer-context-consume-character context)
                (unless (equal (next-char) #\Linefeed)
                  (lexer-context-add-part context (make-escaped-character (next-char))))
                (lexer-context-consume-character context)
                (again))
               ((equal (next-char) #\")
                (lexer-context-add-part context (read-double-quote stream))
                (again))

               ;; If the current character is an unquoted '$' or '`',
               ;; the shell shall identify the start of any candidates
               ;; for parameter expansion ( Parameter Expansion),
               ;; command substitution ( Command Substitution), or
               ;; arithmetic expansion ( Arithmetic Expansion) from
               ;; their introductory unquoted character sequences: '$'
               ;; or "${", "$(" or '`', and "$((", respectively. The
               ;; shell shall read sufficient input to determine the end
               ;; of the unit to be expanded (as explained in the cited
               ;; sections). While processing the characters, if
               ;; instances of expansions or quoting are found nested
               ;; within the substitution, the shell shall recursively
               ;; process them in the manner specified for the construct
               ;; that is found. The characters found from the beginning
               ;; of the substitution to its end, allowing for any
               ;; recursion necessary to recognize embedded constructs,
               ;; shall be included unmodified in the result token,
               ;; including any embedded or enclosing substitution
               ;; operators or quotes. The token shall not be delimited
               ;; by the end of the substitution.
               ((equal (next-char) #\$)
                (lexer-context-add-part context (read-dollar stream))
                (again))
               ((equal (next-char) #\`)
                (lexer-context-add-part context (read-backquote stream))
                (again))

               ;; If the current character is not quoted and can be used
               ;; as the first character of a new operator, the current
               ;; token (if any) shall be delimited. The current
               ;; character shall be used as the beginning of the next
               ;; (operator) token.
               ((operator-prefix-p (string (next-char)))
                (unless (lexer-context-no-content-p context)
                  (delimit))
                (lexer-context-extend-word context)
                (again))

               ;; If the current character is an unquoted <newline>, the
               ;; current token shall be delimited.
               ((equal #\linefeed (next-char))
                (when (lexer-context-no-content-p context)
                  (lexer-context-consume-character context)
                  (lexer-context-add-part context +newline+))
                (delimit))

               ;; If the current character is an unquoted <blank>, any
               ;; token containing the previous character is delimited
               ;; and the current character shall be discarded.
               ((blank-p (next-char))
                (unless (lexer-context-no-content-p context)
                  (delimit))
                (lexer-context-consume-character context)
                (again))

               ((handle-extensible-syntax context)
                (again))

               ;; If the previous character was part of a word, the
               ;; current character shall be appended to that word.
               ((not (lexer-context-no-content-p context))
                (lexer-context-extend-word context)
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
                (lexer-context-extend-word context)
                (again)))))))))
