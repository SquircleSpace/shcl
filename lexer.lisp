(in-package :shcl.lexer)

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(defclass token ()
  ((value :type string
          :initform (error "Required")
          :accessor token-value
          :initarg :value)))
(defmethod print-object ((token token) stream)
  (format stream "#<~A ~W>" (class-name (class-of token)) (token-value token)))

(defclass eof (token)
  ((value :initform "<EOF>")))
(defmethod print-object ((eof eof) stream)
  (format stream "#<EOF>"))
(define-once-global +eof+ (make-instance 'eof))

(defun plusify (symbol)
  (intern (concatenate 'string "+" (symbol-name symbol) "+")))

(defmacro define-simple-token (name)
  `(defclass ,name (token)
     ()))

(define-simple-token a-word)
(define-simple-token assignment-word)
(define-simple-token name)
(define-simple-token io-number)

(defparameter *print-literals-by-name* t)
(defclass literal-token (token)
  ())
(defmethod print-object ((literal-token literal-token) stream)
  (if *print-literals-by-name*
      (format stream "#<~A>" (class-name (class-of literal-token)))
      (format stream "#<LITERAL-TOKEN ~W>" (token-value literal-token))))

(defmacro define-literal-token (name string)
  `(progn
     (defclass ,name (literal-token)
       ((value :initform ,string)))))

(define-literal-token newline (string #\linefeed))
(define-once-global +newline+ (make-instance 'newline))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *operators*
    #((and-if . "&&")
      (or-if . "||")
      (dsemi . ";;")
      (dless . "<<")
      (dgreat . ">>")
      (lessand . "<&")
      (greatand . ">&")
      (lessgreat . "<>")
      (dlessdash . "<<-")
      (clobber . ">|")
      (semi . ";")
      (par . "&")
      (pipe . "|")
      (paren . ")")
      (great . ">")
      (less . "<"))))

(defun make-operator (string)
  (make-instance (car (find string *operators* :test #'equal :key #'cdr))))

(defun operator-p (word)
  (find word *operators* :test #'equal :key #'cdr))

(defun prefix-match-p (prefix-word whole-word)
  (when (> (length prefix-word) (length whole-word))
    (return-from prefix-match-p nil))
  (loop :for w-char :across whole-word
     :for char :across prefix-word
     :do (unless (equal w-char char)
           (return-from prefix-match-p nil)))
  t)

(defun operator-prefix-p (word)
  (find-if (lambda (o-word) (prefix-match-p word o-word)) *operators* :key #'cdr))

(defmacro define-literal-tokens ()
  `(progn ,@(loop :for pair :across *operators* :collect
               `(define-literal-token ,(car pair) ,(cdr pair)))))
(define-literal-tokens)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *reserved-words*
    #((if-word . "if")
      (then . "then")
      (else . "else")
      (elif . "elif")
      (fi . "fi")
      (do-word . "do")
      (done . "done")
      (case-word . "case")
      (esac . "esac")
      (while . "while")
      (until . "until")
      (for . "for")
      ({ . "{")
      (} . "}")
      (! . "!")
      (in . "in"))))

(defmacro define-reserved-words ()
  `(progn ,@(loop :for pair :across *reserved-words* :collect
               `(define-literal-token ,(car pair) ,(cdr pair)))))
(define-reserved-words)

(defun reserved-p (word)
  (find word *reserved-words* :test #'equal :key #'cdr))

(defun reserved-prefix-p (word)
  (find-if (lambda (r-word) (prefix-match-p word r-word)) *reserved-words*))

(define-condition eof-error (error)
  ((comment :initarg :comment
            :initform "Unknown context"
            :accessor eof-comment)))
(defun eof-error (&rest rest)
  (apply 'error 'eof-error rest))

(defun read-single-quote (stream)
  (let ((next-char (peek-char nil stream nil :eof))
        (value (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
    (labels ((next ()
               (vector-push-extend next-char value)
               (read-char stream nil :eof)
               (setf next-char (peek-char nil stream nil :eof))))
      (assert (equal next-char #\'))
      (next)
      (loop :while (not (equal next-char #\'))
         :do (if (equal :eof next-char)
                 (eof-error :comment "Single quote expected")
                 (next)))
      (assert (equal next-char #\'))
      ;; One more for the close quote
      (next))
    value))

(defun read-double-quote (stream)
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
        (next-char (peek-char nil stream nil :eof)))
    (labels ((keep ()
               (vector-push-extend next-char result)
               (skip))
             (skip ()
               (read-char stream nil :eof)
               (setf next-char (peek-char nil stream nil :eof))))
      (loop
         (cond ((equal :eof next-char)
                (eof-error :comment "Double quote expected"))

               ((equal #\" next-char)
                (keep)
                (return-from read-double-quote result))

               ((equal #\Backslash next-char)
                (skip)
                (if (find next-char '(#\$ #\` #\" #\Backslash #\Linefeed))
                    (keep)
                    (vector-push-extend #\Backslash result)))

               ((equal #\` next-char)
                (loop :for char :across (read-backquote stream)
                   :do (vector-push-extend char result)))

               ((equal #\$ next-char)
                (loop :for char :across (read-dollar stream)
                   :do (vector-push-extend char result))))))
    (assert nil nil "This function shouldn't return via this path")))

(defun read-backquote (stream)
  (declare (ignore stream))
  (error "Backquote is not implemented.  It sucks anyway.  Use $()"))

(defun read-dollar-curly (stream)
  (declare (ignore stream))
  (error "${} is not implemented"))

(defun read-dollar-paren (stream)
  (let ((next-char (peek-char nil stream nil :eof)))
    (assert (equal #\( next-char))
    (read-char stream nil :eof)
    (setf next-char (peek-char nil stream nil :eof))
    (when (equal #\( next-char)
      (return-from read-dollar-paren (concatenate 'string "(" (read-dollar-paren-paren stream)))))
  ;; We need to parse a full command.  The best way to do that is with
  ;; our `NEXT-TOKEN' function, but that returns tokens instead of
  ;; strings.  We need a string containing everything it read!
  ;; Luckily, an echo stream allows us to know exactly what was read
  ;; by `NEXT-TOKEN'.
  (let ((out-stream (make-string-output-stream)))
    (format out-stream "(")
    (let* ((echo-stream (make-echo-stream stream out-stream))
           (token (next-token echo-stream)))

      (loop
       (cond ((typep token 'eof)
              (eof-error ") expected"))

             ((typep token 'paren)
              (return-from read-dollar-paren (get-output-stream-string out-stream))))
       (setf token (next-token echo-stream)))))
  (assert nil nil "This function doesn't return normally"))

(defun read-dollar-paren-paren (stream)
  (let ((next-char (peek-char nil stream nil :eof))
        (result (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
    (labels ((take ()
               (vector-push-extend next-char result)
               (read stream nil :eof)
               (setf next-char (peek-char nil stream nil :eof))))
      (assert (equal #\( next-char))
      (take)
      (error "$(()) is not implemented"))))

(defun read-name (stream)
  (let ((next-char (peek-char nil stream nil :eof))
        (result (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
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

(defun read-dollar-word (stream)
  (let ((next-char (peek-char nil stream nil :eof)))
    (when (find next-char #(#\@ #\* #\# #\? #\- #\$ #\! #\0))
      (return-from read-dollar-word (string next-char))))
  (read-name stream))

(defun read-dollar (stream)
  (let ((next-char (peek-char nil stream nil :eof)))
    (assert (equal #\$ next-char))
    (read-char stream nil :eof)
    (setf next-char (peek-char nil stream nil :eof))
    (cond ((equal #\{ next-char)
           (concatenate 'string "$" (read-dollar-curly stream)))

          ((equal #\( next-char)
           (concatenate 'string "$" (read-dollar-paren stream)))

          (t
           (concatenate 'string "$" (read-dollar-word stream))))))

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
  (find char '(#\space #\tab #\linefeed #\return)))

(defun token-iterator (stream)
  (let (hit-eof)
    (make-iterator ()
      (when hit-eof
        (stop))
      (let ((token (next-token stream)))
        (when (typep token 'eof)
          (setf hit-eof t))
        (emit token)))))

(defgeneric tokenize (source))

(defmethod tokenize ((stream stream))
  (tokens-in-stream stream))

(defmethod tokenize ((string string))
  (tokens-in-string string))

(defun tokens-in-string (string)
  (tokens-in-stream (make-string-input-stream string)))

(defun tokens-in-stream (stream)
  (let ((result (make-array 0 :adjustable t :fill-pointer t))
        (token (next-token stream)))
    (loop :while (not (typep token 'eof)) :do
       (progn (vector-push-extend token result)
              (setf token (next-token stream))))
    (vector-push-extend token result)
    result))

(defun next-token (stream)
  (let ((word (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
        (next-char (peek-char nil stream nil :eof))
        contains-quotes)
    (labels ((is-operator (&optional (the-word word))
               (and (not contains-quotes) (operator-p the-word)))
             (extend? (&optional (char next-char) (the-word word))
               (concatenate 'string the-word (list char)))
             (extend! (&optional (char next-char))
               (vector-push-extend char word))
             (extend-string! (string)
               (loop :for char :across string
                  :do (extend! char)))
             (consume ()
               (read-char stream nil :eof)
               (reassess-next-char))
             (reassess-next-char ()
               (setf next-char (peek-char nil stream nil :eof)))
             (delimit (&optional value)
               (return-from next-token
                 (cond (value
                        value)

                       ((is-operator)
                        (make-operator word))

                       ((and (not (find-if-not #'digit-char-p word))
                             (or (equal #\< next-char) (equal #\> next-char)))
                        (make-instance 'io-number :value word))

                       (t
                        (make-instance 'token :value word))))))

      (macrolet ((again () '(return-from again)))

        ;; The lexing rules depend on whether the current character
        ;; was quoted.  We will always deal with quoting upfront, and
        ;; so we can assume that the current character is never quoted
        (loop
           (block again
             (cond
               ;; If the end of input is recognized, the current token
               ;; shall be delimited. If there is no current token, the
               ;; end-of-input indicator shall be returned as the token.
               ((eq :eof next-char)
                (delimit
                 (when (equal 0 (length word))
                   +eof+)))

               ;; If the previous character was used as part of an
               ;; operator and the current character is not quoted and
               ;; can be used with the current characters to form an
               ;; operator, it shall be used as part of that (operator)
               ;; token.
               ((and (is-operator) (is-operator (extend?)))
                (extend!)
                (consume)
                (again))

               ;; If the previous character was used as part of an
               ;; operator and the current character cannot be used with
               ;; the current characters to form an operator, the
               ;; operator containing the previous character shall be
               ;; delimited.
               ((and (is-operator) (not (is-operator (extend?))))
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
               ((equal next-char #\')
                (setf contains-quotes t)
                (extend-string! (read-single-quote stream))
                (reassess-next-char)
                (again))
               ((equal next-char #\Backslash)
                (setf contains-quotes t)
                (consume)
                (unless (equal next-char #\Linefeed)
                  (extend! #\Backslash)
                  (extend!))
                (consume)
                (again))
               ((equal next-char #\")
                (setf contains-quotes t)
                (extend-string! (read-double-quote stream))
                (reassess-next-char)
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
               ((equal next-char #\$)
                (extend-string! (read-dollar stream))
                (reassess-next-char)
                (again))
               ((equal next-char #\`)
                (extend-string! (read-backquote stream))
                (reassess-next-char)
                (again))

               ;; If the current character is not quoted and can be used
               ;; as the first character of a new operator, the current
               ;; token (if any) shall be delimited. The current
               ;; character shall be used as the beginning of the next
               ;; (operator) token.
               ((operator-prefix-p (string next-char))
                (unless (equal 0 (length word))
                  (delimit))
                (extend!)
                (consume)
                (again))

               ;; If the current character is an unquoted <newline>, the
               ;; current token shall be delimited.
               ((equal #\linefeed next-char)
                (unless (equal 0 (length word))
                  (delimit))
                (consume)
                (delimit +newline+))

               ;; If the current character is an unquoted <blank>, any
               ;; token containing the previous character is delimited
               ;; and the current character shall be discarded.
               ((blank-p next-char)
                (unless (equal 0 (length word))
                  (delimit))
                (consume)
                (again))

               ;; If the previous character was part of a word, the
               ;; current character shall be appended to that word.
               ((not (equal 0 (length word)))
                (extend!)
                (consume)
                (again))

               ;; If the current character is a '#', it and all
               ;; subsequent characters up to, but excluding, the next
               ;; <newline> shall be discarded as a comment. The
               ;; <newline> that ends the line is not considered part of
               ;; the comment.
               ((equal #\# next-char)
                (read-comment stream)
                (again))

               ;; The current character is used as the start of a new
               ;; word.
               (t
                (extend!)
                (consume)
                (again)))))))))
