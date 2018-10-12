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

(defpackage :shcl/core/parser
  (:use
   :common-lisp :alexandria :shcl/core/lexer :shcl/core/utility
   :shcl/core/iterator)
  (:import-from :shcl/core/advice #:define-advisable)
  (:import-from :shcl/core/data #:define-data)
  (:import-from :closer-mop)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:export
   ;; Foundations of parsing
   #:parser-error #:parser-value #:parser-let* #:parser-let #:parser-lookahead
   #:parser-try #:parser-choice #:parse-eof #:parse-object-of-type
   #:parser-handler-case #:parser-repeat #:parser-bind #:parser-block
   #:private-parser-block #:parse #:parser-if

   ;; Error types
   #:expected-eof #:expected-eof-got #:unexpected-eof
   #:unexpected-eof-expected-type #:type-mismatch #:type-mismatch-expected-type
   #:type-mismatch-got #:choice #:choice-errors #:choice-errors-iterator
   #:unconditional-failure

   ;; Convenience and high-level parsing tools
   #:syntax-iterator #:define-terminal #:define-nonterminal #:syntax-tree
   #:define-nonterminal-class #:parse-failure #:parse-failure-error-object
   #:print-error))
(in-package :shcl/core/parser)

(optimization-settings)

(define-data syntax-tree ()
  ()
  (:documentation
   "A base class that nonterminals derive from.

See `define-nonterminal'."))

(defmethod make-load-form ((sy syntax-tree) &optional environment)
  (let ((slots (mapcar 'closer-mop:slot-definition-name (closer-mop:class-slots (class-of sy)))))
    (make-load-form-saving-slots sy :slot-names slots :environment environment)))

(defmethod print-object ((st syntax-tree) stream)
  (print-unreadable-object (st stream :type t :identity nil)))

(defgeneric print-error (err stream)
  (:documentation
   "Print a human-readable description of `err' to `stream'."))

(defmethod print-error (err stream)
  (if err
      (format stream "Unknown error (~W)" err)
      (format stream "Unknown error")))

(define-data parser-error ()
  ()
  (:documentation
   "A base class that built-in parse error classes derive from.

This class doesn't do anything."))

(defgeneric expected-eof-got (expected-eof)
  (:documentation
   "Returns the object that was produced when end-of-file was
expected."))

(define-data expected-eof (parser-error)
  ((got
    :initarg :got
    :reader expected-eof-got
    :initform (required)))
  (:documentation
   "A class representing the fact that end-of-file was expected but
the token stream wasn't empty.

`expected-eof-got' extracts the token that was produced."))

(defmethod print-error ((err expected-eof) stream)
  (format stream "Expected end of file, but instead found ~W" (token-value (expected-eof-got err))))

(defgeneric unexpected-eof-expected-type (unexpected-eof)
  (:documentation
   "Returns the type of token that was expected when end-of-file was
encountered."))

(define-data unexpected-eof (parser-error)
  ((expected-type
    :initarg :expected-type
    :reader unexpected-eof-expected-type
    :initform (required)))
  (:documentation
   "A class representing the fact that more tokens were expected but
the token stream indicated end-of-file.

`unexpected-eof-expected-type' returns the type that was expected."))

(defmethod print-error ((err unexpected-eof) stream)
  (format stream "Unexpected end of file, expected token of type ~W" (unexpected-eof-expected-type err)))

(defgeneric type-mismatch-expected-type (type-mismatch)
  (:documentation
   "Returns the type that was expected when the parse error occurred."))

(defgeneric type-mismatch-got (type-mismatch)
  (:documentation
   "Returns the token that was produced when the parse error occurred."))

(define-data type-mismatch (parser-error)
  ((expected-type
    :initarg :expected-type
    :reader type-mismatch-expected-type
    :initform (required))
   (got
    :initarg :got
    :reader type-mismatch-got
    :initform (required)))
  (:documentation
   "A class representing the fact that the next token was not of the
correct type.

Use `type-mismatch-expected-type' and `type-mismatch-got' to see what
was expected and what was received."))

(defmethod print-error ((err type-mismatch) stream)
  (format stream "Unexpected token, expected token of type ~W but got ~S"
          (type-mismatch-expected-type err)
          (or (token-value (type-mismatch-got err))
              (type-mismatch-got err))))

(defgeneric choice-errors (choice)
  (:documentation
   "Returns a vector of the errors produced by each of the potential
parses."))

(define-data choice (parser-error)
  ((errors
    :initarg :errors
    :reader choice-errors
    :type vector
    :initform (required)))
  (:documentation
   "A class representing the fact that a series of potential parses
all failed.

Use `choice-errors' to access the errors produced by the potential
parses."))

(defun choice-errors-iterator (choice &key recursive-p)
  "Return an iterator that traverses the errors contained in a
`choice' instance.

If `recursive-p' is non-nil, then the iterator will recursively
traverse any `choice' objects it encounters.  In that case, the
iterator will not emit any `choice' objects.  If `recursive-p' is nil
then `choice' objects will be emitted just like every other error
object."
  (unless recursive-p
    (return-from choice-errors-iterator
      (vector-iterator (choice-errors choice))))

  (let ((stack (make-extensible-vector)))
    (vector-push-extend (vector-iterator (choice-errors choice)) stack)
    (make-computed-iterator
      (loop :while (not (zerop (length stack))) :do
         (block again
           (do-iterator (err (aref stack (1- (length stack))))
             (when (typep err 'choice)
               (vector-push-extend (choice-errors-iterator err :recursive-p recursive-p) stack)
               (return-from again))
             (emit err))
           (vector-pop stack)))
      (stop))))

(defmethod print-error ((err choice) stream)
  (when (zerop (length (choice-errors err)))
    (return-from print-error
      (format stream "No choice but to fail the parse")))
  (format stream "Tried multiple parses that all failed")
  (do-iterator (error (choice-errors-iterator err :recursive-p t))
    (format stream "~%")
    (print-error error stream)))

(define-data unconditional-failure (parser-error)
  ()
  (:documentation
   "A class representing the fact that the parse could not have
possibly succeeded.

This error type is completely opaque and provides no useful
information about why the parse failed.  So, use it sparingly!"))

(defgeneric parse-failure-error-object (parse-failure)
  (:documentation
   "Aquire the parse error object that the parser produced.

See `parse-failure'."))

(define-condition parse-failure (error)
  ((error-object
    :initarg :error-object
    :initform nil
    :reader parse-failure-error-object))
  (:report (lambda (c s)
             (print-error (parse-failure-error-object c) s)))
  (:documentation
   "An error condition to represent parse failure.

Note that parsers don't use the condition system to indicate parse
errors.  This condition is meant to be used at the interface between
parser logic and everything else."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parser-part-name (thing)
    (symbol-nconc-intern nil "PARSE-" thing)))

(defconstant +parser-secret-token+ '+parser-secret-token+)

(defmacro parser-bind ((value error-p) parser-form &body body)
  "Evaluate `parser-form' and bind the results.

The exact structure of a parser result is unspecified.  This macro is
the only supported way to extract parse results.

If `error-p' is non-nil value then `parser-form' returned a parse
failure.  Whatever value the parser produced (either an error or
succesful parse value) is stored in `value'.

To produce parse results this macro can consume you must use
`parser-value' or `parser-error'."
  (let ((value-internal (gensym "VALUE-INTERNAL"))
        (error-p-internal (gensym "ERROR-P-INTERNAL"))
        (secret-token (gensym "SECRET-TOKEN")))
    `(multiple-value-bind
           (,value-internal ,error-p-internal ,secret-token)
         ,parser-form
       (unless (eq ,secret-token +parser-secret-token+)
         (error "`parser-form' did not return valid parser values~%~S" ',parser-form))
       (let ((,value ,value-internal)
             (,error-p ,error-p-internal))
         ,@body))))

(defun syntax-iterator (parser-function token-iterator)
  "Return an iterator that emits the values produced by
`parser-function' when it is repeatedly called with `token-iterator'.

If `parser-function' returns nil then the iterator will stop producing
output.

If a parse error occurs this function signals a `parse-failure'
error."
  (make-computed-iterator
    (parser-bind (value error-p) (funcall parser-function token-iterator)
      (when error-p
        (error 'parse-failure :error-object value))
      (unless value
        (stop))
      (emit value))))

(defun parser-error (error)
  "Returns values that ndicate that parsing has failed.

Note that this function may return multiple values.  The contents of
these values is unspecified.  You must use `parser-bind' to extract
meaning from the values."
  (values error t +parser-secret-token+))

(defun parser-value (value)
  "Returns values that ndicate that parsing has succeeded.

Note that this function may return multiple values.  The contents of
these values is unspecified.  You must use `parser-bind' to extract
meaning from the values."
  (values value nil +parser-secret-token+))

(defmacro private-parser-block (macro-name &body body)
  "Establish a parser block with a named parse macro.

See `parser-block'.  This is effectively `parser-block' but you're
allowed to choose an alternate name for the `parse' local macro.  This
is primarily useful in macro contexts where you don't want to
interfere with forms the user provides."
  (let ((value (gensym "VALUE"))
        (err (gensym "ERR"))
        (thing (gensym "THING"))
        (parser-block (gensym "PARSER-BLOCK")))
    `(block ,parser-block
       (macrolet
           ((,macro-name (,thing)
              `(parser-bind (,',value ,',err) ,,thing
                 (when ,',err
                   (return-from ,',parser-block
                     (parser-error ,',value)))
                 ,',value)))
         ,@body))))

(defmacro parse (form)
  "Attempt to parse a thing and extract the parse value.

This macro must be used inside the lexical scope of a `parser-block'
form.  See `parser-block' for documentation on how this macro works."
  (declare (ignore form))
  (error "The parse macro must be used inside a parser block"))

(defmacro parser-block (&body body)
  "Establish a lexical scope where the `parse' macro can be used.

Inside the lexical scope of `body', a local macro named `parse' is
defined.  `parse' takes one argument: a parser form.  If the parser
form returns a parse error then the parse error is returned from the
`parser-block' form.  If the parser form returns a successful parse
then `parse' simply returns the parsed value.  This allows you to
perform a series of parses and early return if any one of them fails.
For example, you might use `parser-block' in the following way.

    (parser-block
      (parser-value
       (some-random-function (parse (parse-foo iter))
                             (parse (parse-bar iter)))))

You generally don't want to use this macro in a macro context.  You
should usually use `private-parser-block' with a gensym'd name for the
parse macro.  In fact, this macro will expand to something
semantically equivalent to the following form.

    `(private-parser-block parse ,@body)"
  `(private-parser-block parse
     ,@body))

(defmacro parser-if (iter condition-parser then &optional (else nil else-p))
  "This is the parser equivalent of `if'.

Attempt to parse `condition-parser'.  If it parses succesfully, then
evaluate `then' and return the result.  If it fails to parse and does
not move `iter', then `else' will be evaluated instead.  If `else' is
not provided, the parse error will be returned instead.  If
`condition-parser' fails to parse and moves `iter', then neither
`then' or `else' is evaluated.  The parse error produced by
`condition-parser' will be retuned."
  (let ((condition (gensym "CONDITION"))
        (parser-if (gensym "PARSER-IF"))
        (err (gensym "ERR")))
    `(block ,parser-if
       (parser-handler-case ,iter
           (parser-let
               ((,condition ,condition-parser))
             (declare (ignore ,condition))
             (return-from ,parser-if ,then))
         ,(if else-p
              `(t ()
                  ,else)
              `(t (,err)
                  (parser-error ,err)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun let-bindings-transformer (parse-macro)
    (lambda (binding)
      (destructuring-bind (variable form) binding
        `(,variable (,parse-macro ,form))))))

(defmacro parser-let* (bindings &body body)
  "Parse values and bind them to variables.

This macro is very similar to `let*'.  The key difference is that the
value form for each binding is treated as a parser form.  If it parses
successfully (i.e. returns nil as the second value) then the binding
is established normally.  If the value form fails to parse (ie.e
returns a non-nil second value) then the `parser-let' form will
immediately return the same parse error."
  (let ((parse (gensym "PARSE")))
    `(private-parser-block ,parse
       (let* ,(mapcar (let-bindings-transformer parse) bindings)
         ,@body))))

(defmacro parser-let (bindings &body body)
  "Parse values and bind them to variables.

This macro behaves similarly to `parser-let*'.  The only difference is
that bindings are established in parallel -- just like `let' would."
  (let ((parse (gensym "PARSE")))
    `(private-parser-block ,parse
       (let ,(mapcar (let-bindings-transformer parse) bindings)
         ,@body))))

(defmacro parser-lookahead (iter &body body)
  "Evaluate `body' and then rewind `iter' on parser success.

This form returns the values produced by evaluating `body'.  If `body'
parses successfully (i.e. returns nil for its second value) then
`iter' will be moved back to the location it had prior to this form
being evaluated.  `iter' is evaluated once and the resulting value is
manipulated as described earlier."
  (let ((fork (gensym "FORK"))
        (original-iter (gensym "ORIGINAL-ITER"))
        (value (gensym "VALUE"))
        (err (gensym "ERR")))
    `(let* ((,original-iter ,iter)
            (,fork (fork ,original-iter)))
       (parser-bind (,value ,err) (progn ,@body)
         (cond
           (,err
            (parser-error ,value))

           (t
            (move-forkable-wrapper-iterator-to ,original-iter ,fork)
            (parser-value ,value)))))))

(defmacro parser-try (iter &body body)
  "Evaluate `body' and then rewind `iter' on parser failure.

This macro is very similar to `parser-lookahead'.  Unlike
`parser-lookahead', this macro modifies `iter' only when `body'
evaluates to a parse failure (i.e. returns a non-nil second value).
If a parse failure occurs then this macro will move `iter' to the
position it had prior to this form being evaluated."
  (let ((fork (gensym "FORK"))
        (original-iter (gensym "ORIGINAL-ITER"))
        (value (gensym "VALUE"))
        (err (gensym "ERR")))
    `(let* ((,original-iter ,iter)
            (,fork (fork ,original-iter)))
       (parser-bind (,value ,err) (progn ,@body)
         (cond
           (,err
            (move-forkable-wrapper-iterator-to ,original-iter ,fork)
            (parser-error ,value))

           (t
            (parser-value ,value)))))))

(defmacro parser-choice (iter &body options)
  "Evaluate each form in `options' until one of them parses successfully.

When control enters this macro, `iter' is evaluated and the result is
saved.  If one of the option forms fails to parse and moves `iter'
from its initial position then this macro will immediately fail to
parse with the same value.  If an option fails to parse without moving
`iter' then the next option will be attempted.  If one of the option
parses succesfully then this macro will immediately return the parsed
value.

This is effectively the parser equivalent of `or'."
  (let ((choice-block (gensym "CHOICE-BLOCK"))
        (original-iter (gensym "ORIGINAL-ITER"))
        (position (gensym "POSITION"))
        (err (gensym "ERR"))
        (errors (gensym "ERRORS"))
        (value (gensym "VALUE")))
    (labels
        ((option-handler (option)
           `(parser-bind (,value ,err) ,option

              (unless ,err
                ;; Happy day!
                (return-from ,choice-block
                  (parser-value ,value)))

              (unless (eq (forkable-wrapper-iterator-position-token ,original-iter)
                          ,position)
                ;; Sad day :(
                (return-from ,choice-block
                  (parser-error ,value)))

              (vector-push-extend ,value ,errors)
              ;; Live to parse another day
              )))

      (cond
        ((null options)
         '(parser-error (make-instance 'unconditional-failure)))

        ((null (cdr options))
         (car options))

        (t
         `(block ,choice-block
            (let* ((,original-iter ,iter)
                   (,position (forkable-wrapper-iterator-position-token ,original-iter))
                   (,errors (make-extensible-vector)))
              ,@(mapcar #'option-handler options)
              (parser-error (make-instance 'choice :errors ,errors)))))))))

(defmacro parser-handler-case (iter parser-form &body clauses)
  "Evaluate a parser form and handle any parse errors it returns.

This is the parser equivalent of `handler-case'.  First, `iter' is
evaluated and the result is saved.  `parser-form' is evaluated.  If it
returns a successful parse (i.e. the second return value is nil) then
this macro returns the same parse value.  If it returns a parse
error (i.e. the second return value is non-nil) then `iter' is
consulted.  If the position of `iter' is unchanged then `clauses' will
be searched for a suitable handler.  If the position of `iter' has
changed then this macro fails to parse with the same error value.

The clauses in `clauses' are evaluated just like in `handler-case'."
  (let ((value (gensym "VALUE"))
        (err (gensym "ERR"))
        (original-iter (gensym "ORIGINAL-ITER"))
        (position (gensym "POSITION")))
    (labels
        ((transform-clause (clause)
           (destructuring-bind (type (&optional (var nil var-p)) &rest forms) clause
             (if var-p
                 `(,type
                   (let ((,var ,value))
                     ,@forms))
                 `(,type
                   ,@forms)))))
      `(let* ((,original-iter ,iter)
              (,position (forkable-wrapper-iterator-position-token ,original-iter)))
         (parser-bind (,value ,err) ,parser-form
           (cond
             ((not ,err)
              (parser-value ,value))
             ((eq ,position (forkable-wrapper-iterator-position-token ,original-iter))
              (typecase ,value
                ,@(mapcar #'transform-clause clauses)
                (t
                 (parser-error ,value))))
             (t
              (parser-error ,value))))))))

(defun parser-repeat-times (min max iter body-fn)
  (let ((result (make-extensible-vector)))
    (loop
       :while (or (not max)
                  (< (length result) max))
       :do
       (parser-bind (value err)
           (parser-handler-case iter
               (funcall body-fn)
             (t (err)
                (return-from parser-repeat-times
                  (if (>= (length result) min)
                      (parser-value result)
                      (parser-error err)))))
         (when err
           (return-from parser-repeat-times
             (parser-error value)))
         (vector-push-extend value result)))
    (parser-value result)))

(defun parser-repeat-until (min stop-parser-fn iter body-fn)
  (parser-block
    (let ((result (if (> min 0)
                      (parse (parser-repeat-times min min iter body-fn))
                      (make-extensible-vector))))
      (labels
          ((done-p ()
             ;; If parser-handler-case returns a value normally then
             ;; we failed and we moved the iterator.
             (return-from parser-repeat-until
               (parser-handler-case iter
                   (parser-let
                       ((value (funcall stop-parser-fn iter)))
                     (declare (ignore value))
                     ;; If the stop parser parsed succesfully, then
                     ;; we're done.
                     (return-from done-p t))
                 (t ()
                    ;; We had an error, but the iterator didn't move.
                    ;; We're not done.
                    (return-from done-p nil))))))
        (loop :until (done-p) :do
           (vector-push-extend (parse (funcall body-fn)) result)))
      (parser-value result))))

(defun parser-repeat-f (min stopping-point iter body-fn)
  (check-type min (integer 0))
  (etypecase stopping-point
    ((or (integer 0) null)
     (parser-repeat-times min stopping-point iter body-fn))
    ((or function symbol)
     (parser-repeat-until min stopping-point iter body-fn))))

(defmacro parser-repeat ((min stopping-point iter) &body body)
  "Parse a sequence.

`iter' is evaluated and the result is saved.  This macro then
repeatedly evaluates `body'.  Every time `body' returns a successful
parse the parsed value is added to the result vector.  If `body' fails
to parse and the position of `iter' is unchanged then this macro
parses succesfuly and returns the result vector.  If `body' fails to
parse and the position of `iter' has changed then this macro fails to
parse with the same error.

`min' must be a non-negative integer.  If `body' fails to parse at
least `min' times then a parse error will be returned.

`stopping-point' is either nil, a non-negative integer, or a function.

If it is an integer, then parsing will stop with a successful result
when that number of parses of `body' have completed.  If `body' has
fulfilled the `min' number of required parses and it fails to parse
without moving the `iter', then `parser-repeat' will successfully
return the parsed objects produced by `body'.  A nil `stopping-point'
means that there is no limit on how many times `body' will be
evaluated.

If `stopping-point' is a function, then it will be called to decide
when parsing should stop.  After the minimum number of repetitions
have been satisfied, `parser-repeat' will call `stopping-point' after
each succesful parse of `body'.  If `stopping-point' returns a
succesful prase, then `parser-repeat' will return a succesful parse
value.  If `stopping-point' returns a failing parse without moving
`iter', then parsing will continue.  If `stopping-point' returns a
failing parse and moves `iter', then `parser-repeat' will return the
parse error produced by `stopping-point'.  Note that when
`stopping-point' is a function, `body' must always parse succesfully.
If `body' fails to parse then `parser-repeat' will fail to parse with
the same error.  This is different from the case where
`stopping-point' is an integer or nil!"
  `(parser-repeat-f ,min ,stopping-point ,iter (lambda () ,@body)))

(defun parse-eof (iter)
  "Parse successfully iff the given iterator produces no values.

This parser function never consumes input.  If the iterator has
reached the end of its values, this parser produces the value `:eof'."
  (parser-try iter
    (multiple-value-bind (value valid) (next iter)
      (if valid
          (parser-error (make-instance 'expected-eof :got value))
          (parser-value :eof)))))

(defun parse-object-of-type (iter type)
  "Parse successfully if the next object produced by `iter' is of type
`type'."
  (multiple-value-bind (value valid) (next iter)
    (cond
      ((not valid)
       (parser-error (make-instance 'unexpected-eof :expected-type type)))

      ((not (typep value type))
       (parser-error (make-instance 'type-mismatch :expected-type type :got value)))

      (t
       (parser-value value)))))

(defmacro define-terminal (type &optional (name nil name-p))
  "Define a parser function that parses objects satisfying the type `type'.

`type' is not evaluated.  `name' is the name of the function to
define.  If `name' is not provided then it defaults to `type' prefixed
with PARSE-.  If `type' is not a symbol then you must provide a `name'.

The function generated by this macro is advisable.  See
`shcl/core/advice:define-advice'.

Note that the function generated by this macro consumes no input
unless the parse succeeds."
  (unless name-p
    (unless (symbolp type)
      (error "name must be provided for complex types."))
    (setf name (parser-part-name type)))
  (let ((iter (gensym "ITER")))
    `(define-advisable ,name (,iter)
       ,(format
         nil
         "Parse successfully if the next object satisfies the type ~W.

If the next object does not satisfy the type then this function does
not move the iterator."
         type)
       (parser-try ,iter
         (parse-object-of-type ,iter ',type)))))

(defmacro define-nonterminal-class (class-name direct-superclasses slots &body options)
  "Define a class just like `define-nonterminal' would have.

This is a wrapper around `define-data' that `define-nonterminal' uses.
You should generally only use this macro when your parsing needs are
too complex for `define-nonterminal' to handle.  You don't need to use
this macro to define a nonterminal class.

This macro does 3 things.

1. It ensures that every slot has an initarg eq to the slot name
   itself.
2. It ensures the class inherits directly from `syntax-tree'.
3. It provides trivial documentation if none is provided explicitly."
  `(define-data ,class-name (,@direct-superclasses syntax-tree)
     ,(mapcar
       (lambda (slot)
         (when (symbolp slot)
           (setf slot (list slot)))
         (destructuring-bind (name &rest args) slot
           `(,name :initarg ,name ,@args)))
       slots)
     ,@options
     ,@(unless (assoc :documentation options)
         `((:documentation
            "A class representing a nonterminal production in a grammar")))))

(defmacro define-nonterminal (name-and-options &body productions)
  "Define a parser for a nonterminal.

This macro defines a parser function.  As the name implies, this macro
tries to emulate the behavior of a nonterminal node in a grammar.

`name-and-options' is either a symbol or a list.  If it is a list,
then the first element is the name of the parser function to
generate (if one is generated).  After the first element, the
remainder of the list is a plist which supports the following keys.

- `:function-name': This key should have a symbol value.  The value
  represents the name of the parser function to generate.  If this
  option is not provided then it defaults to the class name prefixed
  with PARSE-.

- `:parser-choice': This key should have a symbol value.  It is not
  evaluated and defaults to `parser-choice'.  This option exists to
  give you the opportunity to control how backtracking works.  You
  might do this, for example, to observe intermediate parse errors.

The parser function generated by this macro is advisable.  See
`shcl/core/advice:define-advice'.

Each form in `productions' describes a way to parse this nonterminal.
Each production is turned into a parser form and then combined with
`parser-choice'.  Thus, unlike a real grammar, the parser function
will use the first production to match.

1 .If the production is nil then it always succeeds and returns nil.

2. If the production is a list then it is treated as a description of
   slots for the nonterminal class and how to fill the slots.  Each
   element of the list is a list of two elements and describes one
   slot of the class.  The first element of the pair is the name of
   the slot.  The second element should evaluate to a funcallable
   object that takes one argument: the token iterator.  The function
   will be funcalled and the return value will be used to initialize
   the slot.  A class will be generated that has all the slots named
   by all the productions of this type.  See
   `define-nonterminal-class'.  You may also include `&optional' in
   the production list.  After `&optional' appears, slots are
   initialized more conservatively.  If a slot initializer returns a
   parse failure but consumes no input then that slot and all
   remaining slots will be initialized to nil.

   As a shorthand, you may simply provide a symbol instead of a list
   as a slot description.  The symbol will be used as the name of the
   slot and the slot will be initialized using the function named by
   prefixing the symbol with PARSER-.  For exmaple, these production
   forms are equivalent.

       (foo bar baz)
       ((foo parse-foo) (bar parse-bar) (baz parse-baz))

3. If the production is a symbol then it is treated as a parser
   function name.  This production type simply defers parsing to the
   named parser function.  If it succeeds, then the nonterminal parser
   will emit the same value.  If it fails then the nonterminal parser
   will either try the next production or emit the same failure.  See
   `parser-choice'."
  (let ((iter (gensym "ITER"))
        (parse (gensym "PARSE"))
        ;; (err (gensym "ERR"))
        (slots (make-hash-table))
        (options (make-extensible-vector))
        (result-forms (make-extensible-vector))
        function-name
        class-name
        parser-choice-sym)

    (when (symbolp name-and-options)
      (setf name-and-options `(,name-and-options)))

    (destructuring-bind (class &key ((:function-name fn) (parser-part-name class))
                               (parser-choice 'parser-choice))
        name-and-options
      (check-type parser-choice symbol)
      (setf function-name fn)
      (setf class-name class)
      (setf parser-choice-sym parser-choice))

    (labels
        ((ensure-slot (name)
           (setf (gethash name slots) t)
           name)
         (add-option (form)
           (vector-push-extend form options))
         (add-result-form (form)
           (vector-push-extend form result-forms))
         (slot-parts (slot-description)
           (if (symbolp slot-description)
               (values (ensure-slot slot-description) `#',(parser-part-name slot-description))
               (destructuring-bind (slot-name parser-function) slot-description
                 (values (ensure-slot slot-name) parser-function)))))

      (dolist (production productions)
        (cond
          ((null production)
           (add-option `(parser-value nil)))

          ((symbolp production)
           (add-option `(,production ,iter)))

          (t
           (let* (optional-break-sym
                  (initargs
                   (loop :while production :for slot-description = (pop production) :nconc
                      (case slot-description
                        (&optional
                         (when optional-break-sym
                           (error "&optional may only be used once"))
                         (setf optional-break-sym (gensym "OPTIONAL-BREAK"))
                         nil)

                        (otherwise
                         (multiple-value-bind (slot-name parser-function)
                             (slot-parts slot-description)
                           (cond
                             (optional-break-sym
                              `(',slot-name (unless ,optional-break-sym
                                              (,parse
                                               (parser-choice ,iter
                                                 (funcall ,parser-function ,iter)
                                                 (progn
                                                   (setf ,optional-break-sym t)
                                                   (parser-value nil)))))))
                             (t
                              `(',slot-name (,parse (funcall ,parser-function ,iter)))))))))))

             (add-option
              (if optional-break-sym
                  `(let (,optional-break-sym)
                     (private-parser-block ,parse
                       (parser-value (make-instance ',class-name ,@initargs))))
                  `(private-parser-block ,parse
                     (parser-value (make-instance ',class-name ,@initargs)))))))))

      (unless (zerop (hash-table-count slots))
        (add-result-form
         `(define-nonterminal-class ,class-name ()
              ,(hash-table-keys slots))))

      (add-result-form
       `(define-advisable ,function-name (,iter)
          ,(format nil "This parser function tries to produce instance of the ~W class" class-name)
          (,parser-choice-sym ,iter
                              ,@(coerce options 'list)))))

    (apply 'progn-concatenate (coerce result-forms 'list))))
