;; Copyright 2019 Bradley Jensen
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
  (:use :common-lisp)
  (:import-from :shcl/core/utility #:optimization-settings #:required #:make-extensible-vector #:symbol-nconc-intern #:symbol-nconc-gensym #:progn-concatenate)
  (:import-from :shcl/core/advice #:define-advisable)
  (:import-from :shcl/core/data #:define-data #:define-cloning-setf-expander #:clone)
  (:import-from :shcl/core/iterator #:iterator #:do-iterator #:make-computed-iterator #:emit #:stop)
  (:import-from :shcl/core/sequence #:head #:tail #:empty-p #:attach #:empty-of #:walk #:popf #:attachf)
  (:import-from :fset)
  (:import-from :alexandria #:hash-table-keys)
  (:export
   ;; Foundations of parsing
   #:parser-throw #:parser-lookahead #:parser-try #:parser-choice #:parse-eof
   #:parse-object-of-type #:parser-repeat-times #:parser-repeat-until
   #:parse-with-sequence #:parser-var #:unset-parser-var #:parser-var-let*

   ;; Error types
   #:expected-eof #:expected-eof-got #:unexpected-eof
   #:unexpected-eof-expected-type #:type-mismatch #:type-mismatch-expected-type
   #:type-mismatch-got #:choice #:choice-errors
   #:unconditional-failure #:parser-error-vars #:parser-error-sequence

   ;; Convenience and high-level parsing tools
   #:define-terminal #:define-nonterminal #:syntax-tree
   #:define-nonterminal-class #:parse-failure #:parse-failure-error-object
   #:print-error))
(in-package :shcl/core/parser)

(optimization-settings)

(define-data syntax-tree ()
  ()
  (:documentation
   "A base class that nonterminals derive from.

See `define-nonterminal'."))

(defmethod print-object ((st syntax-tree) stream)
  (print-unreadable-object (st stream :type t :identity nil)))

(defgeneric print-error (err stream)
  (:documentation
   "Print a human-readable description of `err' to `stream'."))

(defmethod print-error (err stream)
  (if err
      (format stream "Unknown error (~W)" err)
      (format stream "Unknown error")))

(defvar *parser-sequence*)
(defvar *parser-vars*)
(defvar *parser-errors*)

(defun capture-sequence ()
  (when (boundp '*parser-sequence*)
    *parser-sequence*))

(defun capture-vars ()
  (if (boundp '*parser-vars*)
      *parser-vars*
      (fset:empty-map)))

(defgeneric parser-error-vars (parser-error)
  (:documentation
   "Return the var bindings in effect when the `parser-error' was
created."))

(defgeneric parser-error-sequence (parser-error)
  (:documentation
   "Return the sequence that was active when the `parser-error' was
created."))

(define-data parser-error ()
  ((vars
    :initform (capture-vars)
    :reader parser-error-vars)
   (sequence
    :initform (capture-sequence)
    :reader parser-error-sequence))
  (:documentation
   "A base class that built-in parse error classes derive from.

This class simply captures the parser state at the time when it is
created.  Namely, it captures the sequence and user-defined
variables (i.e. `parser-var'.)."))

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
  (format stream "Expected end of file, but instead found ~W" (expected-eof-got err)))

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
          (type-mismatch-got err)))

(defgeneric choice-errors (choice)
  (:documentation
   "Returns a vector of the errors produced by each of the potential
parses."))

(define-data choice (parser-error)
  ((errors
    :initarg :errors
    :reader choice-errors
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
      (iterator (choice-errors choice))))

  (let ((stack (make-extensible-vector)))
    (vector-push-extend (iterator (choice-errors choice)) stack)
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
  (when (empty-p (choice-errors err))
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
             (handler-bind
                 ((type-error (lambda (e) (break "type error ~A" e))))
               (print-error (parse-failure-error-object c) s))))
  (:documentation
   "An error condition to represent parse failure.

Note that parsers don't use the condition system to indicate parse
errors.  This condition is meant to be used at the interface between
parser logic and everything else."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parser-part-name (thing)
    (symbol-nconc-intern nil "PARSE-" thing)))

(defun parser-var (key &optional default)
  "Look up the value associated with `key' in the parser's state.

This function is a setf-able place.

Returns the value associated with the given key (or `default' if no
value is present) and a boolean indicating whether the parser had a
value for that key.

When a parse begins, the parser establishes a table specifically for
storing state associated with the parse.  The parser itself doesn't
store anything in the table.  You're free to store anything you want
in it.  For example, you can use a parser var to record extra metadata
about the objects that have been parsed.

Note that if the parser backtracks due to a parse error, the state of
the parser vars will be rolled back.  So, for example, the following
form will always return nil.

    (parser-choice
      (progn (setf (parser-var 'sym) 123)
             (assert (equal 123 (parser-var 'sym)))
             (parser-throw))
      (parser-var 'sym))

If you want to remove a parser var from the table, use
`unset-parser-var'."
  (check-type key symbol)
  (unless (boundp '*parser-vars*)
    (error "Cannot lookup parser var outside of a parse"))
  (multiple-value-bind (value found-p) (fset:lookup *parser-vars* key)
    (if found-p
        (values value t)
        (values default nil))))

(defun (setf parser-var) (value key)
  "Change the value associated with a parser var.

See `parser-var' and `unset-parser-var'."
  (check-type key symbol)
  (unless (boundp '*parser-vars*)
    (error "Cannot assign parser var outside of a parse"))
  (setf (fset:lookup *parser-vars* key) value))

(defun unset-parser-var (key)
  "Remove the value associated with the given `key' from the parser
var table.

See `parser-var'."
  (check-type key symbol)
  (unless (boundp '*parser-vars*)
    (error "Cannot unset parser var outside of a parse"))
  (setf *parser-vars* (fset:less *parser-vars* key))
  (values))

(defmacro parser-var-let1 ((var value) &body body)
  (let ((previous (gensym "PREVIOUS"))
        (valid-p (gensym "VALID-P"))
        (set-p (gensym "SET-P")))
    `(multiple-value-bind (,previous ,valid-p) (parser-var ',var)
       (let (,set-p)
         (unwind-protect
              (progn
                (setf (parser-var ',var) ,value)
                (setf ,set-p t)
                ,@body)
           (when ,set-p
             (if ,valid-p
                 (setf (parser-var ',var) ,previous)
                 (unset-parser-var ',var))))))))

(defmacro parser-var-let* (bindings &body body)
  "Establish bindings for a series of parser variables.

This macro is used just like `let*'.  The first element of each
binding is an unevaluated symbol name and the second element is an
evaluated value.  When control enters `body', the given bindings will
be in effect.  When control exits `body', the bound parser variables
will return to the value they had before control entered
`parser-var-let*'.  This macro behaves more or less like `let*' does
when establishing bindings for special variables.

Example usage:
    (parser-var-let*
        ((some-var (+ 1 2 3))
         (other-var (* 1 2 3)))
      (assert (eql (parser-var 'some-var) (parser-var 'other-var))))"
  (unless bindings
    (return-from parser-var-let*
      `(progn ,@body)))

  (labels
      ((normalize (binding)
         (when (symbolp binding)
           (setf binding (list binding nil)))
         (check-type binding list)
         (check-type (car binding) symbol)
         (when (null (cdr binding))
           (setf binding (list (car binding) nil)))
         (when (cdr (cdr binding))
           (error "binding must not have multiple value forms"))
         binding))
    `(parser-var-let1 ,(normalize (car bindings))
       (parser-var-let* ,(cdr bindings)
         ,@body))))

(defun produce-error-object ()
  (case (fset:size *parser-errors*)
    (0
     nil)
    (1
     (fset:first *parser-errors*))
    (otherwise
     (make-instance 'choice :errors *parser-errors*))))

(defmacro parse-with-sequence (sequence &body body)
  "Evaluate `body' with `sequence' used as the input sequence to be
parsed.

This form establishes a dynamic extent in which other parser
forms (e.g. `parser-var', `parser-try', `parser-throw', etc.) are able
to function.

`sequence' must be walkable.  See `walk'.

If `body' fails to parse then this macro will signal a `parse-failure'
error.  If `body' exits normally then this form produces 4 values: the
value returned by `body', the tip of the input sequence when parsing
finished, the state of the parser vars in effect when parsing
concluded (see `parser-var'), and a sequence containing all the errors
that were encountered during the parse."
  `(let ((*parser-sequence* (walk ,sequence))
         (*parser-vars* (fset:empty-map))
         (*parser-errors* (fset:empty-seq)))
     (parser-catch (values (progn ,@body) *parser-sequence* *parser-vars* *parser-errors*)
       (error 'parse-failure :error-object (produce-error-object)))))

(defconstant +parser-error+ '+parser-error+)

(defun parser-throw (&optional (error nil error-p))
  "Indicate that a parser error has occurred and that the parser
should backtrack.

If `error' is provided then it will be added to the list of failures
seen.  You should generally provide an error object (ideally a
subclass of `parser-error') so that useful parse errors can be
provided.  You should generally only omit the error argument when
you're re-throwing an error that already occurred."
  (when error-p
    (fset:push-last *parser-errors* error))
  (throw +parser-error+ nil))

(defmacro parser-catch (parser-form &body error-handler-body)
  (let ((parser-catch (gensym "PARSER-CATCH"))
        (original-vars (gensym "ORIGINAL-VARS")))
    `(let ((,original-vars *parser-vars*))
       (block ,parser-catch
         (catch +parser-error+
           (return-from ,parser-catch ,parser-form))
         (setf *parser-vars* ,original-vars)
         ,@error-handler-body))))

(defmacro parser-catch-soft (parser-form &body error-handler-body)
  (let ((original-sequence (gensym "ORIGINAL-SEQUENCE")))
    `(let ((,original-sequence *parser-sequence*))
       (parser-catch ,parser-form
         (unless (eq *parser-sequence* ,original-sequence)
           (parser-throw))
         ,@error-handler-body))))

(defmacro parser-try (&body body)
  "Evaluate `body', but rollback changes to the input sequence if the
parse fails.

If `body' exits normally then this macro behaves exactly like `progn'.
If `body' throws a parse error then the parser input sequence will be
returned to the position it was at when control entered this macro.
Thus, `parser-try' will turn a parse error that consumed input into a
parse error that did not.

See also `parser-lookahead'."
  (let ((sequence-value (gensym "SEQUENCE-VALUE"))
        (success-p (gensym "SUCCESS-P")))
    `(let ((,sequence-value *parser-sequence*)
           ,success-p)
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setf ,success-p t))
         (unless ,success-p
           (setf *parser-sequence* ,sequence-value))))))

(defmacro parser-lookahead (&body body)
  "Evaluate `body' but rollback changes to the input sequence if the
parse succeeds.

If `body' throws a parse error then this macro behaves exactly like
`progn'.  If `body' exits normally then the parser input sequence will
be returned to the position it was at when control entered this macro.
Thus, `parser-lookahead' will turn a successful parse that consumed
input into a successful parse that did not.

See also `parser-try'."
  (let ((sequence-value (gensym "SEQUENCE-VALUE")))
    `(let ((,sequence-value *parser-sequence*))
       (multiple-value-prog1
           (progn ,@body)
         (setf *parser-sequence* ,sequence-value)))))

(defmacro parser-choice (&body alternatives)
  "Evaluate each form in `alternatives' until one of them parses successfully.

Each form in `alternatives' is evaluated in turn.  If the form under
consideration parses successfully, then this macro will return the
produced values and stop evaluating alternatives.  If the form under
consideration fails to parse after consuming input, then this macro
will fail to parse in the same way.  If the form under consideration
fails to parse but doesn't consume input, then this macro will simply
try the next alternative.  If there are no other alternatives, this
macro will fail to parse in the same way."
  (let ((parser-choice (gensym "PARSER-CHOICE")))
    (labels
      ((handle-alternatives (alternatives)
         (assert alternatives)

         (when (null (cdr alternatives))
           (return-from handle-alternatives
             (car alternatives)))

         `(parser-catch-soft
           (return-from ,parser-choice
             ,(car alternatives)))))

      (when (null alternatives)
        (return-from parser-choice `(parser-throw (make-instance 'unconditional-failure))))

      (when (null (cdr alternatives))
        (return-from parser-choice (handle-alternatives alternatives)))

      `(block ,parser-choice
         ,@(loop :for tail :on alternatives :collect (handle-alternatives tail))))))

(defun parser-repeat-times-f (output-sequence min max body-fn)
  (unless min
    (setf min 0))
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))
  (let ((count 0))
    (loop
      :while (or (not max)
                 (< count max))
      :do
         (parser-choice
           (progn
             (attachf output-sequence
                      (funcall body-fn))
             (incf count))
           (if (>= count min)
               (return)
               (parser-throw)))))
  output-sequence)

(defmacro parser-repeat-times ((output-sequence &optional min max) &body body)
  "Parse `body' repeatedly and attach its results to `output-sequence'.

Setting `min' to nil (or failing to provide a value) is equivalent to
setting it to 0.  Setting `max' to nil (or failing to provide a value)
is equivalent to setting it to infinity.

This macro repeatedly evaluates `body' and attaches the result to
`output-sequence'.  Repetition will continue until either

a. `max' parses have occurred, or
b. `min' parses have occurred and `body' fails to parse without
   consuming the input sequence.

Unless an error occurs, this macro ultimately returns the modified
`output-sequence'.

If `body' fails to parse after consuming input then this macro will
also fail to parse.  If `body' fails to parse at least `min' times,
this macro will fail to parse."
  `(parser-repeat-times-f ,output-sequence ,min ,max (lambda () ,@body)))

(defun parser-repeat-until-f (output-sequence end-parser body-parser)
  (loop
    :while (parser-choice
             (progn (funcall end-parser)
                    nil)
             t)
    :do (attachf output-sequence (funcall body-parser)))
  output-sequence)

(defmacro parser-repeat-until ((output-sequence end-parser-form) &body body)
  "Parse `body' repeatedly and attach its results to `output-sequence'.

This macro repeatedly evaluates `body' and attaches the result to
`output-sequence'.  Repetition will continue until `end-parser-form'
parses successfully.  If `body' fails to parse or `end-parser-form'
fails to parse and consumes input, then this macro will also fail to
parse.

Unless an error occurs, this macro ultimately returns the modified
`output-sequence'."
  `(parser-repeat-until-f ,output-sequence (lambda () ,end-parser-form) (lambda () ,@body)))

(defun parse-eof ()
  "Parse successfully iff the sequence is empty.

This parser function never consumes input.  If the sequence has
reached the end of its values, this parser produces the value `:eof'."
  (if (empty-p *parser-sequence*)
      :eof
      (parser-throw (make-instance 'expected-eof :got (head *parser-sequence*)))))

(defun parse-object-of-type (type)
  "Parse successfully if the next object in the sequence is of type
`type'."
  (multiple-value-bind (value valid-p) (popf *parser-sequence*)
    (cond
      ((not valid-p)
       (parser-throw (make-instance 'unexpected-eof :expected-type type)))

      ((not (typep value type))
       (parser-throw (make-instance 'type-mismatch :expected-type type :got value)))

      (t
       value))))

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
  `(define-advisable ,name ()
     ,(format
       nil
       "Parse successfully if the next object satisfies the type ~W.

If the next object does not satisfy the type then this function does
not move the iterator."
       type)
     (parser-try
       (parse-object-of-type ',type))))

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

The parser function generated by this macro is advisable.  See
`shcl/core/advice:define-advice'.

Each form in `productions' describes a way to parse this nonterminal.
Each production is turned into a parser form and then combined with
`parser-choice'.  Thus, unlike a real grammar, the parser function
will use the first production to match.

1. If the production is nil then it always succeeds and returns nil.

2. If the production is a list then it is treated as a description of
   slots for the nonterminal class and how to fill the slots.  Each
   element of the list is a list of two elements and describes one
   slot of the class.  The first element of the pair is the name of
   the slot.  The second element is the form which should be evaluated
   to populate the slot.  A class will be generated that has all the
   slots named by all the productions of this type.  See
   `define-nonterminal-class'.  You may also include `&optional' in
   the production list.  After `&optional' appears, slots are
   initialized more conservatively.  If a slot initializer returns a
   parse failure but consumes no input then that slot and all
   remaining slots will be initialized to nil.

   As a shorthand, you may simply provide a symbol instead of a list
   as a slot description.  The symbol will be used as the name of the
   slot and the slot will be initialized using the function named by
   prefixing the symbol with PARSE-.  For exmaple, these production
   forms are equivalent.

       (foo bar baz)
       ((foo (parse-foo)) (bar (parse-bar)) (baz (parse-baz)))

3. If the production is a symbol then it is treated as a parser
   function name.  This production type simply defers parsing to the
   named parser function.  If it succeeds, then the nonterminal parser
   will emit the same value.  If it fails then the nonterminal parser
   will either try the next production or emit the same failure.  See
   `parser-choice'."
  (let ((slots (make-hash-table))
        (options (make-extensible-vector))
        (result-forms (make-extensible-vector))
        function-name
        class-name)

    (when (symbolp name-and-options)
      (setf name-and-options `(,name-and-options)))

    (destructuring-bind (class &key ((:function-name fn) (parser-part-name class)))
        name-and-options
      (setf function-name fn)
      (setf class-name class))

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
               (values (ensure-slot slot-description) `(,(parser-part-name slot-description)))
               (destructuring-bind (slot-name parser-form) slot-description
                 (values (ensure-slot slot-name) parser-form)))))

      (dolist (production productions)
        (cond
          ((null production)
           (add-option nil))

          ((symbolp production)
           (add-option `(,production)))

          (t
           (let* (optional-break-sym
                  (initargs
                    (loop :while production :for slot-description = (pop production)
                          :nconc
                          (case slot-description
                            (&optional
                             (when optional-break-sym
                               (error "&optional may only be used once"))
                             (setf optional-break-sym (gensym "OPTIONAL-BREAK"))
                             nil)

                            (otherwise
                             (multiple-value-bind (slot-name parser-form)
                                 (slot-parts slot-description)
                               (cond
                                 (optional-break-sym
                                  `(',slot-name (unless ,optional-break-sym
                                                  (parser-choice
                                                      ,parser-form
                                                    (progn
                                                      (setf ,optional-break-sym t)
                                                      nil)))))
                                 (t
                                  `(',slot-name ,parser-form)))))))))

             (add-option
              (if optional-break-sym
                  `(let (,optional-break-sym)
                     (make-instance ',class-name ,@initargs))
                  `(make-instance ',class-name ,@initargs)))))))

      (unless (zerop (hash-table-count slots))
        (add-result-form
         `(define-nonterminal-class ,class-name ()
              ,(hash-table-keys slots))))

      (add-result-form
       `(define-advisable ,function-name ()
          ,(format nil "This parser function tries to produce instance of the ~W class" class-name)
          (parser-choice
              ,@(coerce options 'list)))))

    (apply 'progn-concatenate (coerce result-forms 'list))))
