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

(defpackage :shcl/core/shell-readtable
  (:use :common-lisp :shcl/core/utility :shcl/core/data)
  (:import-from :fset)
  (:export
   #:dispatch-table-read #:with-dispatch-character #:with-default-handler
   #:with-handler #:use-table #:subtable #:*empty-shell-readtable*
   #:dispatch-table))
(in-package :shcl/core/shell-readtable)

(optimization-settings)

(define-data dispatch-table ()
  ((%characters
    :type fset:map
    :initform (fset:empty-map)
    :updater dispatch-table-characters))
  (:documentation
   "A class representing a super-powered readtable.

A normal Common Lisp readtable supports binding a handler to either a
character (e.g. #\') or to a two-character sequence (e.g. \"#'\").
There isn't an easy way to bind a handler to a three character
sequence.  You cannot tell the readtable to treat a two-character
sequence like it treats a dispatch character.  Put another way, you
cannot nest dispatch characters.

A `dispatch-table' is like a Common Lisp readtable.  Unlike lisp
readtables, `dispatch-table' supports nesting.  For example, you can
designate \"#\" as a dispatch sequence.  Then, much like a lisp
readtable, you can define handlers for two character sequences
starting with #\#.  You can also designate some of those two character
sequences as dispatch sequences.  For example, you could designate
\"#a\" as a dispatch sequence.  Again, you would be able to define
handlers for all the three character sequences starting with \"#a\".
You can keep nesting dispatch characters as deep as you wish.

`dispatch-table' also supports defining a default handler for a
dispatch sequence.  This default handler will be used when there isn't
a specific handler set for the character that follows a dispatch
sequence.

Note that `dispatch-table' does not treat numeral characters
specially.  In a normal lisp readtable, number characters that follow
a dispatch character are bundled up and passed to the handler
function.  `dispatch-table' does not do that.

Use `with-dispatch-character', `with-default-handler', `with-handler',
and `use-table' to create dispatch tables that have handlers installed
in them."))

(defmethod make-load-form ((table dispatch-table) &optional env)
  (declare (ignore env))
  (let ((default (table-default table))
        (characters (fset:convert 'hash-table (dispatch-table-characters table))))
    (values
     `(make-instance ',(class-of table))
     `(setf (slot-value ',table '%characters) (fset:with-default
                                                  (fset:convert 'fset:map ,characters)
                                                ,default)))))

(defmethod print-object ((table dispatch-table) stream)
  (print-unreadable-object (table stream)
    (format stream "~A" (dispatch-table-characters table))))

(defparameter *empty-shell-readtable* (make-instance 'dispatch-table)
  "A `dispatch-table' devoid of handlers.")

(defun table-handler (dispatch-table character)
  "Look up the handler for the given character."
  (fset:lookup (dispatch-table-characters dispatch-table) character))

(define-setf-expander table-handler (dispatch-table character &environment env)
  (multiple-value-bind (vars vals set-vars set-form get-form) (get-setf-expansion dispatch-table env)
    (let ((set-var (GENSYM "SET-VAR"))
          (char (gensym "CHAR")))
      (values
       `(,@vars ,char)
       `(,@vals ,character)
       `(,set-var)
       `(let ((,(car set-vars) ,get-form))
            (setf (fset:lookup (dispatch-table-characters ,(car set-vars)) ,char) ,set-var)
            ,set-form
            ,set-var)
       `(table-handler ,get-form ,char)))))

(defun table-default (dispatch-table)
  "Look up the default handler"
  (fset:lookup (dispatch-table-characters dispatch-table) '#:anything))

(define-setf-expander table-default (dispatch-table &environment env)
  (multiple-value-bind (vars vals set-vars set-form get-form) (get-setf-expansion dispatch-table env)
    (let ((set-var (gensym "SET-VAR")))
      (values
       vars
       vals
       `(,set-var)
       `(let ((,(car set-vars) ,get-form))
          (setf (dispatch-table-characters ,(car set-vars))
                (fset:with-default (dispatch-table-characters ,(car set-vars)) ,set-var))
          ,set-form
          ,set-var)
       `(table-default ,get-form)))))

(define-condition character-already-set (error)
  ((sequence
    :accessor character-already-set-sequence
    :initform (required)
    :initarg :character-sequence)
   (value
    :accessor character-already-set-value
    :initarg :value
    :initform (required)))
  (:report (lambda (c s) (format s "~A is already set to ~A" (character-already-set-sequence c) (character-already-set-value c)))))

(define-condition character-not-dispatch (error)
  ((sequence
    :accessor character-not-dispatch-sequence
    :initform (required)
    :initarg :character-sequence))
  (:report (lambda (c s) (format s "~W is not a dispatch character" (character-not-dispatch-sequence c)))))

(defun %map-subtable (dispatch-table unhandled-characters handled-characters fn)
  (unless unhandled-characters
    (let ((result (funcall fn dispatch-table)))
      (check-type result dispatch-table)
      (return-from %map-subtable result)))

  (destructuring-bind (char &rest rest) unhandled-characters
    (push char handled-characters)
    (multiple-value-bind (entry found) (table-handler dispatch-table char)
      (unless (and found (typep entry 'dispatch-table))
        (error 'character-not-dispatch :character-sequence (reverse handled-characters)))
      (setf (table-handler dispatch-table char)
            (%map-subtable entry rest handled-characters fn))
      dispatch-table)))

(defun map-subtable (dispatch-table character-sequence modifier)
  "Return a copy of `dispatch-table' that has been modified such that the
subtable for the given character sequence has been replaced by the
return value of the modifier function.

The modifier function takes one argument: the existing subtable for
the character sequence."
  (setf character-sequence (if (typep character-sequence 'sequence)
                               (coerce character-sequence 'list)
                               (fset:convert 'list character-sequence)))
  (%map-subtable dispatch-table character-sequence nil modifier))

(defun %subtable (dispatch-table unhandled-characters handled-characters)
  (unless unhandled-characters
    (return-from %subtable dispatch-table))

  (destructuring-bind (char &rest rest) unhandled-characters
    (push char handled-characters)
    (multiple-value-bind (entry found) (table-handler dispatch-table char)
      (unless (and found (typep entry 'dispatch-table))
        (error 'character-not-dispatch :character-sequence (reverse handled-characters)))
      (%subtable entry rest handled-characters))))

(defun subtable (dispatch-table character-sequence)
  "Retrieve the inner `dispatch-table' for the given character sequence.

If you have nominated `character-sequence' as a dispatch sequence,
this function will return the `dispatch-table' associated with that
dispatch sequence.  This function signals an error if the given
character sequence hasn't been designated as a dispatch character
sequence.

This is a `setf'-able place, but keep in mind that `dispatch-table's
are immutable.  So, this will only mutate the place where
`dispatch-table' is stored."
  (etypecase character-sequence
    (list)
    (sequence
     (setf character-sequence (coerce character-sequence 'list)))
    (fset:seq
     (setf character-sequence (fset:convert 'list character-sequence))))
  (%subtable dispatch-table character-sequence nil))

(define-setf-expander subtable (dispatch-table character-sequence &environment env)
  (multiple-value-bind (vars vals set-vars set-form get-form) (get-setf-expansion dispatch-table env)
    (let ((set-var (gensym "SET-VAR"))
          (chars (gensym "CHARS")))
      (values
       `(,@vars ,chars)
       `(,@vals ,character-sequence)
       `(,set-var)
       `(let ((,(car set-vars) (map-subtable ,get-form ,chars (constantly ,set-var))))
          ,set-form
          ,set-var)
       `(subtable ,get-form ,chars)))))

(defun with-default-handler (dispatch-table character-sequence handler &key (on-conflict :error))
  "Return a `dispatch-table' where the given dispatch character
sequence has its default handler changed to `handler'.

See `with-dispatch-character'.

If there is a conflict, the `on-conflict' argument dictates what
should be done.  Valid options are `:error' and `:replace'.  `:error'
signals an error.  `:replace' removes any existing handler and
installs the given one.  A conflict occurs when there is already a
non-nil default handler for the given character sequence and `handler'
is not `eq' to it.

`handler' must be a function (or otherwise funcallable) that takes 3 arguments:
- the stream being read from,
- the sequence of characters that were read prior to this handler being called,
- and the context object provided during reading."
  (unless (find on-conflict #(:error :replace))
    (error "on-conflict argument must be either :error or :replace"))
  (labels
      ((fn (subtable)
         (let ((default (table-default subtable)))
           (cond
             ((eq default handler)
              subtable)

             ((or (eq on-conflict :replace)
                  (not default))
              (setf (table-default subtable) handler)
              subtable)

             (t
              (error 'character-already-set :character-sequence character-sequence
                     :value default))))))
    (map-subtable dispatch-table character-sequence #'fn)))

(defun with-dispatch-character (dispatch-table character-sequence &key (on-conflict :error) use-table)
  "Return a `dispatch-table' where the given character sequence is a dispatch
character sequence.

This is approximately analogous to `make-dispatch-macro-character',
except it permits nesting.  For example, if (#\#) is a dispatch
character sequence, then you can define (#\# #\|) to also be a
dispatch character sequence.  After a character sequence has been made
into a dispatch character sequence, you may define handlers for
subsequent characters with `with-handler'.  Continuing the example
from earlier, you could establish a handler for the character
sequence (#\# #\,) or (#\# #\| #\!).

Unlike `make-dispatch-macro-character', you may establish a default
handler that runs if the dispatch character sequence is followed by a
character that isn't bound to a handler.  See `with-default-handler'.

The `on-conflict' argument establishes what should happen if the given
character sequence is already bound to a handler.  If the character
sequence is already a dispatch character sequence, there is no
conflict.  Valid values are `:error', `:replace', and `:convert'.

`:replace' will replace the existing handler with a new dispatch
table.  `:convert' will replace any existing handler with a dispatch
table with the existing handler as the default handler.  `:error' signals
an error.

The `use-table' argument allows you to provide a dispatch table that
should be merged (with the `use-table' function) into the dispatch
table for the given character sequence.  The following forms are
semantically equivalent.

    (with-dispatch-character table sequence :on-conflict strategy :use-table other-table)
    (progn (with-dispatch-character table sequence :on-conflict strategy)
           (setf (subtable table sequence) (use-table (subtable table sequence) other-table)))"
  (unless (find on-conflict #(:error :replace :convert))
    (error "on-conflict argument must be either :error, :replace:, or :convert"))
  (let* ((seq (fset:convert 'fset:seq character-sequence))
         (dispatch-sequence (fset:less-last seq))
         (terminal-character (fset:last seq)))
    (when (zerop (fset:size seq))
      (error "Character sequence must not be empty"))
    (labels
        ((merge-tables (subtable)
           (when use-table
             (let ((handler (table-handler subtable terminal-character)))
               (setf (table-handler subtable terminal-character) (use-table handler use-table))))
           subtable)
         (fn (subtable)
           (multiple-value-bind (entry found) (table-handler subtable terminal-character)
             (cond
               ((typep entry 'dispatch-table)
                (merge-tables subtable))

               ((or (eq :replace on-conflict)
                    (not found))
                (setf (table-handler subtable terminal-character)
                      (make-instance 'dispatch-table))
                (merge-tables subtable))

               ((eq :convert on-conflict)
                (let ((replacement (make-instance 'dispatch-table)))
                  (setf (table-default replacement) entry)
                  (setf (table-handler subtable terminal-character) replacement)
                  (merge-tables subtable)))

               (t
                (error 'character-already-set :character-sequence character-sequence
                       :value entry))))))
      (map-subtable dispatch-table dispatch-sequence #'fn))))

(defun with-handler (dispatch-table character-sequence handler &key (on-conflict :error))
  "Returns a `dispatch-table' where the given character sequence has been
bound to the given handler.

For information about the `on-conflict' and `handler' arguments, see
`with-default-handler'.  See also `with-dispatch-character'."
  (unless (find on-conflict #(:error :replace))
    (error "on-conflict argument must be either :error or :replace"))
  (let* ((seq (fset:convert 'fset:seq character-sequence))
         (dispatch-sequence (fset:less-last seq))
         (terminal-character (fset:last seq)))
    (when (zerop (fset:size seq))
      (error "Character sequence must not be empty"))
    (labels
        ((fn (subtable)
           (multiple-value-bind (entry found) (table-handler subtable terminal-character)
             (cond
               ((and found
                     (eq entry handler))
                subtable)

               ((or (not found)
                    (eq on-conflict :replace))
                (setf (table-handler subtable terminal-character)
                      handler)
                subtable)

               (t
                (error 'character-already-set :character-sequence character-sequence
                       :value entry))))))
      
      (map-subtable dispatch-table dispatch-sequence #'fn))))

(defun %use-table (base other key-stack)
  (let ((other-iter (fset:iterator (dispatch-table-characters other)))
        (result base))
    (labels
        ((merge-tables (key other-value)
           (multiple-value-bind (base-value base-found) (table-handler base key)
             (unless base-found
               (setf (table-handler result key) other-value)
               (return-from merge-tables))

             (when (eq base-value other-value)
               (return-from merge-tables))

             (when (and (typep base-value 'dispatch-table)
                        (typep other-value 'dispatch-table))
               (setf (table-handler result key) (%use-table base-value other-value key-stack))
               (return-from merge-tables))

             (error "Conflict for handler at ~A.  Have ~A, tried to add ~A" key-stack base-value other-value))))

      ;; Merge handlers
      (loop :while (not (funcall other-iter :done?)) :do
         (multiple-value-bind (key value real) (funcall other-iter :get)
           (declare (ignore real))
           (vector-push-extend key key-stack)
           (merge-tables key value)
           (vector-pop key-stack))))

    ;; Merge default handler
    (cond
      ((or (eq (table-default base) (table-default other))
           (and (table-default base) (not (table-default other)))))
      ;; Nothing to do

      ((and (not (table-default base))
            (table-default other))
       (setf (table-default result) (table-default other)))

      (t
       (error "Cannot merge default handlers at ~A" key-stack)))
    result))

(defun use-table (dispatch-table other-dispatch-table)
  "Produce a table that contains the union of the given tables"
  (%use-table dispatch-table other-dispatch-table (make-extensible-vector)))

(defun %dispatch-table-read (stream dispatch-table initiation-sequence fallback context)
  (let ((next-char (peek-char nil stream nil :eof)))
    (multiple-value-bind (value found) (table-handler dispatch-table next-char)
      (when (not found)
        (return-from %dispatch-table-read (funcall (or value fallback) stream initiation-sequence context)))

      (vector-push-extend (read-char stream nil :eof) initiation-sequence)

      (let ((result
             (typecase value
               (dispatch-table
                (let ((inner-fallback (table-default value)))
                  (%dispatch-table-read stream
                                        value
                                        initiation-sequence
                                        (or inner-fallback
                                            (lambda (s i c)
                                              (declare (ignore s c))
                                              (error "Unhandled dispatch character sequence ~A" i)))
                                        context)))

               (t
                (funcall value stream initiation-sequence context)))))

        result))))

(defun dispatch-table-read (stream context dispatch-table &key no-match-value)
  "Attempt to read something from `stream' using a `dispatch-table'.

`stream' is the input stream to read a value from.

`context' is an arbitrary object that will be passed to the handlers.
Typically, there will be some conventional value to pass for this
argument.

`dispatch-table' is the table to use while reading.

The return value of this function is the return value produced by
whatever handler in `dispatch-table' ends up running.
`no-match-value' is the value that will be returned if the
`dispatch-table' had no handler for the next character in `stream'.
If this value is returned then no characters in `stream' were
consumed."
  (labels
      ((fallback (s initiation-sequence c)
         (declare (ignore s c))
         (assert (equal 0 (length initiation-sequence)) nil
                 "This function should only run when the first table had no matches, but we had ~A" initiation-sequence)
         (return-from dispatch-table-read no-match-value)))
    (%dispatch-table-read stream dispatch-table (make-extensible-vector) #'fallback context)))
