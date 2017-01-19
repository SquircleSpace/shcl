(defpackage :shcl/core/shell-readtable
  (:use :common-lisp :shcl/core/utility :shcl/core/data)
  (:import-from :fset)
  (:export
   #:shell-extensible-read #:build-shell-readtable #:define-shell-readtable
   #:with-dispatch-character #:with-default-handler #:with-handler #:use-table
   #:+standard-shell-readtable+))
(in-package :shcl/core/shell-readtable)

(optimization-settings)

(define-data dispatch-table ()
  ((%characters
    :type fset:map
    :initform (fset:with-default (fset:empty-map) nil)
    :updater dispatch-table-characters)))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *empty-shell-readtable* (make-instance 'dispatch-table))
  (define-symbol-macro +standard-shell-readtable+ *empty-shell-readtable*))

(defun table-handler (readtable character)
  "Look up the handler for the given character."
  (fset:lookup (dispatch-table-characters readtable) character))

(define-setf-expander table-handler (readtable character &environment env)
  (multiple-value-bind (vars vals set-vars set-form get-form) (get-setf-expansion readtable env)
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

(defun table-default (readtable)
  "Look up the default handler"
  (fset:lookup (dispatch-table-characters readtable) '#:anything))

(define-setf-expander table-default (readtable &environment env)
  (multiple-value-bind (vars vals set-vars set-form get-form) (get-setf-expansion readtable env)
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

(defun %map-subtable (readtable unhandled-characters handled-characters fn)
  (unless unhandled-characters
    (let ((result (funcall fn readtable)))
      (check-type result dispatch-table)
      (return-from %map-subtable result)))

  (destructuring-bind (char &rest rest) unhandled-characters
    (push char handled-characters)
    (multiple-value-bind (entry found) (table-handler readtable char)
      (unless (and found (typep entry 'dispatch-table))
        (error 'character-not-dispatch :character-sequence (reverse handled-characters)))
      (setf (table-handler readtable char)
            (%map-subtable entry rest handled-characters fn))
      readtable)))

(defun map-subtable (readtable character-sequence modifier)
  "Return a copy of readtable that has been modified such that the
subtable for the given character sequence has been replaced by the
return value of the modifier function.

The modifier function takes one argument: the existing subtable for
the character sequence."
  (setf character-sequence (if (typep character-sequence 'sequence)
                               (coerce character-sequence 'list)
                               (fset:convert 'list character-sequence)))
  (%map-subtable readtable character-sequence nil modifier))

(defun %subtable (readtable unhandled-characters handled-characters)
  (unless unhandled-characters
    (return-from %subtable readtable))

  (destructuring-bind (char &rest rest) unhandled-characters
    (push char handled-characters)
    (multiple-value-bind (entry found) (table-handler readtable char)
      (unless (and found (typep entry 'dispatch-table))
        (error 'character-not-dispatch :character-sequence (reverse handled-characters)))
      (%subtable entry rest handled-characters))))

(defun subtable (readtable character-sequence)
  (%subtable readtable (fset:convert 'list character-sequence) nil))

(define-setf-expander subtable (readtable character-sequence &environment env)
  (multiple-value-bind (vars vals set-vars set-form get-form) (get-setf-expansion readtable env)
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

(defun with-default-handler (readtable character-sequence handler &key (on-conflict :error))
  "Return a readtable where the given dispatch character sequence has
its default handler changed to `handler'.

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
    (map-subtable readtable character-sequence #'fn)))

(defun with-dispatch-character (readtable character-sequence &key (on-conflict :error))
  "Return a readtable where the given character sequence is a dispatch
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

`:replace' will replace the existing handler with an empty dispatch
table.  `:convert' will replace any existing handler with a dispatch
table with the given handler as the default handler.  `:error' signals
an error."
  (unless (find on-conflict #(:error :replace :convert))
    (error "on-conflict argument must be either :error, :replace:, or :convert"))
  (let* ((seq (fset:convert 'fset:seq character-sequence))
         (dispatch-sequence (fset:less-last seq))
         (terminal-character (fset:last seq)))
    (when (zerop (fset:size seq))
      (error "Character sequence must not be empty"))
    (labels
        ((fn (subtable)
           (multiple-value-bind (entry found) (table-handler subtable terminal-character)
             (cond
               ((typep entry 'dispatch-table)
                subtable)

               ((or (eq :replace on-conflict)
                    (not found))
                (setf (table-handler subtable terminal-character)
                      (make-instance 'dispatch-table))
                subtable)

               ((eq :convert on-conflict)
                (let ((replacement (make-instance 'dispatch-table)))
                  (setf (table-default replacement) entry)
                  (setf (table-handler subtable terminal-character) replacement)
                  subtable))

               (t
                (error 'character-already-set :character-sequence character-sequence
                       :value entry))))))
      (map-subtable readtable dispatch-sequence #'fn))))

(defun with-handler (readtable character-sequence handler &key (on-conflict :error))
  "Returns a readtable where the given character sequence has been
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
      
      (map-subtable readtable dispatch-sequence #'fn))))

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

             (error "Conflict for handler at ~A" key-stack))))

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

(defun use-table (readtable other-readtable)
  "Produce a table that contains the union of the given tables"
  (%use-table readtable other-readtable (make-extensible-vector)))

(defmacro build-shell-readtable (&body body)
  "Construct a table.

The result of this macro is equivalent to
(shcl/core/utility:-> #<empty-table> ,@body)"
  `(-> *empty-shell-readtable* ,@body))

(defmacro define-shell-readtable (name &body body)
  "Define a table suitable for use at compile time."
  (let (documentation)
    (when (typep (first body) 'string)
      (setf documentation (list (pop body))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,name (build-shell-readtable ,@body)
         ,@documentation))))

(defun %shell-extensible-read (stream readtable initiation-sequence fallback context)
  (let ((next-char (peek-char nil stream nil :eof)))
    ;; TODO: Isn't it a bit insane for default handlers to get a
    ;; single peek'd character when normal handlers only get read
    ;; characters?
    (vector-push-extend next-char initiation-sequence)
    (multiple-value-bind (value found) (table-handler readtable next-char)
      (when (not found)
        (return-from %shell-extensible-read (funcall (or value fallback) stream initiation-sequence context)))

    (read-char stream nil :eof)
    (let ((result
           (typecase value
             (dispatch-table
              (let ((inner-fallback (table-default value)))
                (%shell-extensible-read stream
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

(defun shell-extensible-read (stream context readtable)
  (labels
      ((fallback (s initiation-sequence c)
         (declare (ignore s c))
         (assert (equal 1 (length initiation-sequence)) nil
                 "This function should only run when the first table had no matches, but we had ~A" initiation-sequence)
         (return-from shell-extensible-read nil)))
    (%shell-extensible-read stream readtable (make-extensible-vector) #'fallback context)))
