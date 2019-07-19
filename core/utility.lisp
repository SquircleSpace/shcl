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

(defpackage :shcl/core/utility
  (:use :common-lisp :alexandria :bordeaux-threads)
  (:import-from :fset)
  (:import-from :closer-mop)
  (:shadow #:when-let #:when-let*)
  (:export
   #:whitespace-p #:as-> #:-> #:->> #:required
   #:required-argument-missing #:optimization-settings #:when-let #:when-let*
   #:try #:debug-log #:dump-logs #:status #:make-extensible-vector
   #:symbol-nconc-gensym #:symbol-nconc-intern #:progn-concatenate #:bodyify
   #:document #:define-documentation-type
   ;; Conditions
   #:not-implemented
   ;; Hooks
   #:define-hook #:add-hook #:remove-hook #:run-hook #:on-revival
   #:observe-revival #:on-dump #:observe-dump))
(in-package :shcl/core/utility)

(defmacro optimization-settings ()
  "Declaims standard optimization settings.

Put this at the top of every file!"
  `(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0))))

(optimization-settings)

(defvar *documentation* (make-hash-table))

(defun get-documentation (symbol doc-type)
  (let ((inner-table (gethash symbol *documentation*)))
    (when inner-table
      (nth-value 0 (gethash doc-type inner-table)))))

(defun (setf get-documentation) (new-value symbol doc-type)
  (let ((inner-table (gethash symbol *documentation*)))
    (cond
      ((and (not inner-table) (not new-value))
       (return-from get-documentation new-value))
      ((not inner-table)
       (setf inner-table (make-hash-table))
       (setf (gethash symbol *documentation*) inner-table)))
    (if new-value
        (setf (gethash doc-type inner-table) new-value)
        (remhash doc-type inner-table))
    (when (zerop (hash-table-count inner-table))
      (remhash symbol *documentation*))
    new-value))

(defmacro define-documentation-type (doc-type)
  "Define a new category of documentation."
  (check-type doc-type symbol)
  (let ((symbol (gensym "SYMBOL"))
        (doc-type-arg (gensym "DOC-TYPE"))
        (new-value (gensym "NEW-VALUE")))
    `(progn
       (defmethod documentation ((,symbol symbol) (,doc-type-arg (eql ',doc-type)))
         (get-documentation ,symbol ,doc-type-arg))

       (defmethod (setf documentation) (,new-value (,symbol symbol) (,doc-type-arg (eql ',doc-type)))
         (setf (get-documentation ,symbol ,doc-type-arg) ,new-value)))))

(defmacro document (symbol doc-type &body documentation-string)
  "Update the documentation for the given symbol.

Although this macro accepts a `&body', you must provide exactly one
string in the macro body.  The macro's lambda list uses `&body' purely
to trick editors into using better indentation."
  (unless documentation-string
    (error "document expects a docstring"))
  (when (cdr documentation-string)
    (error "document expects exactly one docstring"))
  (check-type symbol symbol)
  (check-type doc-type symbol)
  (check-type (car documentation-string) string)
  `(setf (documentation ',symbol ',doc-type) ,(car documentation-string)))

(defconstant +whitespace-characters+
  (if (boundp '+whitespace-characters+)
      (symbol-value '+whitespace-characters+)
      #(#\Space #\Linefeed #\Formfeed #\Vt #\Tab #\Return))
  "The set of characters which should be considered whitespace")

(defun whitespace-p (char)
  "Returns non-nil iff the given character is a whitespace character"
  (find char +whitespace-characters+))

(defmacro as-> (value sigil &body forms)
  "Thread the given value through the provided forms.

This is very similar to Clojure's macro with the same name.

This macro first binds `sigil' to `value'.  Then, it repeatedly binds
`sigil' to be the output of each form you provide in `forms'.  These
forms should produce the same result.

    (as-> value x
      (+ x 1)
      (* x x)
      3)

    (let ((x value)
          (x (+ x 1))
          (x (* x x))
          (x 3))
      x)"
  (labels
      ((binding (form)
         `(,sigil ,form)))
    `(let* ((,sigil ,value)
            ,@(mapcar #'binding forms))
       ,sigil)))

(defmacro -> (value &body forms)
  "Thread the given value through the provided forms.

This is very similar to Clojure's macro with the same name.

This macro is just a simpler form of `as->' where the sigil is
automatically inserted as the first argument to each function call.
As a convenience, some forms will be re-written to use `funcall'
automatically.  For example, these forms should have the same result.

    (let ((fn-4 (lambda (arg) (1+ arg))))
      (-> value
        (fn arg1 arg2)
        (fn2)
        #'fn3
        fn-4))

    (let ((fn-4 (lambda (arg) (1+ arg))))
      (as-> value #:sigil
        (fn #:sigil arg1 arg2)
        (fn2 #:sigil)
        (funcall #'fn3 #:sigil)
        (funcall fn-4 #:sigil)))"
  (let ((sigil (gensym "SIGIL")))
    (labels
        ((prepare (form)
           (cond ((and (consp form) (not (eq 'function (car form))))
                  `(,(car form) ,sigil ,@(cdr form)))
                 (t
                  `(funcall ,form ,sigil)))))
      `(as-> ,value ,sigil ,@(mapcar #'prepare forms)))))

(defmacro ->> (value &body forms)
  "Thread the given value through the provided forms.

This is very similar to Clojure's macro with the same name.

See `->'.  Unlike `->', this threads the value through as the last
argument to the function rather than the first argument."
  (let ((sigil (gensym "SIGIL")))
    (labels
        ((prepare (form)
           (cond ((and (consp form) (not (eq 'function (car form))))
                  `(,@form ,sigil))
                 (t
                  `(funcall ,form ,sigil)))))
      `(as-> ,value ,sigil ,@(mapcar #'prepare forms)))))

(defparameter *log-buffer* (make-array 1 :initial-element (make-array 0 :element-type 'string :fill-pointer t :adjustable t)
                                       :adjustable t :fill-pointer t))
(defconstant +maximum-log-lines-in-chunk+ 4096)
(defparameter *log-buffer-lock* (make-lock)
  "The lock that protects `*log-buffer*'.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *log-components* (make-hash-table))
  (defparameter *log-levels* (make-hash-table))

  (defstruct log-component)
  (defstruct log-level
    "A struct to represent everything that is known about a log
level."
    documentation))

(defmethod documentation ((log-level-name symbol) (doc-type (eql 'log-level)))
  (let ((log-level-struct (gethash log-level-name *log-levels*)))
    (when log-level-struct
      (log-level-documentation log-level-struct))))

(defmacro define-log-level (name &body options)
  (check-type name symbol)
  (let ((documentation (when (stringp (car options))
                         (pop options))))
    (when options
      (error "No options are valid for `define-log-level'"))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *log-levels*)
             (make-log-level
              ,@(when documentation
                  `(:documentation ,documentation)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-log-component (name)
    (let ((entry (gethash name *log-components*)))
      (unless entry
        (setf entry (make-log-component))
        (setf (gethash name *log-components*) entry))
      entry)))

(define-log-level error
  "A log level to represent errors.")
(define-log-level warning
  "A log level to represent warnings.")
(define-log-level status
  "A log level for standard logging.")

(defstruct log-line
  level
  component
  timestamp
  message)

(defparameter *log-stream* nil)

(defun emit-log-line (line output-stream)
  (let ((line-stream (make-string-output-stream)))
    (format line-stream "[~A|~A/~A] "
            (log-line-timestamp line)
            (package-name (log-line-component line))
            (symbol-name (log-line-level line)))
    (write-string (log-line-message line) line-stream)
    (format line-stream "~%")
    (write-string (get-output-stream-string line-stream) output-stream)))

(defmacro debug-log (level message &rest format-args)
  "Emit a log line."
  (let ((level-info (gethash level *log-levels*))
        (component-info (ensure-log-component *package*))
        (line (gensym "LINE"))
        (last (gensym "LAST")))
    (declare (ignore component-info))
    (unless level-info
      (error "No level known by the name ~A" level))
    `(let ((,line (make-log-line :level ',level
                                 :component ,*package*
                                 :timestamp (/ (coerce (get-internal-real-time) 'double-float) internal-time-units-per-second)
                                 :message (format nil ,message ,@format-args))))
       (with-lock-held (*log-buffer-lock*)
         (when *log-stream*
           (emit-log-line ,line *log-stream*))
         (symbol-macrolet
             ((,last (aref *log-buffer* (- (length *log-buffer*) 1))))
           (when (<= +maximum-log-lines-in-chunk+ (+ 1 (length ,last)))
             (vector-push-extend (make-array 0 :element-type 'string :fill-pointer t :adjustable t)
                                 *log-buffer*))
           (vector-push-extend ,line ,last)
           (values))))))

(defun dump-logs (&optional (output-stream *standard-output*))
  "Write out all recorded log lines to the given output stream."
  (with-lock-held (*log-buffer-lock*)
    (loop :for chunk :across *log-buffer* :do
       (loop :for line :across chunk :do
          (emit-log-line line output-stream)))
    (values)))

(define-condition not-implemented (warning error)
  ((feature
    :initarg :feature
    :initform ""
    :accessor not-implemented-feature
    :type string))
  (:report (lambda (c s) (format s "NOT IMPLEMENTED ~A~%" (not-implemented-feature c))))
  (:documentation
   "A condition indicating that a feature hasn't been implemented
yet."))

(defmacro define-hook (name &optional documentation)
  "Create a hook.

A hook is more or less an unordered collection of functions.  When the
hook is run with `run-hook', each function will be called once."
  `(defparameter ,name
     (fset:empty-set)
     ,documentation))

(defun add-hook (hook function)
  "Add a function a function to a hook."
  (check-type hook symbol)
  (check-type function (or symbol function))
  (setf (symbol-value hook) (fset:with (symbol-value hook) function))
  hook)

(defun remove-hook (hook function-symbol)
  "Remove a function from a hook."
  (check-type hook symbol)
  (check-type function-symbol symbol)
  (setf (symbol-value hook) (fset:less (symbol-value hook) function-symbol))
  hook)

(defun run-hook (hook &rest args)
  "Run each function in the provided hook.

Any arguments provided in `args' will be passed to every function in
the hook."
  (fset:do-set (fn (symbol-value hook))
    (apply fn args)))

(define-hook *revival-hook*
  "This hook is run when the process starts.")

(defmacro on-revival (function-symbol)
  "When the process starts, call the named function."
  `(add-hook '*revival-hook* ',function-symbol))

(defun observe-revival ()
  "The process has started!"
  (run-hook '*revival-hook*))

(pushnew 'observe-revival uiop:*image-restore-hook*)

(define-hook *dump-hook*
  "This hook is run when an executable is being prepared.

Note, it is as of yet undetermined whether this hook will run or not
for lisp compilers like ECL.")

(defmacro on-dump (function-symbol)
  "When saving an executable, call the named function."
  `(add-hook '*dump-hook* ',function-symbol))

(defun observe-dump ()
  "We're saving an executable!"
  (run-hook '*dump-hook*))

(pushnew 'observe-dump uiop:*image-dump-hook*)

(defun %when-let (let-sym bindings body)
  (let ((block (gensym (format nil "WHEN-~A-BLOCK" (symbol-name let-sym)))))
    (labels
        ((transform (binding)
           (when (symbolp binding)
             (setf binding (list binding nil)))
           (destructuring-bind (variable &optional value) binding
             (let ((value-sym (gensym "VALUE")))
               (setf value `(let ((,value-sym ,value))
                              (if ,value-sym
                                  ,value-sym
                                  (return-from ,block))))
               (list variable value)))))
      `(block ,block
         (,let-sym ,(mapcar #'transform bindings)
           ,@body)))))

(defmacro when-let* (bindings &body body)
  "Establish bindings (like `let*' would).  If any variable is bound to
nil, then the whole `when-let*' short circuts and evaluates to nil."
  (%when-let 'let* bindings body))

(defmacro when-let (bindings &body body)
  "Establish bindings (like `let' would).  If any variable is bound to
nil, then the whole `when-let' short circuts and evaulates to nil."
  (%when-let 'let bindings body))

(define-condition required-argument-missing (error)
  ()
  (:documentation
   "A condition for indicating that a required argument was not provided."))

(defmacro required ()
  "This form always signals an error of type `required-argument-missing'."
  `(error 'required-argument-missing))

(defmacro try (exceptionable-form &body clauses)
  "A better version of `catch'.

When you use `catch', you can't distinguish between normal execution and a thrown value.  Sometimes that is desirable.  Other times, you might like to know the difference.  With `signal' and conditions, you have that flexibility.  However, the condition system is fairly heavyweight and thus isn't appropriate for all use cases.  The `try' macro attempts to bring some of the flexibility of `handler-case' to `catch' and tries to emulate the control-flow of a more traditional exception system.

Example:
(try
    (throw 'foobar (values 1 2 3))
  (foobar (x y z) (+ x y z))
  (baz (a) (frobnosticate a)))

It is unspecified what happens if one of the handler clauses throws a
tag named in a different clause."
  ;; We're going to do a slightly insane thing.  We're going to build
  ;; up a series of nested forms that look like this.
  ;; (multiple-value-call #'foo-handler
  ;;   (catch 'foo
  ;;     (return-from no-problem
  ;;       (multiple-value-call #'bar-handler
  ;;         (catch 'bar
  ;;           (return-from no-problem <...>))))))
  ;; If nothing is thrown, then the return-from form will execute.  If
  ;; something is thrown, then we bypass the return-from and call the
  ;; handler instead.  When the handler returns, its wrapping
  ;; return-from form will cause us to exit the try form altogether.
  (let ((no-problem (gensym "NO-PROBLEM"))
        labels-forms
        tag-label-alist)
    (dolist (clause clauses)
      (destructuring-bind (tag lambda-list &body body) clause
        (let ((label (gensym (symbol-name tag))))
          (push (cons tag label) tag-label-alist)
          (push `(,label ,lambda-list ,@body) labels-forms))))
    (setf labels-forms (nreverse labels-forms)
          tag-label-alist (nreverse tag-label-alist))

    (labels
        ((catch-form (tag-alist)
           (if (null tag-alist)
               exceptionable-form
               (let* ((head (car tag-alist))
                      (rest (cdr tag-alist))
                      (tag (car head))
                      (label (cdr head)))
                   `(multiple-value-call #',label
                      (catch ',tag
                        (return-from ,no-problem ,(catch-form rest))))))))
      `(block ,no-problem
         (labels (,@labels-forms)
           ,(catch-form tag-label-alist))))))

(defun make-extensible-vector
    (&key
       (initial-size 0)
       (initial-element nil initial-element-p)
       (initial-contents nil initial-contents-p)
       (element-type t)
       (fill-pointer t))
  "This function provides a quick way to make a single-dimensional,
adjustable array with a fill pointer."
  (cond
    ((and initial-element-p initial-contents-p)
     (error "Can't specify both initial-element and initial-contents"))

    ((not fill-pointer)
     (error "fill-pointer cannot be nil"))

    (initial-element-p
     (make-array initial-size :adjustable t :fill-pointer fill-pointer :initial-element initial-element :element-type element-type))

    (initial-contents-p
     (make-array initial-size :adjustable t :fill-pointer fill-pointer :initial-contents initial-contents :element-type element-type))

    (t
     (make-array initial-size :adjustable t :fill-pointer fill-pointer :element-type element-type))))

(defun %symbol-nconc (&rest bits)
  (let ((stream (make-string-output-stream)))
    (dolist (bit bits)
      (etypecase bit
        (string
         (write-string bit stream))
        (symbol
         (write-string (symbol-name bit) stream))))
    (get-output-stream-string stream)))

(defun symbol-nconc-gensym (first-bit &rest other-bits)
  "Create a gensym whose name is the concatenation of all the
fragments provided.

If one of the provided fragments is a symbol, it is turned into a
string with `symbol-name'."
  (gensym (apply '%symbol-nconc first-bit other-bits)))

(defun symbol-nconc-intern (package first-bit &rest other-bits)
  "Intern a symbol in the given package whose name is the
concatenation of all the fragments provided.

If one of the provided fragments is a symbol, it is turned into a
string with `symbol-name'.

If a non-nil `package' is provided, it will be passed to `intern'."
  (if package
      (intern (apply '%symbol-nconc first-bit other-bits) package)
      (intern (apply '%symbol-nconc first-bit other-bits))))

(defun progn-concatenate (&rest forms)
  "Return a form equivalent to evaluating each form in `forms' but
with unnecessary `progn' uses removed."
  (let ((result (list 'progn)))
    (dolist (form forms)
      (cond
        ((and (consp form) (eq 'progn (car form)))
         (dolist (subform (cdr form))
           (push subform result)))
        (form
         (push form result))))

    (cond
      ((cddr result)
       (nreverse result))
      ((cdr result)
       (car result)))))

(defun bodyify (form)
  "Turn a single form into a list of forms appropriate for splicing
with ,@ in a macro.

If `form' is a `progn' form, then this simply returns the cdr of
`form'.  Otherwise, this returns a list containing `form'.  This is
useful if you want to unroll `progn' forms in your macros."
  (if (and (consp form)
           (eq 'progn (car form)))
      (cdr form)
      (list form)))
