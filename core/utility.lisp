(defpackage :shcl/core/utility
  (:use :common-lisp :alexandria :bordeaux-threads)
  (:import-from :fset)
  (:import-from :closer-mop)
  (:shadow #:when-let #:when-let*)
  (:export
   #:whitespace-p #:as-> #:-> #:->> #:define-once-global #:required
   #:required-argument-missing #:optimization-settings #:when-let #:when-let*
   #:try #:debug-log #:dump-logs #:status #:make-extensible-vector
   #:symbol-nconc-gensym #:symbol-nconc-intern
   ;; Hooks
   #:define-hook #:add-hook #:remove-hook #:run-hook #:on-revival
   #:observe-revival #:on-dump #:observe-dump))
(in-package :shcl/core/utility)

(defmacro optimization-settings ()
  "Declaims standard optimization settings.

Put this at the top of every file!"
  `(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0))))

(optimization-settings)

(defconstant +whitespace-characters+
  (if (boundp '+whitespace-characters+)
      +whitespace-characters+
      #(#\Space #\Linefeed #\Tab #\Return))
  "The set of characters which should be considered whitespace")

(defun whitespace-p (char)
  "Returns non-nil iff the given character is a whitespace character"
  (find char +whitespace-characters+))

(defmacro as-> (value sigil &body forms)
  (let ((form value))
    (dolist (fn-call forms)
      (let (found)
        (setf form
              (loop :for elt :in fn-call :collecting
                 (if (eq elt sigil)
                     (if found
                         (error "Multiple sigils found in ~A" fn-call)
                         (progn (setf found t) form))
                     elt)))
        (unless found
          (error "No sigils found in ~A" fn-call))))
    form))

(defmacro -> (value &body forms)
  (let ((sigil (gensym "SIGIL")))
    (labels
        ((prepare (form)
           (cond ((and (consp form) (not (eq 'function (car form))))
                  `(,(car form) ,sigil ,@(cdr form)))
                 (t
                  `(funcall ,form ,sigil)))))
      `(as-> ,value ,sigil ,@(mapcar #'prepare forms)))))

(defmacro ->> (value &body forms)
  (let ((sigil (gensym "SIGIL")))
    (labels
        ((prepare (form)
           (cond ((and (consp form) (not (eq 'function (car form))))
                  `(,@form ,sigil))
                 (t
                  `(funcall ,form ,sigil)))))
      `(as-> ,value ,sigil ,@(mapcar #'prepare forms)))))

(defmacro define-once-global (name initform &body options)
  "Define a global variable.

The variable is initialized the first time it is accessed and is
initialized at most once.  Redefining the variable with
`define-once-global' will reset the variable to be uninitialized."
  (check-type name symbol)
  (let* ((value (gensym "VALUE"))
         (setter-value (gensym "SETTER-VALUE"))
         (lock (gensym "LOCK"))
         (documentation (second (find :documentation options :key 'car)))
         (no-lock (second (find :no-lock options :key 'car)))
         (read-only (second (find :read-only options :key 'car)))
         (lock-form (if no-lock '(progn) `(with-lock-held (,lock)))))

    (check-type documentation (or null string))
    (when documentation
      (setf documentation (list documentation)))
    (unless (member no-lock '(nil t))
      (error ":no-lock option must have value nil or t"))
    (unless (member read-only '(nil t))
      (error ":read-only option must have value nil or t"))
    `(progn
       ,@(unless no-lock `((defvar ,lock (bordeaux-threads:make-lock ,(symbol-name name)))))
       (defvar ,value)
       (defun ,name ()
         ,@documentation
         (,@lock-form
          (unless (boundp ',value)
            (setf ,value ,initform))
          ,value))
       ,@(unless read-only
           `((defun (setf ,name) (,setter-value)
               (,@lock-form
                (setf ,value ,setter-value)
                ,setter-value))))
       (define-symbol-macro ,name (,name)))))

(defparameter *log-buffer* (make-array 1 :initial-element (make-array 0 :element-type 'string :fill-pointer t :adjustable t)
                                       :adjustable t :fill-pointer t))
(defconstant +maximum-log-lines-in-chunk+ 4096)
(defparameter *log-buffer-lock* (make-lock)
  "The lock that protects `*log-buffer*'.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *log-components* (make-hash-table))
  (defparameter *log-levels* (make-hash-table))

  (defstruct log-component)
  (defstruct log-level))

(defmacro define-log-level (name &body options)
  (check-type name symbol)
  (when options
    (error "No options are valid for `define-log-level'"))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *log-levels*) (make-log-level))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-log-component (name)
    (let ((entry (gethash name *log-components*)))
      (unless entry
        (setf entry (make-log-component))
        (setf (gethash name *log-components*) entry))
      entry)))

(define-log-level error)
(define-log-level warning)
(define-log-level status)

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
  (with-lock-held (*log-buffer-lock*)
    (loop :for chunk :across *log-buffer* :do
       (loop :for line :across chunk :do
          (emit-log-line line output-stream)))
    (values)))

(defstruct hook
  (functions (fset:empty-set)))

(defmacro define-hook (name &optional documentation)
  "Create a hook.

A hook is more or less an unordered collection of functions.  When the
hook is run with `run-hook', each function will be called once."
  `(defparameter ,name
     (make-hook)
     ,documentation))

(defun add-hook (hook function-symbol)
  "Add a function a function to a hook."
  (check-type hook hook)
  (check-type function-symbol symbol)
  (setf (hook-functions hook) (fset:with (hook-functions hook) function-symbol))
  hook)

(defun remove-hook (hook function-symbol)
  "Remove a function from a hook."
  (check-type hook hook)
  (check-type function-symbol symbol)
  (setf (hook-functions hook) (fset:less (hook-functions hook) function-symbol))
  hook)

(defun run-hook (hook)
  "Run each function in the provided hook."
  (fset:do-set (fn (hook-functions hook))
    (funcall fn)))

(define-hook *revival-hook*
  "This hook is run when the process starts.")

(defmacro on-revival (function-symbol)
  "When the process starts, call the named function."
  `(add-hook *revival-hook* ',function-symbol))

(defun observe-revival ()
  "The process has started!"
  (run-hook *revival-hook*))

(define-hook *dump-hook*
  "This hook is run when an executable is being prepared.

Note, it is as of yet undetermined whether this hook will run or not
for lisp compilers like ECL.")

(defmacro on-dump (function-symbol)
  "When saving an executable, call the named function."
  `(add-hook *dump-hook* ',function-symbol))

(defun observe-dump ()
  "We're saving an executable!"
  (run-hook *dump-hook*))

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
  (gensym (apply '%symbol-nconc first-bit other-bits)))

(defun symbol-nconc-intern (package first-bit &rest other-bits)
  (if package
      (intern (apply '%symbol-nconc first-bit other-bits) package)
      (intern (apply '%symbol-nconc first-bit other-bits))))
