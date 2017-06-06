(defpackage :shcl/core/debug
  (:use
   :common-lisp :trivial-gray-streams :shcl/core/utility :shcl/core/iterator)
  (:import-from :closer-mop)
  (:import-from :fset)
  (:export
   #:graph-class-hierarchy #:undocumented-symbols
   #:undocumented-symbols-in-package #:verbose-echo-stream))
(in-package :shcl/core/debug)

(optimization-settings)

(defun all-subclasses (class)
  "Returns an `fset:set' of all classes which subclass `class'."
  (let ((result (fset:empty-set)))
    (dolist (subclass (closer-mop:class-direct-subclasses class))
      (fset:adjoinf result subclass)
      (fset:unionf result (all-subclasses subclass)))
    result))

(defgeneric graph-class-hierarchy (class stream)
  (:documentation
   "Produce a DOT representation of is-a relationships between `class'
and all of its subclasses"))
(defmethod graph-class-hierarchy ((name symbol) stream)
  (graph-class-hierarchy (find-class name) stream))
(defmethod graph-class-hierarchy ((class standard-class) stream)
  (format stream "digraph G {~%")
  (let ((classes (fset:with (all-subclasses class) class)))
    (fset:do-set (the-class classes)
      (format stream "\"~A\" [color=gray]~%" (class-name the-class))
      (dolist (superclass (closer-mop:class-direct-superclasses the-class))
        (format stream "\"~A\" -> \"~A\"~%" (class-name the-class) (class-name superclass)))))
  (format stream "}~%"))

(defparameter *intentionally-undocumented-packages*
  (fset:set
   (find-package :shcl/shell/prompt-types)
   (find-package :shcl/core/posix-types)))

(defun documented-shcl-package-p (package)
  "Returns non-nil iff the provided package belongs to SHCL and should
be documented."
  (when (fset:lookup *intentionally-undocumented-packages* package)
    (return-from documented-shcl-package-p nil))
  (let ((shcl-prefix "SHCL/"))
    (when (> (length (package-name package)) (length shcl-prefix))
      (string= (package-name package) shcl-prefix :end1 (length shcl-prefix)))))

(defun symbol-documentation-types ()
  "Return an array of types of documentation a symbol can have.

Strictly speaking, there may be more forms of documentation than this.
However, this is a good enough approximation for our purposes: finding
undocumented symbols."
  (let* ((methods (list-iterator (closer-mop:generic-function-methods #'documentation)))
         (specializers (map-iterator methods 'closer-mop:method-specializers))
         (symbol-methods (filter-iterator specializers (lambda (x) (eq (find-class 'symbol) (first x)))))
         (eql-methods (filter-iterator symbol-methods (lambda (x) (typep (second x) 'closer-mop:eql-specializer))))
         (eql-specializers (map-iterator eql-methods 'second))
         (values (map-iterator eql-specializers 'closer-mop:eql-specializer-object)))
    (iterator-values values)))

(defparameter *symbol-documentation-types* (symbol-documentation-types)
  "An array of types of documentation a symbol can have.

See `symbol-documentation-types'.")

(defun documented-p (symbol)
  "Returns non-nil if the given symbol has some form of documentation."
  (loop :for documentation-type :across *symbol-documentation-types* :do
     (progn
       (when (documentation symbol documentation-type)
         (return-from documented-p t))))
  nil)

(defun %undocumented-symbols-in-package (package syms)
  "Store all the undocumented symbols from `package' in the adjustable
array `syms'."
  (do-external-symbols (sym package)
    (unless (documented-p sym)
      (vector-push-extend sym syms))))

(defun undocumented-symbols-in-package (package)
  "Return an array containing the undocumented symbols in `package'."
  (let ((syms (make-extensible-vector)))
    (%undocumented-symbols-in-package package syms)
    syms))

(defun undocumented-symbols (&key (package-predicate 'documented-shcl-package-p))
  "Returns an array of all shcl symbols that are undocumented."
  (let* ((all-packages (list-iterator (list-all-packages)))
         (shcl-packages (filter-iterator all-packages package-predicate))
         (syms (make-extensible-vector)))
    (do-iterator (package shcl-packages)
      (%undocumented-symbols-in-package package syms))
    syms))

(defclass verbose-echo-stream (fundamental-character-output-stream)
  ((output-stream
    :initform *standard-output*
    :initarg :output-stream
    :documentation
    "The stream where characters should be echoed."))
  (:documentation
   "This is a more verbose form of the standard echo stream.

The extra verbositty is helpful when debugging."))

(defmethod stream-write-char ((s verbose-echo-stream) char)
  (with-slots (output-stream) s
    (format output-stream "CHAR: ~W~%" char)
    char))
