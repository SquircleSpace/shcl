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

(defpackage :shcl/core/debug
  (:use
   :common-lisp :trivial-gray-streams :shcl/core/utility :shcl/core/iterator)
  (:import-from :closer-mop)
  (:import-from :fset)
  (:export
   #:graph-dependencies #:graph-class-hierarchy #:undocumented-symbols
   #:undocumented-symbols-in-package #:verbose-echo-stream
   #:traverse-dependencies #:shcl-system-name-p
   #:shcl-package-name-p))
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

(defun string-prefix-p (prefix word)
  (when (>= (length word) (length prefix))
    (string= word prefix :end1 (length prefix))))

(defun shcl-package-name-p (package-name)
  "Returns non-nil iff the given package belongs to SHCL."
  (string-prefix-p "SHCL/" package-name))

(defun shcl-system-name-p (system-name)
  "Returns non-nil iff the given ASDF system name belongs to SHCL."
  (string-prefix-p "shcl/" system-name))

(defun traverse-dependencies-fn (seed-system-name visit-fn)
  (let ((visited (make-hash-table :test 'equal)))
    (labels
        ((unpack-dependency (dep)
           (typecase dep
             (symbol
              (symbol-name peer))
             (cons
              (unless (eq :feature (car dep))
                (warn "Don't know how to deal with dependency ~W" dep)
                (return-from unpack-dependency))
              (destructuring-bind (feature-kw feature system) dep
                (declare (ignore feature-kw))
                (when (find feature *features*)
                  (unpack-dependency system))))
             (string
              dep)
             (t
              (warn "don't know how to deal with dependency ~W" dep)
              nil)))
         (visit (name)
           (when (gethash name visited)
             (return-from visit))
           (setf (gethash name visited) t)
           (let ((system (asdf:find-system name)))
             (dolist (peer (asdf/system:system-depends-on system))
               (setf peer (unpack-dependency peer))
               (when (and peer (funcall visit-fn name peer))
                 (visit peer))))))
      (visit seed-system-name))))

(defmacro traverse-dependencies (seed-system-name (depender depended) &body body)
  "Explore the ASDF dependencies of a system.

Starting with the system named `seed-system-name', this will traverse
the ASDF dependency graph.  Every time it finds a dependency, it will
bind `depender' to the name of the system that is doing the depending
and `depended' to the name of the system that is depended upon.  It
will then evaluate the provided body.

If the body returns non-nil, this macro will delve into the
dependencies of `depended'.  If the body returns nil, this macro will
not explore `depended' and will instead continue to explore
`depender'."
  `(traverse-dependencies-fn ,seed-system-name
                             (lambda (,depender ,depended) ,@body)))

(defun graph-dependencies (&key
                             (stream *standard-output*)
                             (seed-system-name "shcl/shell/main")
                             (include-predicate (constantly t))
                             (explore-predicate 'shcl-system-name-p))
  "Produce a graph of ASDF system dependencies.

This function walks the dependency tree and prints a graphviz
description of the tree.

`stream' is the output stream to write to.

`seed-system-name' is the name of the system where the traversal
should start.

`include-predicate' is a function that is given a system name and
returns non-nil if the system should be included in the graph.

`explore-predicate' is a function that is given a system name and
returns non-nil if the graph traversal should explore the dependencies
of the named system."
  (format stream "digraph G {~%")
  (traverse-dependencies seed-system-name (depender depended)
    (when (funcall include-predicate depended)
      (format stream "\"~A\" -> \"~A\"~%" depender depended))
    (funcall explore-predicate depended))
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
  (shcl-package-name-p (package-name package)))

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

(defun undocumented-symbols-in-package (package)
  "Return a set containing the undocumented symbols in `package'."
  (let ((result (fset:empty-set)))
    (do-external-symbols (sym package)
      (unless (documented-p sym)
        (fset:adjoinf result sym)))
    result))

(defun undocumented-symbols (&key (package-predicate 'documented-shcl-package-p))
  "Returns an array of all shcl symbols that are undocumented."
  (let* ((all-packages (list-iterator (list-all-packages)))
         (shcl-packages (filter-iterator all-packages package-predicate))
         (syms (fset:empty-set)))
    (do-iterator (package shcl-packages)
      (fset:unionf syms (undocumented-symbols-in-package package)))
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
