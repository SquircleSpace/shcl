(defpackage :shcl/test/data
  (:use :common-lisp :prove :shcl/core/utility :shcl/core/data
        :shcl/test/foundation)
  (:import-from :fset))
(in-package :shcl/test/data)

(optimization-settings)

(define-data base ()
  ((a
    :initarg :a
    :initform nil
    :updater base-a)))

(define-data derived (base)
  ((b
    :initarg :b
    :initform nil
    :updater derived-b)))

(define-data derived-b (base)
  ((c
    :initarg :c
    :initform nil
    :updater derived-c)))

(defclass vanilla ()
  ((a
    :initarg :a
    :initform nil)))

(define-test basics
  (let* ((base (make-instance 'base :a 123))
         (derived (make-instance 'derived :a 456 :b 789))
         (cons (cons base derived)))
    (ok (equal 123 (base-a base))
        "Readers work")
    (ok (equal 456 (base-a derived))
        "Readers work on subclasses")

    (let ((old-base base))
      (setf (base-a base) 'a)
      (ok (not (eq base old-base))
          "Post update, new value is not eq to old value")
      (ok (not (eql (base-a base) (base-a old-base)))
          "Post update, contained value is different")
      (setf base old-base))

    (let ((old-base base)
          (old-cons cons))
      (setf (base-a (car cons)) 'a)
      (ok (eq old-cons cons)
          "Non-data intermediate places are not cloned")
      (ok (and (eq old-base base)
               (equal (base-a old-base) 123)
               (not (eq old-base (car cons))))
          "All existing pointers to data are unaffected by an update")
      (setf (car cons) old-base))

    (let ((old-base base)
          (base base))
      (setf (base-a base) base)
      (ok (and (not (eq base old-base))
               (eq (base-a base) old-base)
               (equal (base-a old-base) 123))
          "Circularity isn't confusing"))))

(define-test inheritance
  (is-error (eval '(define-data bad-data (vanilla) ())) 'error
            "Inheriting from a normal class is an error")
  (is-error (eval '(defclass bad-class (base) ())) 'error
            "Inheriting from a data class in a normal class is an error"))

(define-data numbers ()
  ((a
    :initarg :a
    :initform 0
    :updater numbers-a)
   (b
    :initarg :b
    :initform 0
    :updater numbers-b)))

(define-data numbers-c (numbers)
  ((c
    :initarg :c
    :initform 0
    :updater numbers-c)))

(define-data numbers-d (numbers)
  ((d
    :initarg :d
    :initform 0
    :updater numbers-d)))

(define-test ordering
  (let ((a (make-instance 'numbers :a 123 :b 123))
        (b (make-instance 'numbers :a 123 :b 123)))
    (is (fset:compare a b) :equal
        "Non-eq data classes are equal")
    (setf (numbers-a a) 122)
    (is (fset:compare a b) :less
        "Compare works for first slot differences")
    (setf (numbers-a a) 123)
    (setf (numbers-b a) 124)
    (is (fset:compare a b) :greater
        "Compare works for second slot differences")
    (setf (numbers-b a) 123)

    (setf b (make-instance 'numbers-c :a 123 :b 123 :c 123))
    (is (fset:compare a b) :less
        "Base classes are less than derived classes")
    (setf (numbers-a a) 124)
    (is (fset:compare a b) :greater
        "Base classes can be greater than derived classes")
    (setf (numbers-a a) 122)
    (is (fset:compare a b) :less
        "Derived classes can be greater than base classes")

    (setf a (make-instance 'numbers-d :a 123 :b 123 :d 0))
    (setf b (make-instance 'numbers-c :a 123 :b 123 :c 1))
    (is (fset:compare a b) :unequal
        "Different derived classes are unequal")
    (setf (numbers-a a) 124)
    (is (fset:compare a b) :greater
        "Different direved clases can be ordered")))
