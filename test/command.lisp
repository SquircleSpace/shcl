;; Copyright 2018 Bradley Jensen
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

(defpackage :shcl/test/command
  (:use :common-lisp :prove :shcl/core/utility :shcl/core/command
        :shcl/test/foundation)
  (:import-from :fset))
(in-package :shcl/test/command)

(optimization-settings)

(defclass test-command ()
  ())

(defmethod invoke-command ((command test-command) modifier &rest rest)
  (declare (ignore rest))
  (when modifier
    modifier)
  (if (next-method-p)
      (call-next-method)
      (class-name (class-of command))))

(defclass fallback (test-command)
  ((priority
    :reader command-priority
    :initform 1000
    :allocation :class)))

(defclass value-producer (test-command)
  ((priority
    :reader command-priority
    :initform 5
    :allocation :class)
   (value
    :initarg :value
    :initform nil)))

(defmethod invoke-command ((command value-producer) modifier &rest args)
  (declare (ignore args modifier))
  (slot-value command 'value))

(define-test command-namespace
  (let ((*command-namespace* (make-command-namespace :fallback (make-instance 'fallback))))
    (is (invoke-command (lookup-command (gensym)) nil)
        'fallback)
    (is (invoke-command (lookup-command "foobar") nil)
        'fallback)
    (install-command "foobar" (make-instance 'value-producer :value "barfood"))
    (is (invoke-command (lookup-command "foobar") nil)
        "barfood"
        :test 'equal)
    (is-condition
     (install-command "foobar" (make-instance 'value-producer :value "foobar"))
     'warning
     "Redefining commands emits a warning")
    (is-condition
     (install-command "command/with/slash" (make-instance 'value-producer :value "confusing"))
     'confusing-command-name
     "Commands with slashes produce warnings")
    (handler-case
        (progn
          (install-command "other" (make-instance 'value-producer))
          (pass "Correct usage doesn't emit a warning"))
      (redefining-command (e)
        (declare (ignore e))
        (fail "Correct usage generated a warning")))))

(define-test happy-argument-parsing
  (is 123 (funcall (shell-lambda () 123) "argv0")
      "Trivial shell lambda works")

  (is "argv0" (funcall (shell-lambda (&argv0 argv0) argv0) "argv0")
      "argv0 works")

  (is '("--flag" "--flag") (funcall (shell-lambda (&flag flag) (coerce flag 'list))
                                    "argv0" "--flag" "--flag")
      "Automatic flags work")

  (is '("--no-flag" "--flag") (funcall (shell-lambda (&flag (the-flag "--flag" "--no-flag"))
                                         (coerce the-flag 'list))
                                       "argv0" "--no-flag" "--flag")
      "Manual flags work")

  (is '("val" "val2") (funcall (shell-lambda (&option flag) (coerce flag 'list))
                               "argv0" "--flag" "val" "--flag" "val2")
      "Automatic options work")

  (is '("val" "val2") (funcall (shell-lambda (&option (the-flag "--flag" "--no-flag"))
                                         (coerce the-flag 'list))
                               "argv0" "--flag" "val" "--no-flag" "val2")
      "Manual options work")

  (is "test" (funcall (shell-lambda (&required arg) arg) "argv0" "test")
      "Required arguments work")

  (is "test" (funcall (shell-lambda (&optional arg) arg) "argv0" "test")
      "Optional arguments work")

  (is '("test") (funcall (shell-lambda (&rest rest) rest) "argv0" "test")
      "rest works")

  (is '("-A" "-A" "-B")
      (funcall (shell-lambda (&flag (a "-A") (b "-B"))
                 (concatenate 'list a b))
               "argv0" "-ABA")
      "Flag packs work")

  (is '("-A" "-B" "val")
      (funcall (shell-lambda (&flag (a "-A") (b "-B") &option (c "-C"))
                 (concatenate 'list a b c))
               "argv0" "-ABC" "val")
      "Flag packs with trailing option work")

  (is '("val" "val2")
      (funcall (shell-lambda (&option (opt "-O"))
                 (coerce opt 'list))
               "argv0" "-Oval" "-Oval2")
      "Compressed options work")

  (let (optional1-tripped
        %whole %argv0 %flag1 %flag2 %opt1 %opt2 %required1 %required2 %optional1
        %optional2
        %rest)
    (labels
        ((shell-lambda-test (&rest args)
           (setf optional1-tripped nil)
           (diag (format nil "Testing~{ ~A~}" args))
           (apply (shell-lambda (&whole whole &argv0 argv0
                                        &flag flag1 (flag2 "--flag2" "--no-flag2")
                                        &option opt1 (opt2 "--opt2" "--no-opt2")
                                        &required required1 required2
                                        &optional (optional1 (setf optional1-tripped t)) optional2
                                        &rest rest)
                    (setf %whole whole)
                    (setf %argv0 argv0)
                    (setf %flag1 flag1)
                    (setf %flag2 flag2)
                    (setf %opt1 opt1)
                    (setf %opt2 opt2)
                    (setf %required1 required1)
                    (setf %required2 required2)
                    (setf %optional1 optional1)
                    (setf %optional2 optional2)
                    (setf %rest rest)
                    (unless (equal whole args)
                      (fail "whole must contains all the args"))
                    (unless (not
                             (loop :for arg :in (list argv0 required1 required2) :do
                                (unless (typep arg 'string)
                                  (return t))))
                      (fail "argv0 and required args must be strings"))
                    (unless (not
                             (loop :named outer
                                :for v :in (list flag1 flag2 opt1 opt2) :do
                                (unless (typep v 'vector)
                                  (return-from outer t))
                                (loop :for arg :across v :do
                                   (unless (typep arg 'string)
                                     (return-from outer t)))))
                      (fail "Flags and options must be vectors of strings"))
                    (unless (and (or (typep optional1 'string) (eq t optional1))
                                 (or (typep optional2 'string) (eq nil optional2)))
                        (fail "Optional args must either a string or their default value"))
                    (unless (or (not rest) (and optional1 optional2))
                      (fail "Optional args must be non-nil if there are &rest args")))
                  args)))

      (shell-lambda-test "test" "--flag1" "--flag1" "required1" "required2")
      (is %argv0 "test" :test 'equal
          "Command name is correct")
      (is (length %flag1) 2
          "flag1 was provided twice")
      (is %optional1 t
          "optional1 was not provided")
      (is %optional2 nil
          "optional2 was not provided")
      (is %rest nil
          "rest parsed correctly")
      (ok optional1-tripped
          "Optional form was evaluated")

      (shell-lambda-test "test" "--no-opt2" "arg" "--flag1" "--opt2" "arg2" "required1" "required2" "optional1")
      (is (aref %opt2 0) "arg" :test 'equal
          "First opt2 arg parsed correctly")
      (is (aref %opt2 1) "arg2" :test 'equal
          "Second opt2 arg parsed correctly")
      (is (aref %flag1 0) "--flag1" :test 'equal
          "flag1 was parsed correctly")
      (is %required1 "required1" :test 'equal
          "required1 parsed correctly")
      (is %optional1 "optional1" :test 'equal
          "optional1 parsed correctly")
      (ok (not optional1-tripped)
          "Optional form was not evaluated")
      (is %optional2 nil
          "optional2 was not provided")

      (shell-lambda-test "test" "--opt1" "--" "--" "--flag1" "required2" "optional1" "optional2" "rest1" "rest2")
      (is (aref %opt1 0) "--" :test 'equal
          "opt1 parsed correctly")
      (ok (zerop (length %flag1))
          "flag1 was not provied")
      (is %required1 "--flag1" :test 'equal
          "required1 parsed correctly")
      (is %rest '("rest1" "rest2") :test 'equal
          "rest parsed correctly")

      (shell-lambda-test "test" "--flag2" "--no-flag2" "req1" "req2")
      (is (aref %flag2 0) "--flag2" :test 'equal
          "First flag2 argument is correct")
      (is (aref %flag2 1) "--no-flag2" :test 'equal
          "Second flag2 argument is correct"))))

(define-test sad-argument-parsing
  (is-error
   (funcall (shell-lambda ())) 'error
   "Not providing argv0 is an error")

  (is-error
   (funcall (shell-lambda ()) "argv0" "next") 'error
   "Providing too many arguments is an error")

  (is-error
   (funcall (shell-lambda (&required arg) arg) "argv0") 'error
   "Not providing a required argument is an error")

  (is-error
   (funcall (shell-lambda (&option opt) opt) "argv0" "--opt") 'error
   "Not providing a value for an option is an error")

  (is-error
   (funcall (shell-lambda (&flag (a "-A") &optional arg)
              (list a arg))
            "argv0" "-AB")
   'error
   "Unrecognized flags in a flag pack is an error")

  (is-error
   (funcall (shell-lambda (&flag (a "-A") &option (b "-B"))
              (list a b))
            "argv0" "-AB")
   'error
   "Flag pack with trailing option without a value is an error")

  (is-error
   (funcall (shell-lambda (&option (b "-B"))
              b)
            "argv0" "-B")
   'error
   "Single-letter options still require arguments")

  (is-error
   (eval '(shell-lambda (arg))) 'error
   "Lambda lists must begin with an & arg")

  (is-error
   (eval '(shell-lambda (&flag -))) 'error
   "- is not a valid flag option")

  (is-error
   (eval '(shell-lambda (&flag (arg "-")))) 'error
   "- is not a valid flag option")

  (is-error
   (eval '(shell-lambda (&flag --))) 'error
   "-- is not a valid flag option")

  (is-error
   (eval '(shell-lambda (&flag (arg "--")))) 'error
   "-- is not a valid flag option")

  (is-error
   (eval '(shell-lambda (&whole arg1 arg2))) 'error
   "Too many wholes is an error")

  (is-error
   (eval '(shell-lambda (&rest arg1 arg2))) 'error
   "Too many rests is an error")

  (is-error
   (eval '(shell-lambda (&argv0 arg1 arg2))) 'error
   "Too many argv0s is an error")

  (is-error
   (eval '(shell-lambda (&flag flag &option flag))) 'error
   "Duplicate flags is an error")

  (is-error
   (eval '(shell-lambda (&flag (flag "--A" "--A")))) 'error
   "Duplicate flags is an error"))

(define-test argument-parsing-declarations
  (funcall (shell-lambda (&whole whole)
             (declare (ignore whole))
             (ok (not (boundp 'whole))
                 "Args are bound lexically"))
           "argv0")

  (let ((args '("argv0" "--flag" "--opt" "optval" "req" "opt2" "rest")))
    (apply (shell-lambda (&whole whole &argv0 argv0 &flag flag &option opt
                                 &required req &optional opt2 &rest rest-args)
             (declare (special whole argv0 flag opt req opt2 rest-args))
             (ok (and (equal args (symbol-value 'whole))
                      (equal "argv0" (symbol-value 'argv0))
                      (equal '("--flag") (coerce (symbol-value 'flag) 'list))
                      (equal '("optval") (coerce (symbol-value 'opt) 'list))
                      (equal "req" (symbol-value 'req))
                      (equal "opt2" (symbol-value 'opt2))
                      (equal '("rest") (symbol-value 'rest-args)))
                 "Special declaration works"))
           args)))
