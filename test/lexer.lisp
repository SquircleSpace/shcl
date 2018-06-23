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

(defpackage :shcl/test/lexer
  (:use :common-lisp :prove :shcl/core/utility :shcl/core/lexer
        :shcl/core/shell-readtable :shcl/test/foundation)
  (:import-from :shcl/core/data #:define-data))
(in-package :shcl/test/lexer)

(optimization-settings)

(defun lexes-to-token-types (string &rest tokens)
  (let ((real-tokens (tokenize string)))
    (unless (equal (length real-tokens) (length tokens))
      (return-from lexes-to-token-types nil))
    (loop :for token :across real-tokens
     :for class :in tokens :do
     (unless (typep token class)
       (return-from lexes-to-token-types nil))))
  t)

(define-test basics
  (ok (lexes-to-token-types "foobar" 'simple-word))
  (ok (lexes-to-token-types "foo$(bar)" 'compound-word))
  (ok (lexes-to-token-types "FOO=bar" 'assignment-word))
  (ok (lexes-to-token-types "FOO=\"asdf\"qwer$(pwd)" 'assignment-word))
  (ok (lexes-to-token-types "valid_name43aNd_more" 'name))
  (ok (lexes-to-token-types "3>" 'io-number 'great))
  (ok (lexes-to-token-types "3 >" 'simple-word 'great))
  (ok (lexes-to-token-types ">>" 'dgreat))
  (ok (lexes-to-token-types "&&" 'and-if))
  (ok (lexes-to-token-types "&&" 'literal-token))
  (ok (lexes-to-token-types (string #\linefeed) 'newline))
  (ok (lexes-to-token-types "if" 'reserved-word))
  (ok (lexes-to-token-types "if" 'if-word))
  (ok (lexes-to-token-types "'single quote'" 'single-quote))
  (ok (lexes-to-token-types "\\q" 'single-quote))
  (ok (lexes-to-token-types "\"double quotes $variable  escaped quote \\\"  end\"" 'double-quote))
  (ok (lexes-to-token-types "$(sub command word $variable)" 'command-word))
  (ok (lexes-to-token-types "$variable" 'variable-expansion-word))
  (ok (lexes-to-token-types "${FOO}" 'variable-expansion-word))
  (ok (lexes-to-token-types "${#FOO}" 'variable-expansion-length-word))
  (ok (lexes-to-token-types "$1" 'variable-expansion-word))
  (ok (lexes-to-token-types "some words # and the rest" 'simple-word 'simple-word)))

(define-test word-boundaries
  (ok (lexes-to-token-types (format nil "spaces    seperate  ~C   words  " #\tab) 'simple-word 'simple-word 'simple-word))
  (ok (lexes-to-token-types ">new-word" 'great 'simple-word))
  (ok (lexes-to-token-types "word>" 'simple-word 'great))
  (ok (lexes-to-token-types (format nil "first~%second") 'simple-word 'newline 'simple-word))
  (ok (lexes-to-token-types (format nil "part\\~%part") 'simple-word)))

(define-data form-token (token)
  ((form
   :initarg :form
   :reader form-token-form)))

(defun make-form (value)
  (make-instance 'form-token :form value))

(define-test extensible-reading
  (let* ((readtable +standard-shell-readtable+)
         (stream (make-string-input-stream "[(+ 1 2 3)#,\"asdf\"#.stuff"))
         hash-hit
         s-reader-ran)
    (labels
        (([-reader (s i c)
           (declare (ignore i))
           (shell-lexer-context-add-part c (make-form (read s))))
         (comma-reader (s i c)
           (declare (ignore i))
           (shell-lexer-context-add-part c (make-form (read s))))
         (default-s-reader (s i c)
           (declare (ignore s i))
           (setf s-reader-ran t)
           (shell-lexer-context-add-part c 's))
         (hash-reader (s i c)
           (declare (ignore s i c))
           (setf hash-hit t))
         (r ()
           (let ((c (make-instance 'shell-lexer-context :stream stream :readtable readtable)))
             (lexer-context-shell-extensible-read c)
             (shell-lexer-context-delimit c :if-empty nil))))
      ;; Simple reader
      (setf readtable (with-handler readtable "[" #'[-reader))
      (is
       '(+ 1 2 3)
       (form-token-form (r))
       :test #'equal)

      ;; Dispatch reader
      (setf readtable (with-dispatch-character readtable "#"))
      (setf readtable (with-default-handler readtable "#" #'hash-reader))
      (setf readtable (with-handler readtable "#," #'comma-reader))
      ;; two-character sequence
      (is
       "asdf"
       (form-token-form (r))
       :test #'equal)

      ;; Dispatch char fallback (and commenting)
      (ok (not hash-hit))
      (r)
      (ok hash-hit)

      ;; Ultimate fallback (No matches at all)
      (is
       nil
       (r))

      (is (read-char stream nil :eof) #\.
          :test #'equal)

      ;; Dispatch char fallback (normal case)
      (setf readtable (with-dispatch-character readtable "s"))
      (setf readtable (with-default-handler readtable "s" #'default-s-reader))
      (is
       's
       (r)
       :test #'equal)
      (ok s-reader-ran))))
