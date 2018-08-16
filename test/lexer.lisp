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
        :shcl/core/dispatch-table :shcl/test/foundation)
  (:import-from :shcl/core/data #:define-data))
(in-package :shcl/test/lexer)

(optimization-settings)

(link-package-to-system :shcl/core/lexer)

(defun lexes-to-token-types (string &rest tokens)
  (let ((real-tokens (tokens-in-string string)))
    (unless (equal (length real-tokens) (length tokens))
      (return-from lexes-to-token-types nil))
    (loop :for token :across real-tokens
     :for class :in tokens :do
     (unless (typep token class)
       (return-from lexes-to-token-types nil))))
  t)

(define-test basics
  (ok (lexes-to-token-types "foobar" 'simple-word)
      "Simple words lex")
  (ok (lexes-to-token-types "foo$(bar)" 'compound-word)
      "Trivial compound words lex")
  (ok (lexes-to-token-types "FOO=bar" 'assignment-word)
      "Assignment words lex")
  (ok (lexes-to-token-types "FOO=\"asdf\"qwer$(pwd)" 'assignment-word)
      "Complex assignment words lex")
  (ok (lexes-to-token-types "valid_name43aNd_more" 'name)
      "A complex name lexes")
  (ok (lexes-to-token-types "3>" 'io-number 'great)
      "Basic io-number case lexes")
  (ok (lexes-to-token-types "3 >" 'simple-word 'great)
      "Io numbers must be immediately adjacent to the redirect")
  (ok (lexes-to-token-types ">>" 'dgreat)
      "Operators with multiple characters lex")
  (ok (lexes-to-token-types "&&" 'and-if)
      "&& lexes")
  (ok (lexes-to-token-types "&&" 'literal-token)
      "&& is a literal token")
  (ok (lexes-to-token-types (string #\linefeed) 'newline)
      "Newline lexes")
  (ok (lexes-to-token-types "if" 'reserved-word)
      "if is a reserved word")
  (ok (lexes-to-token-types "if" 'if-word)
      "if lexes")
  (ok (lexes-to-token-types "'single quote'" 'single-quote)
      "Single quote lexes")
  (ok (lexes-to-token-types "\\q" 'single-quote)
      "Backslash lexes")
  (ok (lexes-to-token-types "\"double quotes $variable  escaped quote \\\"  end\"" 'double-quote)
      "Complex double quote lexes")
  (ok (lexes-to-token-types "\"\"" 'double-quote)
      "Empty double quote lexes")
  (ok (lexes-to-token-types "$(sub command word $variable)" 'command-word)
      "Command word lexes")
  (ok (lexes-to-token-types "$variable" 'variable-expansion-word)
      "Basic variable expansion lexes")
  (ok (lexes-to-token-types "${FOO}" 'variable-expansion-word)
      "Basic curly variable expansion lexes")
  (ok (lexes-to-token-types "${#FOO}" 'variable-expansion-word)
      "Basic variable length lexes")
  (ok (lexes-to-token-types "$1" 'variable-expansion-word)
      "Argument access lexes")
  (ok (lexes-to-token-types "$11" 'compound-word)
      "Only the first digit following $ is used for variable access")
  (ok (lexes-to-token-types "${11}" 'variable-expansion-word)
      "Multiple digits of variable access")
  (ok (lexes-to-token-types "some words # and the rest" 'simple-word 'simple-word)
      "Comments do not result in tokens"))

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

(define-data s-token (token)
  ())

(defun make-form (value)
  (make-instance 'form-token :form value))

(define-test extensible-reading
  (let* ((readtable (standard-shell-readtable))
         (stream (make-string-input-stream "[(+ 1 2 3)#,\"asdf\"#.stuff"))
         hash-hit
         s-reader-ran)
    (labels
        (([-reader (s i)
           (declare (ignore i))
           (make-form (read s)))
         (comma-reader (s i)
           (declare (ignore i))
           (make-form (read s)))
         (default-s-reader (s i)
           (declare (ignore s i))
           (setf s-reader-ran t)
           (make-instance 's-token))
         (hash-reader (s i)
           (declare (ignore s i))
           (setf hash-hit t)
           nil)
         (r ()
           (let ((c (make-instance 'shcl/core/lexer::shell-lexer-context :stream stream :readtable readtable)))
             (shcl/core/lexer::handle-extensible-syntax c)
             (shcl/core/lexer::shell-lexer-context-delimit c :if-empty nil))))
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
      (is-type
       (r)
       's-token)
      (ok s-reader-ran))))
