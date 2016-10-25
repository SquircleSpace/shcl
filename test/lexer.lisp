(defpackage :shcl-test/lexer
  (:use :common-lisp :prove :shcl/lexer))
(in-package :shcl-test/lexer)

(defun lexes-to-token-types (string &rest tokens)
  (let ((real-tokens (tokenize string)))
    (unless (equal (length real-tokens) (length tokens))
      (return-from lexes-to-token-types nil))
    (loop :for token :across real-tokens
     :for class :in tokens :do
     (unless (typep token class)
       (return-from lexes-to-token-types nil))))
  t)

(deftest basics
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
  (ok (lexes-to-token-types "$1" 'variable-expansion-word))
  (ok (lexes-to-token-types "some words # and the rest" 'simple-word 'simple-word))
  (ok (lexes-to-token-types "`sub command`" 'command-word)))

(deftest word-boundaries
  (ok (lexes-to-token-types (format nil "spaces    seperate  ~C   words  " #\tab) 'simple-word 'simple-word 'simple-word))
  (ok (lexes-to-token-types ">new-word" 'great 'simple-word))
  (ok (lexes-to-token-types "word>" 'simple-word 'great))
  (ok (lexes-to-token-types (format nil "first~%second") 'simple-word 'newline 'simple-word))
  (ok (lexes-to-token-types (format nil "part\\~%part") 'simple-word)))

(defclass form-token (token)
  ((form
   :initarg :form
   :reader form-token-form)))

(defun make-form (value)
  (make-instance 'form-token :form value))

(deftest extensible-reading
  (let* ((*shell-readtable* *shell-readtable*)
         (stream (make-string-input-stream "[(+ 1 2 3)#,\"asdf\"#.stuff"))
         s-reader-ran)
    (reset-shell-readtable)
    (labels
        (([-reader (s i c)
           (declare (ignore i c))
           (make-form (read s)))
         (comma-reader (s i c)
           (declare (ignore i c))
           (make-form (read s)))
         (default-s-reader (s i c)
           (declare (ignore s i c))
           (setf s-reader-ran t)
           "s"))
      ;; Simple reader
      (set-character-handler #\[ #'[-reader)
      (is
       '(+ 1 2 3)
       (form-token-form (shell-extensible-read stream))
       :test #'equal)

      ;; Dispatch reader
      (make-shell-dispatch-character #\# :default-handler (constantly t))
      (set-shell-dispatch-character #\# #\, #'comma-reader)
      ;; two-character sequence
      (is
       "asdf"
       (form-token-form (shell-extensible-read stream))
       :test #'equal)

      ;; Dispatch char fallback (and commenting)
      (is
       t
       (shell-extensible-read stream))

      ;; Ultimate fallback (No matches at all)
      (is
       nil
       (shell-extensible-read stream))

      (is (read-char stream nil :eof) #\.
          :test #'equal)

      ;; Dispatch char fallback (normal case)
      (make-shell-dispatch-character #\s :default-handler #'default-s-reader)
      (is
       "s"
       (shell-extensible-read stream)
       :test #'equal)
      (ok s-reader-ran))))
