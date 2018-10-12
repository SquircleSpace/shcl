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

(defpackage :shcl/core/shell-grammar
  (:use
   :common-lisp :shcl/core/parser :shcl/core/lexer :shcl/core/utility
   :shcl/core/iterator)
  (:import-from :shcl/core/advice #:define-advice #:define-advisable)
  (:export
   #:command-iterator #:*intermediate-parse-error-hook*
   ;; nonterminals
   #:complete-command #:command-list #:and-or #:and-or-tail :pipeline
   #:pipe-sequence #:pipe-sequence-tail #:command
   #:compound-command :subshell #:compound-list #:term
   #:term-sequence #:for-clause #:for-clause-range #:name-nt
   #:in-nt :words #:case-clause #:case-list-ns
   #:case-list :case-list-tail #:case-item-ns #:case-item #:pattern
   #:pattern-tail :if-clause #:else-part #:while-clause
   #:until-clause #:function-definition :function-body #:fname
   #:brace-group #:do-group #:simple-command
   #:cmd-prefix #:cmd-prefix-tail #:cmd-suffix
   #:cmd-suffix-tail :redirect-list #:redirect-list-tail
   #:io-redirect #:io-file #:filename :io-source #:io-here #:here-end
   #:newline-list :linebreak #:separator-op #:separator
   #:sequential-sep :redirect #:fd-description
   #:condition #:body

   #:parse-simple-command #:parse-simple-command-word))
(in-package :shcl/core/shell-grammar)

(optimization-settings)

(defmacro define-terminals (&body terminals)
  (apply 'progn-concatenate
         (loop :for terminal :in terminals :collect
            (etypecase terminal
              (symbol
               `(define-terminal ,terminal))
              (cons
               `(define-terminal ,@terminal))))))

(define-terminals
  token a-word simple-word assignment-word name newline io-number and-if
  or-if dsemi dless dgreat lessand greatand lessgreat dlessdash
  clobber if-word then else elif fi do-word done case-word esac while until
  for lbrace rbrace bang in semi par pipe lparen rparen great less)

(define-hook *intermediate-parse-error-hook*
    "This hook is run when a parse error is detected in a nonterminal of
the shell grammar.

Each function will receive one argument: the error produced.  Note
that these errors do not represent fatal parse errors.  They simply
represent times when the parser chose a bad branch.")

(defun error-hook (error)
  (run-hook '*intermediate-parse-error-hook* error))

(define-hook *intermediate-parse-value-hook*)

(defun value-hook (value)
  (run-hook '*intermediate-parse-value-hook* value))

(defmacro hooked (&body body)
  `(parser-bind (value error-p) (progn ,@body)
                (cond
                  (error-p
                   (error-hook value)
                   (parser-error value))
                  (t
                   (value-hook value)
                   (parser-value value)))))

(defmacro hooked-parser-choice (iter &body choices)
  (if choices
      `(parser-choice ,iter
        ,@(mapcar (lambda (choice) `(hooked ,choice)) choices))
      `(hooked (parser-choice ,iter))))

(defmacro define-hooked-nonterminal (name-and-options &body clauses)
  (when (symbolp name-and-options)
    (setf name-and-options (list name-and-options)))
  (setf name-and-options
        `(,@name-and-options :parser-choice hooked-parser-choice))
  `(define-nonterminal ,name-and-options ,@clauses))

(define-hooked-nonterminal start
  parse-eof
  parse-complete-command)

(define-hooked-nonterminal complete-command
  (newline-list &optional complete-command)
  (command-list command-end))

(define-hooked-nonterminal command-end
  parse-eof
  parse-newline)

(define-hooked-nonterminal command-list
  (and-or &optional separator-op command-list))

(define-hooked-nonterminal and-or
  (pipeline and-or-tail))

(define-hooked-nonterminal and-or-tail
  (and-if linebreak pipeline and-or-tail)
  (or-if linebreak pipeline and-or-tail)
  ())

(define-hooked-nonterminal pipeline
  (bang pipe-sequence)
  parse-pipe-sequence)

(define-hooked-nonterminal pipe-sequence
  (command pipe-sequence-tail))

(define-hooked-nonterminal pipe-sequence-tail
  (pipe linebreak command pipe-sequence-tail)
  ())

(define-hooked-nonterminal command
  (compound-command &optional redirect-list)
  parse-function-definition
  parse-simple-command)

(define-hooked-nonterminal compound-command
  parse-brace-group
  parse-subshell
  parse-for-clause
  parse-case-clause
  parse-if-clause
  parse-while-clause
  parse-until-clause)

(define-hooked-nonterminal subshell
  (lparen linebreak (term-sequence (lambda (i) (parse-term-sequence i :stop-parser #'parse-rparen :final-separator-optional-p t))) rparen))

(define-nonterminal-class compound-list ()
  (linebreak
   term-sequence))

(define-advisable parse-compound-list (iter &key stop-parser)
  (hooked
   (parser-block
    (parser-value
     (make-instance
      'compound-list
      'linebreak (parse (parse-linebreak iter))
      'term-sequence (parse (parse-term-sequence iter :stop-parser stop-parser)))))))

(defun compound-list-ending-with (ending-parser)
  (lambda (iter)
    (parse-compound-list iter :stop-parser ending-parser)))

(define-nonterminal-class term ()
  (and-or
   separator))

(define-advisable parse-term-sequence (iter &key stop-parser final-separator-optional-p)
  (when (typep stop-parser 'function)
    (let ((old-stop-parser stop-parser))
      (setf stop-parser (lambda (iter)
                          (parser-lookahead iter
                            (hooked (funcall old-stop-parser iter)))))))
  (hooked
    (parser-repeat (1 stop-parser iter)
      (hooked
        (parser-let
            ((and-or (parse-and-or iter))
             (separator (if (or (null stop-parser)
                                (not final-separator-optional-p))
                            (parse-separator iter)
                            (parser-if iter (hooked (funcall stop-parser iter))
                                       (parser-value nil)
                                       (parse-separator iter)))))
          (parser-value
           (make-instance
            'term
            'and-or and-or
            'separator separator)))))))

(define-hooked-nonterminal for-clause
  (for name-nt linebreak for-clause-range (body #'parse-do-group)))

(define-nonterminal-class for-clause-range ()
    (in-nt
     words
     sequential-sep))

(define-advisable parse-for-clause-range (iter)
  (parser-choice iter
    (parser-block
      (parser-value
       (make-instance 'for-clause-range
                      'in-nt (parse (parse-in-nt iter))
                      'words (parser-repeat (0 nil iter)
                               (parse-a-word iter))
                      'sequential-sep (parse-sequential-sep iter))))
    (parser-value nil)))

(define-hooked-nonterminal name-nt
  (name)) ;; Apply rule 5 (need not be reflected in the grammar)

(define-hooked-nonterminal in-nt
  (in)) ;; Apply rule 6 (need not be reflected in the grammar)

(define-hooked-nonterminal case-clause
  (case-word a-word linebreak in-nt linebreak case-list esac)
  (case-word a-word linebreak in-nt linebreak case-list-ns esac)
  (case-word a-word linebreak in-nt linebreak esac))

(define-hooked-nonterminal case-list-ns
  (case-list case-item-ns)
  (case-item-ns))

(define-hooked-nonterminal case-list
  (case-item case-list-tail))

(define-hooked-nonterminal case-list-tail
  (case-item case-list-tail)
  ())

(define-hooked-nonterminal case-item-ns
  (pattern rparen linebreak)
  (pattern rparen (compound-list (compound-list-ending-with #'parse-linebreak)) linebreak)
  (lparen pattern rparen linebreak)
  (lparen pattern rparen (compound-list (compound-list-ending-with #'parse-linebreak)) linebreak))

(define-hooked-nonterminal case-item
  (pattern rparen linebreak dsemi linebreak)
  (pattern rparen (compound-list (compound-list-ending-with #'parse-dsemi)) dsemi linebreak)
  (lparen pattern rparen linebreak dsemi linebreak)
  (lparen pattern rparen (compound-list (compound-list-ending-with #'parse-dsemi)) dsemi linebreak))

(define-hooked-nonterminal pattern
  (a-word pattern-tail)) ;; Apply rule 4 (must be reflected in grammar)

(define-hooked-nonterminal pattern-tail
  (pipe a-word pattern-tail) ;; Do not apply rule 4 (but /bin/sh seems to?)
  ())

(define-hooked-nonterminal else-part-lead
  parse-elif
  parse-else
  parse-fi)

(define-hooked-nonterminal if-clause
  (if-word (condition (compound-list-ending-with #'parse-then)) then (body (compound-list-ending-with #'parse-else-part-lead)) else-part))

(define-hooked-nonterminal else-part
  (elif (condition (compound-list-ending-with #'parse-then)) then (body (compound-list-ending-with #'parse-else-part-lead)) else-part)
  (else (body (compound-list-ending-with #'parse-fi)) fi)
  parse-fi)

(define-hooked-nonterminal while-clause
  (while (condition (compound-list-ending-with #'parse-do-word)) (body #'parse-do-group)))

(define-hooked-nonterminal until-clause
  (until (condition (compound-list-ending-with #'parse-do-word)) (body #'parse-do-group)))

(define-hooked-nonterminal function-definition
  (fname linebreak function-body))

(define-hooked-nonterminal function-body
  (compound-command redirect-list) ;; Apply rule 9 (need not be reflected in the grammar)
  (compound-command)) ;; Apply rule 9 (need not be reflected in the grammar)

(define-nonterminal-class fname ()
    (name
     lparen
     rparen))

(define-advisable parse-fname (iter)
  (parser-let
      ((name-lparen
        (parser-try iter
          (parser-let
              ((name (parse-name iter))
               (lparen (parse-lparen iter)))
            (parser-value (cons name lparen)))))
       (rparen (parse-rparen iter)))
    (parser-value
     (make-instance 'fname
                    'name (car name-lparen)
                    'lparen (cdr name-lparen)
                    'rparen rparen))))

(define-hooked-nonterminal brace-group
  (lbrace (compound-list (compound-list-ending-with #'parse-rbrace)) rbrace))

(define-hooked-nonterminal do-group
  (do-word (compound-list (compound-list-ending-with #'parse-done)) done)) ;; Apply rule 6 (need not be reflected in the grammar)

(define-advisable parse-simple-command-word (iter)
  (parse-a-word iter))

(define-hooked-nonterminal simple-command
  (cmd-prefix &optional (a-word #'parse-simple-command-word) cmd-suffix)
  ((a-word #'parse-simple-command-word) &optional cmd-suffix))

(define-hooked-nonterminal cmd-prefix
  (io-redirect cmd-prefix-tail)
  (assignment-word cmd-prefix-tail))

(define-hooked-nonterminal cmd-prefix-tail
  (io-redirect cmd-prefix-tail)
  (assignment-word cmd-prefix-tail)
  ())

(define-hooked-nonterminal cmd-suffix
  (io-redirect cmd-suffix-tail)
  ((a-word #'parse-simple-command-word) cmd-suffix-tail))

(define-hooked-nonterminal cmd-suffix-tail
  (io-redirect cmd-suffix-tail)
  ((a-word #'parse-simple-command-word) cmd-suffix-tail)
  ())

(define-hooked-nonterminal redirect-list
  (io-redirect redirect-list-tail))

(define-hooked-nonterminal redirect-list-tail
  (io-redirect redirect-list-tail)
  ())

(define-nonterminal-class io-redirect ()
    (io-number
     io-source))

(define-advisable parse-io-redirect (iter)
  (parser-choice iter
    (parse-io-file iter)
    (parse-io-here iter)
    (parser-block
      (parser-value
       (make-instance 'io-redirect
                      'io-number (parse (parse-io-number iter))
                      'io-source (parse
                                  (parser-choice iter
                                    (parse-io-file iter)
                                    (parse-io-here iter))))))))

(define-hooked-nonterminal io-file
  ((redirect #'parse-less) filename)
  ((redirect #'parse-lessand) (fd-description #'parse-simple-word))
  ((redirect #'parse-great) filename)
  ((redirect #'parse-greatand) (fd-description #'parse-simple-word))
  ((redirect #'parse-dgreat) filename)
  ((redirect #'parse-lessgreat) filename)
  ((redirect #'parse-clobber) filename))

(define-hooked-nonterminal filename
  parse-a-word) ;; Apply rule 2 (need not be reflected in grammar)

(define-hooked-nonterminal io-here
  (dless here-end)
  (dlessdash here-end))

(define-hooked-nonterminal here-end
  (a-word)) ;; Apply rule 3 (need not be reflected in grammar)

(define-hooked-nonterminal newline-list
  (newline &optional newline-list))

(define-hooked-nonterminal linebreak
  (newline-list)
  ())

(define-hooked-nonterminal separator-op
  parse-par
  parse-semi)

(define-hooked-nonterminal separator
  (separator-op linebreak)
  (newline-list))

(define-hooked-nonterminal sequential-sep
  (semi linebreak)
  (newline-list))

(defun command-iterator (token-iterator)
  "Given a `forkable-wrapper-iterator' that produces tokens, return an
iterator that produces shell syntax tree objects."
  (let ((iter (syntax-iterator #'parse-start token-iterator)))
    (make-computed-iterator
      (do-iterator (value iter)
        (when (eq value :eof)
          (stop))
        (emit value))
      (stop))))

(defgeneric parse-shell (source))

(defmethod parse-shell ((s string))
  (parse-shell (make-string-input-stream s)))

(defmethod parse-shell ((s stream))
  (parse-shell (forkable-wrapper-iterator (token-iterator s))))

(defmethod parse-shell ((iter forkable-wrapper-iterator))
  (next (command-iterator iter)))
