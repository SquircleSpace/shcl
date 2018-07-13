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
  (:import-from :shcl/core/advice #:define-advice)
  (:export
   #:command-iterator
   ;; nonterminals
   #:complete-command #:command-list #:command-list-tail #:and-or #:and-or-tail
   #:pipeline #:pipe-sequence #:pipe-sequence-tail #:command #:compound-command
   #:subshell #:compound-list #:term #:term-tail #:for-clause #:name-nt #:in-nt
   #:wordlist #:wordlist-tail #:case-clause #:case-list-ns #:case-list
   #:case-list-tail #:case-item-ns #:case-item #:pattern #:pattern-tail
   #:if-clause #:else-part #:while-clause #:until-clause #:function-definition
   #:function-body #:fname #:brace-group #:do-group #:simple-command #:cmd-name
   #:cmd-word #:cmd-prefix #:cmd-prefix-tail #:cmd-suffix #:cmd-suffix-tail
   #:redirect-list #:redirect-list-tail #:io-redirect #:io-file #:filename
   #:io-source #:io-here #:here-end #:newline-list #:newline-list-tail
   #:linebreak #:separator-op #:separator #:command-separator #:sequential-sep
   #:wordly-word #:redirect #:fd-description #:condition #:body))
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

(define-nonterminal start
  parse-eof
  parse-complete-command)

(define-nonterminal complete-command
  (newline-list complete-command)
  (newline-list)
  (command-list command-end))

(define-nonterminal command-end
  parse-eof
  parse-newline)

(define-nonterminal command-list
  (and-or separator-op command-list-tail)
  parse-and-or)

(define-nonterminal command-list-tail
  (and-or separator-op command-list-tail)
  parse-and-or
  ())

(define-nonterminal and-or
  (pipeline and-or-tail))

(define-nonterminal and-or-tail
  (and-if linebreak pipeline and-or-tail)
  (or-if linebreak pipeline and-or-tail)
  ())

(define-nonterminal pipeline
  (bang pipe-sequence)
  parse-pipe-sequence)

(define-nonterminal pipe-sequence
  (command pipe-sequence-tail))

(define-nonterminal pipe-sequence-tail
  (pipe linebreak command pipe-sequence-tail)
  ())

(define-nonterminal command
  (compound-command redirect-list)
  parse-compound-command
  parse-function-definition
  parse-simple-command)

(define-nonterminal compound-command
  parse-brace-group
  parse-subshell
  parse-for-clause
  parse-case-clause
  parse-if-clause
  parse-while-clause
  parse-until-clause)

(define-nonterminal subshell
  (lparen compound-list rparen))

(define-nonterminal compound-list
  parse-term
  (newline-list term))

(define-nonterminal term
  (and-or separator term-tail)
  parse-and-or)

(define-nonterminal term-tail
  (and-or separator term-tail)
  parse-and-or
  ())

(define-nonterminal for-clause
  (for name-nt linebreak (body parse-do-group))
  (for name-nt linebreak in-nt sequential-sep (body parse-do-group))
  (for name-nt linebreak in-nt wordlist sequential-sep (body parse-do-group)))

(define-nonterminal name-nt
  (name)) ;; Apply rule 5 (need not be reflected in the grammar)

(define-nonterminal in-nt
  (in)) ;; Apply rule 6 (need not be reflected in the grammar)

(define-nonterminal wordlist
  (a-word wordlist-tail))

(define-nonterminal wordlist-tail
  (a-word wordlist-tail)
  ())

(define-nonterminal case-clause
  (case-word a-word linebreak in-nt linebreak case-list esac)
  (case-word a-word linebreak in-nt linebreak case-list-ns esac)
  (case-word a-word linebreak in-nt linebreak esac))

(define-nonterminal case-list-ns
  (case-list case-item-ns)
  (case-item-ns))

(define-nonterminal case-list
  (case-item case-list-tail))

(define-nonterminal case-list-tail
  (case-item case-list-tail)
  ())

(define-nonterminal case-item-ns
  (pattern rparen linebreak)
  (pattern rparen compound-list linebreak)
  (lparen pattern rparen linebreak)
  (lparen pattern rparen compound-list linebreak))

(define-nonterminal case-item
  (pattern rparen linebreak dsemi linebreak)
  (pattern rparen compound-list dsemi linebreak)
  (lparen pattern rparen linebreak dsemi linebreak)
  (lparen pattern rparen compound-list dsemi linebreak))

(define-nonterminal pattern
  (a-word pattern-tail)) ;; Apply rule 4 (must be reflected in grammar)

(define-nonterminal pattern-tail
  (pipe a-word pattern-tail) ;; Do not apply rule 4 (but /bin/sh seems to?)
  ())

(define-nonterminal if-clause
  (if-word (condition parse-compound-list) then (body parse-compound-list) fi)
  (if-word (condition parse-compound-list) then (body parse-compound-list) else-part fi))

(define-nonterminal else-part
  (elif (condition parse-compound-list) then (body parse-compound-list) else-part)
  (elif (condition parse-compound-list) then (body parse-compound-list))
  (else (body parse-compound-list)))

(define-nonterminal while-clause
  (while (condition parse-compound-list) (body parse-do-group)))

(define-nonterminal until-clause
  (until (condition parse-compound-list) (body parse-do-group)))

(define-nonterminal function-definition
  (fname lparen rparen linebreak function-body))

(define-nonterminal function-body
  (compound-command redirect-list) ;; Apply rule 9 (need not be reflected in the grammar)
  (compound-command)) ;; Apply rule 9 (need not be reflected in the grammar)

(define-nonterminal fname
  parse-name) ;; Apply rule 8 (must be reflected in the grammar)

(define-nonterminal brace-group
  (lbrace compound-list rbrace))

(define-nonterminal do-group
  (do-word compound-list done)) ;; Apply rule 6 (need not be reflected in the grammar)

(define-nonterminal simple-command
  (cmd-prefix cmd-word cmd-suffix)
  (cmd-prefix cmd-word)
  (cmd-prefix)
  (cmd-name cmd-suffix)
  (cmd-name))

(define-nonterminal cmd-name
  parse-a-word) ;; Apply rule 7a (might need to be reflected in the grammar)

(define-nonterminal cmd-word
  parse-a-word) ;; Apply rule 7b (might need to be reflected in the grammar)

(define-nonterminal cmd-prefix
  (io-redirect cmd-prefix-tail)
  (assignment-word cmd-prefix-tail))

(define-nonterminal cmd-prefix-tail
  (io-redirect cmd-prefix-tail)
  (assignment-word cmd-prefix-tail)
  ())

(define-nonterminal cmd-suffix
  (io-redirect cmd-suffix-tail)
  (a-word cmd-suffix-tail))

(define-nonterminal cmd-suffix-tail
  (io-redirect cmd-suffix-tail)
  (a-word cmd-suffix-tail)
  ())

(define-nonterminal redirect-list
  (io-redirect redirect-list-tail))

(define-nonterminal redirect-list-tail
  (io-redirect redirect-list-tail)
  ())

(define-nonterminal io-redirect
  parse-io-file
  (io-number (io-source parse-io-file))
  parse-io-here
  (io-number (io-source parse-io-here)))

(define-nonterminal io-file
  ((redirect parse-less) filename)
  ((redirect parse-lessand) (fd-description parse-simple-word))
  ((redirect parse-great) filename)
  ((redirect parse-greatand) (fd-description parse-simple-word))
  ((redirect parse-dgreat) filename)
  ((redirect parse-lessgreat) filename)
  ((redirect parse-clobber) filename))

(define-nonterminal filename
  parse-a-word) ;; Apply rule 2 (need not be reflected in grammar)

(define-nonterminal io-here
  (dless here-end)
  (dlessdash here-end))

(define-nonterminal here-end
  (a-word)) ;; Apply rule 3 (need not be reflected in grammar)

(define-nonterminal newline-list
  (newline newline-list-tail))

(define-nonterminal newline-list-tail
  (newline newline-list-tail)
  ())

(define-nonterminal linebreak
  (newline-list)
  ())

(define-nonterminal separator-op
  parse-par
  parse-semi)

(define-nonterminal separator
  (separator-op linebreak)
  (newline-list))

(define-nonterminal sequential-sep
  (semi linebreak)
  (newline-list))

(define-advice parse-cmd-name :around posix-rule (iter)
  (when (typep (peek-lookahead-iterator iter) 'reserved-word)
    (return-from parse-cmd-name
      (values
       nil
       "No reserved words")))
  (call-next-method))

(defun command-iterator (token-iterator)
  "Given a `lookahead-iterator' that produces tokens, return an
iterator that produces shell syntax tree objects."
  (let ((iter (syntax-iterator #'parse-start token-iterator)))
    (make-iterator ()
      (do-iterator (value iter)
        (when (eq value :eof)
          (stop))
        (emit value))
      (stop))))

(defgeneric parse-shell (source))

(defmethod parse-shell ((s string))
  (parse-shell (make-string-input-stream s)))

(defmethod parse-shell ((s stream))
  (parse-shell (lookahead-iterator-wrapper (token-iterator s))))

(defmethod parse-shell ((iter lookahead-iterator))
  (next (command-iterator iter)))
