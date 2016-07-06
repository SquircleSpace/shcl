(in-package :shcl.shell-grammar)

(optimization-settings)

(define-parser *shell-grammar*
  (:start-symbol complete-command)
  (:terminals
   (token a-word assignment-word name newline io-number and-if
          or-if dsemi dless dgreat lessand greatand lessgreat dlessdash
          clobber if-word then else elif fi do-word done case-word esac while until
          for lbrace rbrace bang in semi par pipe lparen rparen great less))

  (complete-command
   (newline-list complete-command)
   (command-list command-separator)
   command-list)

  (command-list
   (and-or command-list-tail))

  (command-list-tail
   (separator-op and-or command-list-tail)
   ())

  (and-or
   (pipeline and-or-tail))

  (and-or-tail
   (and-if linebreak pipeline and-or-tail)
   (or-if linebreak pipeline and-or-tail)
   ())

  (pipeline
   (bang pipe-sequence)
   pipe-sequence)

  (pipe-sequence
   (command pipe-sequence-tail))

  (pipe-sequence-tail
   (pipe linebreak command pipe-sequence-tail)
   ())

  (command
   (compound-command redirect-list)
   compound-command
   function-definition
   simple-command)

  (compound-command
   brace-group
   subshell
   for-clause
   case-clause
   if-clause
   while-clause
   until-clause)

  (subshell
   (lparen compound-list rparen))

  (compound-list
   (term separator)
   term
   (newline-list term separator)
   (newline-list term))

  (term
   (and-or term-tail))

  (term-tail
   (separator and-or term-tail)
   ())

  (for-clause
   (for name-nt linebreak do-group)
   (for name-nt linebreak in-nt sequential-sep do-group)
   (for name-nt linebreak in-nt wordlist sequential-sep do-group))

  (name-nt
   (name)) ;; Apply rule 5 (need not be reflected in the grammar)

  (in-nt
   (in)) ;; Apply rule 6 (need not be reflected in the grammar)

  (wordlist
   (a-word wordlist-tail))

  (wordlist-tail
   (a-word wordlist-tail)
   ())

  (case-clause
   (case-word a-word linebreak in-nt linebreak case-list esac)
   (case-word a-word linebreak in-nt linebreak case-list-ns esac)
   (case-word a-word linebreak in-nt linebreak esac))

  (case-list-ns
   (case-list case-item-ns)
   (case-item-ns))

  (case-list
   (case-item case-list-tail))

  (case-list-tail
   (case-item case-item-tail)
   ())

  (case-item-ns
   (pattern rparen linebreak)
   (pattern rparen compound-list linebreak)
   (lparen pattern rparen linebreak)
   (lparen pattern rparen compound-list linebreak))

  (case-item
   (pattern rparen linebreak dsemi linebreak)
   (pattern rparen compound-list dsemi linebreak)
   (lparen pattern rparen linebreak dsemi linebreak)
   (lparen pattern rparen compound-list dsemi linebreak))

  (pattern
   (a-word pattern-tail)) ;; Apply rule 4 (must be reflected in grammar)

  (pattern-tail
   (pipe a-word pattern-tail) ;; Do not apply rule 4 (but /bin/sh seems to?)
   ())

  (if-clause
   (if-word compound-list then compound-list fi)
   (if-word compound-list then compound-list else-part fi))

  (else-part
   (elif compound-list then compound-list else-part)
   (elif compound-list then compound-list)
   (else compound-list))

  (while-clause
   (while compound-list do-group))

  (until-clause
   (until compound-list do-group))

  (function-definition
   (fname lparen rparen linebreak function-body))

  (function-body
   (compound-command redirect-list) ;; Apply rule 9 (need not be reflected in the grammar)
   (compound-command)) ;; Apply rule 9 (need not be reflected in the grammar)

  (fname
   name) ;; Apply rule 8 (must be reflected in the grammar)

  (brace-group
   (lbrace :strict compound-list rbrace))

  (do-group
   (do-word compound-list done)) ;; Apply rule 6 (need not be reflected in the grammar)

  (simple-command
   (cmd-prefix cmd-word cmd-suffix)
   (cmd-prefix cmd-word)
   (cmd-prefix)
   (cmd-name cmd-suffix)
   (cmd-name))

  (cmd-name
   (a-word)) ;; Apply rule 7a (might need to be reflected in the grammar)

  (cmd-word
   (a-word)) ;; Apply rule 7b (might need to be reflected in the grammar)

  (cmd-prefix
   (io-redirect cmd-prefix-tail)
   (assignment-word cmd-prefix-tail))

  (cmd-prefix-tail
   (io-redirect cmd-prefix-tail)
   (assignment-word cmd-prefix-tail)
   ())

  (cmd-suffix
   (io-redirect cmd-suffix-tail)
   (a-word cmd-suffix-tail))

  (cmd-suffix-tail
   (io-redirect cmd-suffix-tail)
   (a-word cmd-suffix-tail)
   ())

  (redirect-list
   (io-redirect redirect-list-tail))

  (redirect-list-tail
   (io-redirect redirect-list-tail)
   ())

  (io-redirect
   io-file
   (io-number io-file)
   io-here
   (io-number io-here))

  (io-file
   (less filename)
   (lessand filename)
   (great filename)
   (greatand filename)
   (dgreat filename)
   (lessgreat filename)
   (clobber filename))

  (filename
   (a-word)) ;; Apply rule 2 (need not be reflected in grammar)

  (io-here
   (dless here-end)
   (dlessdash here-end))

  (here-end
   (a-word)) ;; Apply rule 3 (need not be reflected in grammar)
  (newline-list
   (newline newline-list-tail))

  (newline-list-tail
   (newline newline-list-tail)
   ())

  (linebreak
   (newline-list)
   ())

  (separator-op
   par
   semi)

  (separator
   (separator-op linebreak)
   (newline-list))

  (command-separator
   separator-op)

  (sequential-sep
   (semi linebreak)
   (newline-list)))

(defmethod parse :around ((type (eql 'cmd-name)) iter)
  (when (typep (peek-lookahead-iterator iter) 'reserved-word)
    (no-parse "Reserved words aren't allowed here" 'a-word))
  (call-next-method))

(defun command-iterator (token-iterator)
  (syntax-iterator *shell-grammar* token-iterator))

(defgeneric parse-shell (source))

(defmethod parse-shell ((s string))
  (parse-shell (make-string-input-stream s)))

(defmethod parse-shell ((s stream))
  (parse-shell (token-iterator s)))

(defmethod parse-shell ((iter token-iterator))
  (next (command-iterator iter)))
