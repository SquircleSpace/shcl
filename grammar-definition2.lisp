(in-package :shcl.yacc-parser)

;; To be loaded by yacc-parser.lisp

(define-recursive-descent-parser shell
  (:start-symbol complete-command)
  (:terminals
   (token a-word assignment-word name newline io-number and-if
          or-if dsemi dless dgreat lessand greatand lessgreat dlessdash
          clobber if-word then else elif fi do-word done case esac while until
          for lbrace rbrace bang in semi par pipe lparen rparen great less))

  (wordly-word
   a-word assignment-word name if-word then else elif fi do-word done case
   esac while until for in)

  (complete-command
   (command-list separator)
   (command-list))

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
   (pipe-sequence)
   (bang pipe-sequence))

  (pipe-sequence
   (command pipe-sequence-tail))

  (pipe-sequence-tail
   (pipe linebreak command pipe-sequence-tail)
   ())

  (command
   (simple-command)
   (compound-command)
   (compound-command redirect-list)
   (function-definition))

  (compound-command
   (brace-group)
   (subshell)
   (for-clause)
   (case-clause)
   (if-clause)
   (while-clause)
   (until-clause))

  (subshell
   (lparen compound-list rparen))

  (compound-list
   (term)
   (newline-list term)
   (term separator)
   (newline-list term separator))

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
   (wordly-word wordlist-tail))

  (wordlist-tail
   (wordly-word wordlist-tail)
   ())

  (case-clause
   (case-word wordly-word linebreak in-nt linebreak case-list esac)
   (case-word wordly-word linebreak in-nt linebreak case-list-ns esac)
   (case-word wordly-word linebreak in-nt linebreak esac))

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
   (wordly-word pattern-tail)) ;; Apply rule 4 (must be reflected in grammar)

  (pattern-tail
   (pipe wordly-word pattern-tail) ;; Do not apply rule 4 (but /bin/sh seems to?)
   ())

  (if-clause
   (if-word compound-list then compound-list else-part fi)
   (if-word compound-list then compound-list fi))

  (else-part
   (elif compound-list then else-part)
   (else compound-list))

  (while-clause
   (while compound-list do-group))

  (until-clause
   (until compound-list do-group))

  (function-definition
   (fname lparen rparen linebreak function-body))

  (function-body
   (compound-command) ;; Apply rule 9 (need not be reflected in the grammar)
   (compound-command redirect-list)) ;; Apply rule 9 (need not be reflected in the grammar)

  (fname
   name) ;; Apply rule 8 (must be reflected in the grammar)

  (brace-group
   (lbrace compound-command rbrace))

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
   (wordly-word cmd-suffix-tail))

  (cmd-suffix-tail
   (io-redirect cmd-suffix-tail)
   (wordly-word cmd-suffix-tail)
   ())

  (redirect-list
   (io-redirect redirect-list-tail))

  (redirect-list-tail
   (io-redirect redirect-list-tail)
   ())

  (io-redirect
   (io-file)
   (io-number io-file)
   (io-here)
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
   (wordly-word)) ;; Apply rule 2 (need not be reflected in grammar)

  (io-here
   (dless here-end)
   (dlessdash here-end))

  (here-end
   (wordly-word)) ;; Apply rule 3 (need not be reflected in grammar)

  (newline-list
   (newline newline-list-tail))

  (newline-list-tail
   (newline newline-list-tail)
   ())

  (linebreak
   (newline-list)
   ())

  (separator-op
   (par)
   (semi))

  (separator
   (separator-op linebreak)
   (newline-list))

  (sequential-sep
   (semi linebreak)
   (newline-list)))
