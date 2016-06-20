(in-package :shcl.yacc-parser)

(defun iterator-as-lexer (iterator)
  (lambda ()
    (multiple-value-bind (value more) (next iterator)
      (if more
          (values (class-name (class-of value)) value)
          (values nil nil)))))

(defun list-to-lexer (list)
  (iterator-as-lexer (list-iterator list)))

(define-parser *shell-parser*
  (:start-symbol complete-command)
  (:terminals
   (token a-word assignment-word name newline io-number and-if
          or-if dsemi dless dgreat lessand greatand lessgreat dlessdash
          clobber if-word then else elif fi do-word done case esac while until
          for lbrace rbrace bang in semi par pipe lparen rparen great less))
  ;; (:precedence)

  (complete-command
   (list separator)
   (list))

  (list
   (list separator-op and-or)
   (and-or))

  (and-or
   (pipeline)
   (and-or and-if linebreak pipeline)
   (and-or or-if linebreak pipeline))

  (pipeline
   (pipe-sequence)
   (bang pipe-sequence))

  (pipe-sequence
   (command)
   (pipe-sequence pipe linebreak command))

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
   (term separator and-or)
   (and-or))

  (for-clause
   (for name-nt linebreak do-group)
   (for name-nt linebreak in-nt sequential-sep do-group)
   (for name-nt linebreak in-nt wordlist sequential-sep do-group))

  (name-nt
   (name))

  (in-nt
   (in))

  (wordlist
   (wordlist a-word)
   (a-word))

  (case-clause
   (case-word a-word linebreak in-nt linebreak case-list esac)
   (case-word a-word linebreak in-nt linebreak case-list-ns esac)
   (case-word a-word linebreak in-nt linebreak esac))

  (case-list-ns
   (case-list case-item-ns)
   (case-item-ns))

  (case-list
   (case-list case-item)
   (case-item))

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
   (a-word)
   (pattern pipe a-word))

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
   (compound-command)
   (compound-command redirect-list))

  (fname
   name)

  (brace-group
   (lbrace compound-command rbrace))

  (do-group
   (do compound-list done))

  (simple-command
   (cmd-prefix cmd-word cmd-suffix)
   (cmd-prefix cmd-word)
   (cmd-prefix)
   (cmd-name cmd-suffix)
   (cmd-name))

  (cmd-name
   (a-word))

  (cmd-word
   (a-word))

  (cmd-prefix
   (io-redirect)
   (cmd-prefix io-redirect)
   (assignment-word)
   (cmd-prefix assignment-word))

  (cmd-suffix
   (io-redirect)
   (cmd-suffix io-redirect)
   (a-word)
   (cmd-suffix a-word))

  (redirect-list
   (io-redirect)
   (redirect-list io-redirect))

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
   (a-word))

  (io-here
   (dless here-end)
   (dlessdash here-end))

  (here-end
   (a-word))

  (newline-list
   (newline)
   (newline-list newline))

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
