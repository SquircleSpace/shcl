(defpackage :shcl.utility
  (:use :common-lisp)
  (:export
   #:define-once-global #:required #:required-argument-missing #:optimization-settings
   ;; Iterators
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator
   #:make-iterator-lookahead #:do-iterator #:peek-lookahead-iterator
   #:move-lookahead-to))

(defpackage :shcl.lexer
  (:use :common-lisp :shcl.utility)
  (:export
   ;; Base classes
   #:token #:a-word #:eof #:io-number #:literal-token #:newline
   ;; Operators
   #:and-if #:or-if #:dsemi #:dless #:dgreat #:lessand #:greatand
   #:lessgreat #:dlessdash #:clobber #:semi #:par #:pipe #:paren
   #:great #:less
   ;; Reserved words
   #:if-word #:then #:else #:elif #:fi #:do-word #:done #:case-word #:esac
   #:while #:until #:for #:{ #:} #:! #:in
   ;; Functions
   #:tokenize #:token-iterator #:tokens-in-string #:tokens-in-stream))

(defpackage :shcl.yacc-parser
  (:use :common-lisp :yacc :alexandria :shcl.lexer :shcl.utility)
  (:export #:parse-stream #:parse-string #:parse #:syntax-tree))

(defpackage :shcl
  (:use :common-lisp :shcl.utility)
  (:export #:main))
