(defpackage :shcl.utility
  (:use :common-lisp)
  (:export
   #:define-once-global #:required #:required-argument-missing #:optimization-settings
   #:try
   ;; Iterators
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator
   #:make-iterator-lookahead #:do-iterator #:peek-lookahead-iterator
   #:move-lookahead-to))

(defpackage :shcl.lexer
  (:use :common-lisp :shcl.utility)
  (:export
   ;; Base classes
   #:token #:a-word #:eof #:io-number #:literal-token #:newline #:name
   #:assignment-word
   ;; Operators
   #:and-if #:or-if #:dsemi #:dless #:dgreat #:lessand #:greatand
   #:lessgreat #:dlessdash #:clobber #:semi #:par #:pipe #:lparen
   #:rparen #:great #:less
   ;; Reserved words
   #:if-word #:then #:else #:elif #:fi #:do-word #:done #:case-word #:esac
   #:while #:until #:for #:lbrace #:rbrace #:bang #:in
   ;; Functions
   #:tokenize #:token-iterator #:tokens-in-string #:tokens-in-stream))

(defpackage :shcl.parser
  (:use :common-lisp :alexandria :shcl.lexer :shcl.utility)
  (:export #:command-iterator))

(defpackage :shcl
  (:use :common-lisp :shcl.utility)
  (:export #:main))
