(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(defpackage :shcl.utility
  (:use :common-lisp)
  (:export
   #:define-once-global
   ;; Iterators
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator
   #:make-iterator-lookahead))

(defpackage :shcl.lexer
  (:use :common-lisp :shcl.utility)
  (:export
   ;; Base classes
   #:token #:eof #:io-number #:literal-token
   ;; Operators
   #:and-if #:or-if #:dsemi #:dless #:dgreat #:lessand #:greatand
   #:lessgreat #:dlessdash #:clobber #:semi #:par #:pipe #:paren
   #:great #:less
   ;; Reserved words
   #:if-word #:then #:else #:elif #:fi #:do-word #:done #:case-word #:esac
   #:while #:until #:for #:{ #:} #:! #:in
   ;; Functions
   #:tokenize #:token-iterator #:tokens-in-string #:tokens-in-stream))

(defpackage :shcl
  (:use :common-lisp)
  (:export #:main))
