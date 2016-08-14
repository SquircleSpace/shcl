(defpackage :shcl-test.suites
  (:use :common-lisp :fiveam)
  (:export #:shcl #:shcl-failing #:lexer #:lexer-failing))

(defpackage :shcl-test.lexer
  (:use :common-lisp :fiveam :shcl-test.suites :shcl.lexer))
