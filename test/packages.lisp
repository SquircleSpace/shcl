(defpackage :shcl-test.suites
  (:use :common-lisp :fiveam)
  (:export #:shcl #:shcl-failing #:lexer #:lexer-failing #:environment))

(defpackage :shcl-test.lexer
  (:use :common-lisp :fiveam :shcl-test.suites :shcl.lexer))

(defpackage :shcl-test.environment
  (:use :common-lisp :fiveam :shcl-test.suites :shcl.environment))
