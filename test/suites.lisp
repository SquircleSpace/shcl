(in-package :shcl-test.suites)

(def-suite shcl
    :description "All the standard tests for shcl")

(def-suite shcl-failing
    :description "Tests which are expected to fail (for now)")

(def-suite lexer
    :in shcl)

(def-suite lexer-failing
    :in shcl-failing)

(def-suite environment
    :in shcl)

(def-suite utility
    :in shcl)

(def-suite posix
    :in shcl)
