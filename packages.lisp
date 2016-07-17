(defpackage :shcl.utility
  (:use :common-lisp)
  (:export
   #:define-once-global #:required #:required-argument-missing #:optimization-settings
   #:try
   ;; Iterators
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator
   #:make-iterator-lookahead #:do-iterator #:peek-lookahead-iterator
   #:move-lookahead-to #:map-iterator #:iterator-values))

(defpackage :shcl.thread
  (:use :common-lisp :shcl.utility :bordeaux-threads)
  (:export
   ;; Semaphores
   #:semaphore #:make-semaphore #:semaphore-signal #:semaphore-wait
   #:semaphore-p
   #:queue #:make-queue #:enqueue #:dequeue #:queue-p
   #:queue-thread #:make-queue-thread #:close-queue-thread #:kill-queue-thread
   #:queue-thread-p
   #:async-f #:async #:sync-f #:sync))

(defpackage :shcl.lexer
  (:use :common-lisp :shcl.utility)
  (:export
   ;; Base classes
   #:token #:a-word #:eof #:io-number #:literal-token #:newline #:name
   #:assignment-word #:reserved-word
   ;; Operators
   #:and-if #:or-if #:dsemi #:dless #:dgreat #:lessand #:greatand
   #:lessgreat #:dlessdash #:clobber #:semi #:par #:pipe #:lparen
   #:rparen #:great #:less
   ;; Reserved words
   #:if-word #:then #:else #:elif #:fi #:do-word #:done #:case-word #:esac
   #:while #:until #:for #:lbrace #:rbrace #:bang #:in
   ;; Functions
   #:tokenize #:token-iterator #:tokens-in-string #:tokens-in-stream
   #:token-value))

(defpackage :shcl.parser
  (:use :common-lisp :alexandria :shcl.lexer :shcl.utility)
  (:export #:define-parser #:syntax-iterator #:parse #:no-parse))

(defpackage :shcl.shell-grammar
  (:use :common-lisp :shcl.parser :shcl.lexer :shcl.utility)
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
   #:io-here #:here-end #:newline-list #:newline-list-tail #:linebreak
   #:separator-op #:separator #:command-separator #:sequential-sep
   #:wordly-word #:redirect))

(defpackage :shcl.posix-types
  (:export
   #:size-t #:ssize-t #:pid-t #:posix-spawn-file-actions-t #:posix-spawnattr-t
   #:errno #:mode-t))

(defpackage :shcl.posix
  (:use :common-lisp :cffi :trivial-garbage :shcl.posix-types :shcl.utility)
  (:export
   #:posix-spawn-file-actions-init #:posix-spawn-file-actions-destroy
   #:with-posix-spawn-file-actions #:posix-spawn-file-actions-addclose
   #:posix-spawn-file-actions-addopen #:posix-spawn-file-actions-adddup2
   #:posix-spawnp #:posix-spawnattr-init #:posix-spawnattr-destroy
   #:with-posix-spawnattr))

(defpackage :shcl.fork-exec
  (:use :common-lisp :alexandria :cffi :shcl.utility :shcl.shell-grammar
        :shcl.posix-types :shcl.posix)
  (:import-from :cl-fad #:list-directory #:directory-pathname-p #:pathname-as-file)
  (:export #:run))

(defpackage :shcl.evaluate
  (:use :common-lisp :trivial-garbage :alexandria :bordeaux-threads
        :shcl.utility :shcl.shell-grammar :shcl.lexer :shcl.fork-exec)
  (:export #:evaluate))

(defpackage :shcl
  (:use :common-lisp :shcl.lexer :shcl.shell-grammar :shcl.utility :shcl.evaluate)
  (:export #:main))
