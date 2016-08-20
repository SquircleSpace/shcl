(defpackage :shcl.utility
  (:use :common-lisp :alexandria :bordeaux-threads)
  (:shadow #:when-let #:when-let*)
  (:export
   #:define-once-global #:required #:required-argument-missing #:optimization-settings
   #:when-let #:when-let* #:try #:debug-log #:logging-enabled-p #:status
   #:make-extensible-vector
   ;; Hooks
   #:define-hook #:add-hook #:remove-hook #:run-hook #:on-revival
   #:observe-revival #:on-dump #:observe-dump
   ;; Iterators
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator #:seq-iterator
   #:make-iterator-lookahead #:do-iterator #:peek-lookahead-iterator
   #:move-lookahead-to #:map-iterator #:iterator-values))

(defpackage :shcl.thread
  (:use :common-lisp :shcl.utility :bordeaux-threads)
  (:export
   ;; Semaphores
   #:semaphore #:make-semaphore #:semaphore-signal #:semaphore-wait
   #:semaphore-p
   #:queue #:make-queue #:enqueue #:dequeue #:dequeue-no-block #:queue-p
   #:queue-thread #:make-queue-thread #:close-queue-thread #:kill-queue-thread
   #:queue-thread-p
   #:async-f #:async #:sync-f #:sync))

(defpackage :shcl.lexer
  (:use :common-lisp :shcl.utility)
  (:export
   ;; Token classes
   #:token #:a-word #:eof #:simple-word #:compound-word
   #:assignment-word #:name #:io-number #:literal-token #:newline
   #:reserved-word #:single-quote #:double-quote #:command-word
   #:variable-expansion-word

   ;; Slot accessors
   #:token-value #:simple-word-text #:compound-word-parts
   #:assignment-word-name #:assignment-word-value-word #:io-number-fd
   #:literal-token-string #:single-quote-contents #:double-quote-parts
   #:command-word-tokens #:variable-expansion-word-variable

   ;; Operators
   #:and-if #:or-if #:dsemi #:dless #:dgreat #:lessand #:greatand
   #:lessgreat #:dlessdash #:clobber #:semi #:par #:pipe #:lparen
   #:rparen #:great #:less

   ;; Reserved words
   #:if-word #:then #:else #:elif #:fi #:do-word #:done #:case-word #:esac
   #:while #:until #:for #:lbrace #:rbrace #:bang #:in

   ;; Functions
   #:tokenize #:token-iterator #:tokens-in-string #:tokens-in-stream

   ;; Extensible reading
   #:set-character-handler #:make-shell-dispatch-character
   #:set-shell-dispatch-character #:*shell-readtable*
   #:shell-extensible-read #:reset-shell-readtable
   #:lexer-context-mark-end-of-token))

(defpackage :shcl.parser
  (:use :common-lisp :alexandria :shcl.lexer :shcl.utility)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
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
   #:errno #:mode-t #:environ))

(defpackage :shcl.posix
  (:use :common-lisp :cffi :trivial-garbage :shcl.posix-types :shcl.utility)
  (:export
   #:posix-spawn-file-actions-init #:posix-spawn-file-actions-destroy
   #:with-posix-spawn-file-actions #:posix-spawn-file-actions-addclose
   #:posix-spawn-file-actions-addopen #:posix-spawn-file-actions-adddup2
   #:posix-spawnp #:posix-spawnattr-init #:posix-spawnattr-destroy
   #:with-posix-spawnattr #:environment-iterator))

(defpackage :shcl.fork-exec
  (:use :common-lisp :alexandria :cffi :shcl.utility :shcl.shell-grammar
        :shcl.posix-types :shcl.posix)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:import-from :cl-fad #:list-directory #:directory-pathname-p #:pathname-as-file)
  (:export #:run))

(defpackage :shcl.environment
  (:use :common-lisp :shcl.utility :shcl.posix)
  (:export
   #:*environment* #:linearized-exported-environment #:with-environment-scope
   #:env #:export-variable #:unexport-variable #:clear-environment #:exported-p
   #:unset-env
   #:$ifs #:$path #:$pwd #:$oldpwd))

(defpackage :shcl.expand
  (:use :common-lisp :shcl.utility :shcl.lexer :shcl.environment)
  (:export
   #:expansion-for-word #:expansion-for-words #:set-alias #:unalias
   #:expand #:make-string-fragment #:word-boundary))

(defpackage :shcl.evaluate
  (:use :common-lisp :trivial-garbage :alexandria :bordeaux-threads
        :shcl.utility :shcl.shell-grammar :shcl.lexer :shcl.fork-exec
        :shcl.thread :shcl.expand :shcl.environment)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:export #:evaluate))

(defpackage :shcl.baking
  (:use :common-lisp :shcl.utility :shcl.lexer :shcl.thread)
  (:export #:bake-form-for-token #:bake-tokens))

(defpackage :shcl.lisp-interpolation
  (:use :common-lisp :shcl.utility :shcl.lexer :shcl.shell-grammar
        :shcl.evaluate :shcl.expand :shcl.baking)
  (:export
   #:enable-shell-splice-syntax #:enable-reader-syntax))

(defpackage :shcl
  (:use :common-lisp :shcl.lexer :shcl.shell-grammar :shcl.utility
        :shcl.evaluate :shcl.baking :shcl.thread :shcl.lisp-interpolation)
  (:export #:main))
