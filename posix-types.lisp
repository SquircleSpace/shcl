(in-package :shcl.posix-types)
(include "errno.h" "sys/types.h" "spawn.h" "dirent.h")

;; Types
(ctype size-t "size_t")
(ctype ssize-t "ssize_t")
(ctype pid-t "pid_t")
(ctype mode-t "mode_t")

(cstruct posix-spawn-file-actions-t "posix_spawn_file_actions_t")
(cstruct posix-spawnattr-t "posix_spawnattr_t")

(cstruct dirent "struct dirent"
  (d-name "d_name" :type :char :count 1))

;; The ever-important errno
(cvar ("errno" errno) :int)

(cvar ("environ" environ) (:pointer :string))
