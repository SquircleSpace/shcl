(in-package :shcl/posix-types)

(include "errno.h" "sys/types.h" "sys/stat.h" "fcntl.h" "spawn.h" "dirent.h")

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

(constant (s-irusr "S_IRUSR"))
(constant (s-iwusr "S_IWUSR"))
(constant (s-irgrp "S_IRGRP"))
(constant (s-iroth "S_IROTH"))
(constant (o-rdonly "O_RDONLY"))
(constant (o-wronly "O_WRONLY"))
(constant (o-rdwr "O_RDWR"))
(constant (o-creat "O_CREAT"))
(constant (o-trunc "O_TRUNC"))
(constant (o-append "O_APPEND"))
(constant (f-getfd "F_GETFD"))
(constant (wuntraced "WUNTRACED"))
(constant (eintr "EINTR"))
