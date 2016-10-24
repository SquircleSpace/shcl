(defpackage :shcl/posix-types
  (:use :common-lisp :cffi-grovel)
  (:export
   #:size-t #:ssize-t #:pid-t #:posix-spawn-file-actions-t
   #:posix-spawnattr-t #:dirent #:d-name #:errno #:mode-t #:environ
   #:s-irusr #:s-iwusr #:s-irgrp #:s-iroth #:o-rdonly #:o-wronly
   #:o-rdwr #:o-creat #:o-trunc #:o-append #:f-getfd #:wuntraced
   #:eintr))
