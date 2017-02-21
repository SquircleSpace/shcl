(in-package :shcl/shell/prompt-types)

(include "histedit.h")

(ctype wchar-t "wchar_t")

(cstruct histevent "HistEvent"
         (num "num" :type :int)
         (str "str" :type :string))

(cstruct lineinfo "LineInfo"
         (buffer "buffer" :type (:pointer :char))
         (cursor "cursor" :type (:pointer :char))
         (lastchar "lastchar" :type (:pointer :char)))

(constant (+el-prompt+ "EL_PROMPT"))
(constant (+el-rprompt+ "EL_RPROMPT"))
(constant (+el-editor+ "EL_EDITOR"))
(constant (+el-addfn+ "EL_ADDFN"))
(constant (+el-bind+ "EL_BIND"))
(constant (+el-hist+ "EL_HIST"))

(constant (+cc-norm+ "CC_NORM"))
(constant (+cc-newline+ "CC_NEWLINE"))
(constant (+cc-eof+ "CC_EOF"))
(constant (+cc-arghack+ "CC_ARGHACK"))
(constant (+cc-refresh+ "CC_REFRESH"))
(constant (+cc-refresh_beep+ "CC_REFRESH_BEEP"))
(constant (+cc-cursor+ "CC_CURSOR"))
(constant (+cc-redisplay+ "CC_REDISPLAY"))
(constant (+cc-error+ "CC_ERROR"))
(constant (+cc-fatal+ "CC_FATAL"))

(constant (+h-setsize+ "H_SETSIZE"))
(constant (+h-enter+ "H_ENTER"))
