(in-package :shcl/shell/prompt-types)

(include "sys/ioctl.h" "histedit.h")

(cstruct winsize "struct winsize"
         (ws-row "ws_row" :type :unsigned-short)
         (ws-col "ws_col" :type :unsigned-short)
         (ws-xpixel "ws_xpixel" :type :unsigned-short)
         (ws-ypixel "ws_ypixel" :type :unsigned-short))

(constant (tiocgwinsz "TIOCGWINSZ"))

(cstruct lineinfo "LineInfo"
         (buffer "buffer" :type (:pointer :char))
         (cursor "cursor" :type (:pointer :char))
         (lastchar "lastchar" :type (:pointer :char)))

(constant (+el-prompt+ "EL_PROMPT"))
(constant (+el-rprompt+ "EL_RPROMPT"))
(constant (+el-editor+ "EL_EDITOR"))
(constant (+el-addfn+ "EL_ADDFN"))
(constant (+el-bind+ "EL_BIND"))

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
