SHCL_DEPENDS= core/*.lisp shell/*.lisp shcl.asd libshcl-support.so make.lisp
SUPPORT_OBJS= core/support/macros.o core/support/spawn.o

all: shcl

%.o : %.c Makefile
	clang -fPIC -o $@ -c $<

core/support/spawn.o: core/support/spawn.c core/support/spawn.h Makefile
	clang -fPIC -o $@ -c $<

libshcl-support.so: ${SUPPORT_OBJS} Makefile
	clang -shared -o $@ ${SUPPORT_OBJS}

shcl: ${SHCL_DEPENDS} Makefile
	sbcl --eval '(require :asdf)' --load make.lisp

.PHONY: test
test: test/*.lisp ${SHCL_DEPENDS} Makefile
	sbcl --eval '(require :asdf)' --load test.lisp
