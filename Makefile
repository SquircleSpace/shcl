SHCL_DEPENDS= core/*.lisp shell/*.lisp shcl.asd libshcl-support.so make.lisp
all: shcl

%.o : %.c
	clang -fPIC -o $@ -c $<

core/support/spawn.o: core/support/spawn.c core/support/spawn.h
	clang -fPIC -o $@ -c $<

libshcl-support.so: core/support/macros.o core/support/spawn.o
	clang -shared -o $@ $^

shcl: ${SHCL_DEPENDS}
	sbcl --load make.lisp

.PHONY: test
test: test/*.lisp ${SHCL_DEPENDS}
	sbcl --load test.lisp
