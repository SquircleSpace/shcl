all: shcl

%.o : %.c
	clang -fPIC -o $@ -c $<

core/support/spawn.o: core/support/spawn.c core/support/spawn.h
	clang -fPIC -o $@ -c $<

libshcl-support.so: core/support/macros.o core/support/spawn.o
	clang -shared -o $@ $^

shcl: core/*.lisp main.lisp shcl.asd libshcl-support.so make.lisp
	sbcl --load make.lisp
