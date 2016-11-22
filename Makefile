all: shcl

%.o : %.c
	clang -fPIC -o $@ -c $<

support/spawn.o: support/spawn.c support/spawn.h
	clang -fPIC -o $@ -c $<

libshcl-support.so: support/macros.o support/spawn.o
	clang -shared -o $@ $^

shcl: *.lisp support/support.lisp shcl.asd libshcl-support.so make.lisp
	sbcl --load make.lisp
