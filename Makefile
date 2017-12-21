# Copyright 2017 Bradley Jensen
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

SHCL_DEBUG=

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
	SHCL_DEBUG="${SHCL_DEBUG}" sbcl --eval '(require :asdf)' --load make.lisp

.PHONY: test
test: test/*.lisp ${SHCL_DEPENDS} Makefile
	sbcl --eval '(require :asdf)' --load test.lisp
