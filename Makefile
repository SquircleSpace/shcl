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

SHCL_DEBUG=1

ifeq ("$(shell uname -s)","Darwin")
LIBSHCL_SUPPORT=libshcl-support.dylib
endif

ifeq ("$(shell uname -s)","Linux")
LIBSHCL_SUPPORT=libshcl-support.so
endif

SUPPORT_OBJS= core/support/macros.o core/support/spawn.o
LISP=sbcl
BUILD_CONFIG= Makefile build-settings
PREFIX=/usr/local

all: shcl

install:
	if [ -f shcl ]; then \
		mkdir -p "${PREFIX}/bin" && \
		cp shcl "${PREFIX}/bin"; \
	fi
	if [ -f "${LIBSHCL_SUPPORT}" ]; then \
		mkdir -p "${PREFIX}/"lib && \
		cp "${LIBSHCL_SUPPORT}" "${PREFIX}/lib"; \
	fi

include dependencies

.PHONY: force

build-settings: force
	@OPTIONS="$$(echo SHCL_DEBUG="${SHCL_DEBUG}"; echo LISP="${LISP}" )"; \
	echo "$${OPTIONS}" | cmp -s - $@ || { \
		echo "$${OPTIONS}" > $@; \
		echo "Build settings changed.  Forcing rebuild..."; \
	}

dependencies: shcl.asd make-deps.lisp ${BUILD_CONFIG}
	SHCL_DEBUG="${SHCL_DEBUG}" ${LISP} --eval '(require :asdf)' --load make-deps.lisp

%.o : %.c ${BUILD_CONFIG}
	clang -fPIC -o $@ -c $<

core/support/spawn.o: core/support/spawn.c core/support/spawn.h ${BUILD_CONFIG}
	clang -fPIC -o $@ -c $<

$(LIBSHCL_SUPPORT): ${SUPPORT_OBJS} ${BUILD_CONFIG}
	clang -shared -o $@ ${SUPPORT_OBJS}

shcl: shcl.asd make.lisp ${LIBSHCL_SUPPORT} ${BUILD_CONFIG}
	if [ -f "$@" ]; then rm "$@"; fi
	SHCL_DEBUG="${SHCL_DEBUG}" ${LISP} --eval '(require :asdf)' --load make.lisp

.PHONY: test
test: shcl
	@if [ "${SHCL_DEBUG}" = 1 ]; then echo -shcl-run-tests | ./shcl; else echo Cannot run tests with SHCL_DEBUG off; exit 1; fi
