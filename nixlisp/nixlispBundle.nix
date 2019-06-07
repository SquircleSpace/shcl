# Copyright 2019 Bradley Jensen
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

{ nixlispDist, quicklisp, clwrapper, writeTextFile, mkDerivation }:
let
  bundler = writeTextFile {
    name = "bundler.lisp";
    text = ''
      (setf *debugger-hook* (lambda (&rest args) (declare (ignore args)) (sb-debug:print-backtrace) (uiop:quit 1)))

      (eval-when (:compile-toplevel :load-toplevel :execute)
        (load "quicklisp/setup.lisp"))

      (ql:bundle-systems (mapcar 'ql-dist:name (ql-dist:provided-systems (ql-dist:find-dist "nixlisp")))
                         :to #P"bundle/")

      (uiop:quit 0)
    '';
  };
in mkDerivation rec {
  name = "nixlisp-bundle-${version}";
  version = "1.0.0";
  propagatedBuildInputs = [ clwrapper ];
  unpackPhase = "true";
  buildPhase = ''
    mkdir -p quicklisp/tmp
    mkdir -p quicklisp/local-projects
    mkdir -p quicklisp/dists/nixlisp/archives
    mkdir -p quicklisp/quicklisp

    cp -r "${quicklisp}/lib/common-lisp/quicklisp/"* quicklisp/

    # We have our own dists, thank you very much
    if [ -d quicklisp/dists/quicklisp ]; then
      rm -rdf quicklisp/dists/quicklisp
    fi

    ${nixlispDist}/bin/nixlisp-installer quicklisp/dists/nixlisp/

    # Sourcing this file with NIX_LISP_SKIP_CODE=1 always returns
    # non-zero.  Since we're building in a set -e environment we need to
    # cause that non-zero return value to become zero.
    NIX_LISP_SKIP_CODE=1 source "${clwrapper}/bin/common-lisp.sh" || true
    mkdir -p bundle
    "${clwrapper}/bin/common-lisp.sh" "$NIX_LISP_LOAD_FILE" "${bundler}"

    # This file contains nondeterministic output (e.g. date and info
    # about host).  Until we have some reason to include it (or a
    # replacement), its better to leave it out.
    rm "bundle/bundle-info.sexp"
  '';
  installPhase = ''
    mkdir -p "$out/lib/common-lisp"
    mv bundle "$out/lib/common-lisp/"

    mkdir -p "$out/lib/common-lisp-settings"
    outhash="$out"
    outhash="''${outhash##*/}"
    outhash="''${outhash%%-*}"
    cat > "$out/lib/common-lisp-settings/bundle-path-config.sh" <<EOF
    if [ -z "\''${_''${outhash}_NIX_LISP_PATH_CONFIG}" ]; then
    export _''${outhash}_NIX_LISP_PATH_CONFIG=1
    export NIX_LISP_ASDF_PATHS="$NIX_LISP_ASDF_PATHS
    $out/lib/common-lisp/bundle"
    fi
    EOF
    chmod +x "$out/lib/common-lisp-settings/bundle-path-config.sh"
  '';
}
