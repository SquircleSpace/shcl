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

{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  name = "shcl";
  src = ./.;
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    pkgs.clang
    pkgs.libedit # BSD 3-clause
    pkgs.lispPackages.alexandria # Public domain
    pkgs.lispPackages.bordeaux-threads # MIT
    pkgs.lispPackages.cffi # MIT
    pkgs.lispPackages.cffi-grovel # MIT
    pkgs.lispPackages.cl-fad # BSD 2-clause
    pkgs.lispPackages.closer-mop # MIT
    pkgs.lispPackages.cl-ppcre # BSD 2-clause
    pkgs.lispPackages.fset # Lisp LGPL
    pkgs.lispPackages.swank # Public domain
    pkgs.lispPackages.trivial-garbage # Public domain
    pkgs.lispPackages.trivial-gray-streams # MIT
    pkgs.lispPackages.lisp-namespace # Lisp LGPL
    pkgs.lispPackages.clwrapper
    pkgs.sbcl
    # For test
    pkgs.lispPackages.prove # MIT
  ];
  LD_LIBRARY_PATH = "${pkgs.stdenv.lib.makeLibraryPath buildInputs}";
  installPhase = ''
    mkdir -p $out/bin
    cp shcl $out/bin
    mkdir -p $out/lib
    cp libshcl-support.so $out/lib
  '';
  buildPhase = ''
    libedit="${pkgs.libedit}" make LISP="cl-wrapper.sh sbcl --eval '(push :shcl-nix *features*)'"
  '';
  dontStrip = true;
}
