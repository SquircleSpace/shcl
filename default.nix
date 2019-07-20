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
let
  mkNixlispBundle = import ./nixlisp/mkNixlispBundle.nix pkgs;
  nixlispBundle = mkNixlispBundle (import ./nixlisp/qlDist.nix);
in
pkgs.stdenv.mkDerivation rec {
  name = "shcl";
  src = ./.;
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    pkgs.stdenv
    pkgs.libedit
    pkgs.sbcl
    nixlispBundle
  ];
  LD_LIBRARY_PATH = "${pkgs.stdenv.lib.makeLibraryPath buildInputs}";
  installPhase = ''
    make PREFIX=$out install LISP="cl-wrapper.sh sbcl --eval '(push :shcl-nix *features*)'"
  '';
  buildPhase = ''
    libedit="${pkgs.libedit}" make LISP="cl-wrapper.sh sbcl --eval '(push :shcl-nix *features*)'"
  '';
  dontStrip = true;
}
