{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  name = "shcl";
  src = ./.;
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    pkgs.clang
    pkgs.libedit
    pkgs.lispPackages.alexandria
    pkgs.lispPackages.bordeaux-threads
    pkgs.lispPackages.cffi
    pkgs.lispPackages.cffi-grovel
    pkgs.lispPackages.cl-cli
    pkgs.lispPackages.cl-fad
    pkgs.lispPackages.closer-mop
    pkgs.lispPackages.cl-ppcre
    pkgs.lispPackages.fset
    pkgs.lispPackages.lisp-namespace
    pkgs.lispPackages.quicklisp
    pkgs.lispPackages.swank
    pkgs.lispPackages.trivial-garbage
    pkgs.lispPackages.trivial-gray-streams
    pkgs.sbcl
    # For test
    pkgs.lispPackages.prove
  ];
  LD_LIBRARY_PATH = "${pkgs.stdenv.lib.makeLibraryPath buildInputs}";
  installPhase = ''
    mkdir -p $out/bin
    cp shcl $out/bin
  '';
  dontStrip = true;
}
