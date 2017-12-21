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
    pkgs.lispPackages.cl-cli # WTFPL
    pkgs.lispPackages.cl-fad # BSD 2-clause
    pkgs.lispPackages.closer-mop # MIT
    pkgs.lispPackages.cl-ppcre # BSD 2-clause
    pkgs.lispPackages.fset # Lisp LGPL
    pkgs.lispPackages.swank # Public domain
    pkgs.lispPackages.trivial-garbage # Public domain
    pkgs.lispPackages.trivial-gray-streams # MIT
    pkgs.sbcl
    # For test
    pkgs.lispPackages.prove # MIT
  ];
  LD_LIBRARY_PATH = "${pkgs.stdenv.lib.makeLibraryPath buildInputs}";
  installPhase = ''
    mkdir -p $out/bin
    cp shcl $out/bin
  '';
  dontStrip = true;
}
