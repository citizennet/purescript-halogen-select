{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;

  nixPackages = [
    pkgs.nodejs
    pkgs.yarn   # can use to run other tools like `yarn bower ...` or `yarn ncu ...`
    pkgs.stack  # if necessary to build PureScript from source
  ];

in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = nixPackages;
  }
