{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, mtl, optparse-applicative
      , reducers, split, stdenv, stm, tagged, tasty, transformers
      }:
      mkDerivation {
        pname = "tasty-rerun";
        version = "1.1.6";
        src = ./.;
        libraryHaskellDepends = [
          base containers mtl optparse-applicative reducers split stm tagged
          tasty transformers
        ];
        homepage = "http://github.com/ocharles/tasty-rerun";
        description = "Run tests by filtering the test tree depending on the result of previous test runs";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
