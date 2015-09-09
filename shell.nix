with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, mtl, optparse-applicative
             , reducers, split, stdenv, stm, tagged, tasty, transformers
             }:
             mkDerivation {
               pname = "tasty-rerun";
               version = "1.1.4";
               src = ./.;
               buildDepends = [
                 base containers mtl optparse-applicative reducers split stm tagged
                 tasty transformers
               ];
               homepage = "http://github.com/ocharles/tasty-rerun";
               description = "Run tests by filtering the test tree depending on the result of previous test runs";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
