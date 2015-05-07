with (import <nixpkgs> {}).pkgs;
(haskell-ng.packages.ghc7101.callPackage ./. {}).env
