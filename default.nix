{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
with import <nixpkgs> {};
let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_2 reducers tasty tastyHunit  tastyQuickcheck tastySmallcheck split;
in cabal.mkDerivation (self: {
  pname = "tasty-rerun";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ reducers tasty ];
  buildTools = [ cabalInstall_1_18_0_2 ];
  testDepends = [ tastyHunit tastyQuickcheck tastySmallcheck split ];
})