{ cabal, reducers, tasty, split, tastyHunit, tastyQuickcheck, tastySmallcheck }:
cabal.mkDerivation (self: {
  pname = "tasty-rerun";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ reducers tasty split ];
  testDepends = [ tastyHunit tastyQuickcheck tastySmallcheck ];
})
