with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages.override {
      extraPrefs = self: {
        tastyRerun = self.callPackage ./. {};
      };
    };
in lib.overrideDerivation haskellPackages.tastyRerun (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
   })
