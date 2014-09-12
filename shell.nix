with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        tastyRerun = self.callPackage ./. {};
      };
    };
in lib.overrideDerivation haskellPackages.tastyRerun (attrs: {
     buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
   })
