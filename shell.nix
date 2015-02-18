with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        depViz = self.callPackage ./. {};
      };
    };
 in lib.overrideDerivation haskellPackages.depViz (attrs: {
      buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
    })

#let
#  pkgs = import <nixpkgs> {};
#  hsEnv = pkgs.haskellPackages.ghcWithPackages (hsPkgs : ([
#    hsPkgs.hlint
#    hsPkgs.hdevtools
#    hsPkgs.hasktags] ++
#    # Include the deps of our project to make them available for tools:
#    (hsPkgs.callPackage ./default.nix {}).propagatedNativeBuildInputs));
#in
#  pkgs.myEnvFun {
#    name = "depviz";
#    buildInputs = with pkgs; [
#      # development tools as fits your needs
#      binutils
#      coreutils
#      hsEnv
#      ];
#    extraCmds = ''
#      $(grep export ${hsEnv.outPath}/bin/ghc)
#    '';
#    }
