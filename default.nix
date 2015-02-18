# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, Cabal, fgl, graphviz, hackageDb, text }:

cabal.mkDerivation (self: {
  pname = "depviz";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ Cabal fgl graphviz hackageDb text ];
  meta = {
    description = "GraphViz + Hackage to Visualize Dependency Graphs";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})