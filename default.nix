# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal }:

cabal.mkDerivation (self: {
  pname = "ghc-srcspan-plugin";
  version = "0.2.0.0";
  src = ./.;
  meta = {
    description = "Generic GHC Plugin for annotating Haskell code with source location data";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
