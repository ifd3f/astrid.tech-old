{ haskell }:
let
  hps = haskell.packages.ghc922.extend (final: prev: rec {
   seams-cms = final.callCabal2nix "seams" ./. { };
 });
in
  hps.seams-cms
