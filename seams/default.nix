{ haskell }:
let
  hps = haskell.packages.ghc923.extend (final: prev: rec {
   seams-cms = final.callCabal2nix "seams" ./. { };
 });
in
  hps.seams-cms
