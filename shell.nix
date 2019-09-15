let
  pkgs      = (import <nixpkgs> {}).pkgs;
  ghc       = pkgs.haskellPackages;
  extras    = [
                ghc.ghcid
                ghc.cabal-install
              ];
  drv       = ghc.callCabal2nix "module98" ./. {};
in ghc.shellFor {
  packages    = p: [drv];
  buildInputs = extras;
}
