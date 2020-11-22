{ sources ? import ./sources.nix }:
let
  overlay = self: pkgs:
  {
    haskell = pkgs.haskell // {
      packageOverrides = selfPkgs: superPkgs: {
            };
          };
        };

in import sources.nixpkgs
{ overlays = [ overlay ] ; config = {allowBroken = true;}; }
