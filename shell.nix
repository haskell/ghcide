# This shell.nix file is designed for use with cabal build
# It does **not** aim to replace Cabal

# Maintaining this file:
#
#     - Bump the nixpkgs version using `niv update nixpkgs`

{ compiler ? "default",
  withHoogle ? false,
  nixpkgs ? import ./nix {}
 }:

with nixpkgs;

let defaultCompiler = "ghc" + lib.replaceStrings ["."] [""] haskellPackages.ghc.version;
    haskellPackagesForProject = if compiler == "default" then haskellPackages else haskell.packages.${compiler};
    ghcide = haskell.lib.doCheck (haskellPackagesForProject.callCabal2nixWithOptions "ghcide" ./. "--benchmark" {});
    isSupported = compiler == "default" || compiler == defaultCompiler;
in
haskellPackagesForProject.shellFor {
  inherit withHoogle;
  doBenchmark = true;
  packages = p: [if isSupported then ghcide else ghc-paths];
  buildInputs = [
    gmp
    zlib
    ncurses

    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.ormolu
    haskellPackages.stylish-haskell
  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
