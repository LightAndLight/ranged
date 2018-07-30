{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./ranged.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  typenums = haskellPackages.callPackage ./nix/typenums.nix {};
  drv = haskellPackages.callPackage f { inherit typenums; };

in

  drv
