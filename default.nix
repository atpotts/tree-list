{pkgs ? import <nixpkgs> {},
 lib ? pkgs.lib,
 haskellPackages ? pkgs.haskellPackages
}:
pkgs.callPackage ./derivation.nix {}
