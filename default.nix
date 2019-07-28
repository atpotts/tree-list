{pkgs ? (import <unstable> {}),
 lib ? pkgs.lib,
 haskellPackages ? pkgs.haskellPackages
}:
pkgs.callPackage ./derivation.nix {}
