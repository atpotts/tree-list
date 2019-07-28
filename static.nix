let
  pkgs = import <unstable> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    (pkgs.callPackage ./shell.nix {})
    pkgs.gmp5.static
    pkgs.glibc.static
    pkgs.zlib.static
    pkgs.zlib.dev
  ];
}
