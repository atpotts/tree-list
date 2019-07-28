{nix-gitignore, haskellPackages}:
let
  stripHash = path:
    let m = builtins.split "^[a-z0-9]{32}-" (baseNameOf path);
    in if builtins.length m == 3 then builtins.elemAt m 2 else path;
  src = nix-gitignore.gitignoreSource ''
    /*.nix
    .*
    result
  '' ./.;
  srcName = stripHash (baseNameOf src);
in haskellPackages.callCabal2nix srcName src { }

