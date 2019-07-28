{ haskellPackages, lib }:
let
  stripHash = path:
    let m = builtins.split "^[a-z0-9]{32}-" (baseNameOf path);
    in if builtins.length m == 3 then builtins.elemAt m 2 else path;
  src = with lib;
    cleanSource (cleanSourceWith {
      src = ./.;
      filter = name: type:
        let baseName = baseNameOf (toString name);
        in (!(type == "directory" && baseName == "dist"))
        && (!hasPrefix "." baseName) && (!hasSuffix ".nix" name);
    });
  srcName = stripHash (baseNameOf src);
in haskellPackages.callCabal2nix srcName src { }

