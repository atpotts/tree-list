{haskellPackages, lib}:
let src = lib.cleanSource (lib.cleanSourceWith {
        filter=name: type: let baseName = baseNameOf (toString name);
            in ! (type == "directory" && baseName == "dist");
        src=./.;
    });

in haskellPackages.callCabal2nix "tree-list" ./. {}

