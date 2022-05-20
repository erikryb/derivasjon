let
    nixpkgs = import ../nix/release.nix;
    derivasjon = import ../nix/default.nix;
in
    nixpkgs.myHaskellPackages.shellFor {
        packages = p: [p.derivasjon];
        genericBuilderArgsModifier = args: args // { doCheck = true; doBenchmark = true; };
        buildInputs = [
            nixpkgs.moreutils
            (nixpkgs.writeShellScriptBin "starthoogle" ''hoogle server --local -p 3000 -n'')
            nixpkgs.myHaskellPackages.cabal-install
            nixpkgs.myHaskellPackages.ghcid
            nixpkgs.myHaskellPackages.require
            nixpkgs.myHaskellPackages.hlint
            nixpkgs.myHaskellPackages.splint
            nixpkgs.myHaskellPackages.pretty-simple
            nixpkgs.myHaskellPackages.ormolu
            nixpkgs.myHaskellPackages.ghcide
        ];
        withHoogle = true;
    }
