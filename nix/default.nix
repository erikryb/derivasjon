let
    nixpkgs = import ./release.nix;
in
    nixpkgs.myHaskellPackages.derivasjon
