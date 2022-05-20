let
    nixpkgs = import ../nix/release.nix;
    derivasjon = nixpkgs.myHaskellPackages.derivasjon;
in
with nixpkgs;

dockerTools.buildImage {
  name = "derivasjon-image";
  contents = [ derivasjon ];

  config = {
    Cmd = [ "${derivasjon}/bin/derivasjon" ];
    ExposedPorts = {
      "8000/tcp" = {};
    };
  };
}
