let

  nixpkgs = import (./nixpkgs/default.nix) { inherit config; };

  #ghcVersion = "ghc8107";
  ghcVersion = "ghc902";

  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];
  extra-deps = import ./extra-deps.nix;
  config = {
    allowUnfree = true;
    allowBroken = true;
    packageOverrides = pkgs: rec {
      myHaskellPackages = pkgs.haskell.packages.${ghcVersion}.override (hpArgs: {
        overrides = pkgs.lib.composeExtensions (hpArgs.overrides or (_: _: { })) (self: super: ((extra-deps super) // {
          mkDerivation = args: super.mkDerivation ({
            enableLibraryProfiling = true;
            enableExecutableProfiling = true;
            doCheck = false;
            #doCheck = true;
            #doBenchmark = false;
            jailbreak = true;
          } // args);
           aeson = self.aeson_1_5_6_0;
          derivasjon = self.callCabal2nix "derivasjon" (gitignore ../code/derivasjon) {};
        }));
      });
    };
  };

in nixpkgs
