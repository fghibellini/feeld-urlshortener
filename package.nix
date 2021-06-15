let
  pkgs = import <nixpkgs> { inherit config;  };

  config = {
      allowBroken = true;
      packageOverrides = pkgs: rec {
          haskellPackages = pkgs.haskell.packages.ghc8104.override {
              overrides = self: super: {
                  hasql-pool = pkgs.haskell.lib.dontCheck super.hasql-pool;
              };
          };
        };
  };
in
  pkgs.haskellPackages.callPackage ./default.nix { }
