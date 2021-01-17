{ compiler ? "ghc884" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              haskell-project-boilerplate =
                haskellPackagesNew.callPackage ./project.nix {  };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  { haskell-project-boilerplate = pkgs.haskell.packages.${compiler}.haskell-project-boilerplate;
  }
