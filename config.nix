{  }:

let
  compiler = "ghc884";

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              purefunctor-me =
                haskellPackagesNew.callPackage ./nix/purefunctor-me.nix {  };
            };
          };
        };
      };
    };
  };
in
  { compiler = compiler;
    nixpkgs = import <nixpkgs> { inherit config; };
  }
