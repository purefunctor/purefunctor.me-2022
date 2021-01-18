{  }:

let
  compiler = "ghc884";

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: _:
              let
                toPackage = file: _: {
                  name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                  value = haskell.lib.dontCheck (haskellPackagesNew.callPackage (./. + "/nix/${file}") {  });
                };
              in
                pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);
          };
        };
      };
    };
  };
in
  { compiler = compiler;
    nixpkgs = import <nixpkgs> { inherit config; };
  }
