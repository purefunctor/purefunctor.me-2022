{ doCheck ? false }:

let
  compiler = "ghc884";

  nixpkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/987b80a824261d7bdbb14a46dc8b3814689da56e.tar.gz") { inherit config; };

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = 
              let
                collapseOverrides = 
                  pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

                manualOverrides = self: super: {
                  # Make sure that our project has its own derivation.
                  purefunctor-me = 
                    let
                      check = if doCheck then haskell.lib.doCheck else pkgs.lib.id;
                      src = nixpkgs.nix-gitignore.gitignoreSourcePure [./.gitignore] ./.;
                    in
                      check (super.callCabal2nix "purefunctor-me" src { });

                  # Disable tests and benchmarks for all packages.
                  mkDerivation = args: super.mkDerivation ({
                    doCheck = false;
                    doBenchmark = false;
                    doHoogle = true;
                    doHaddock = true;
                    enableLibraryProfiling = false;
                    enableExecutableProfiling = false;
                  } // args);
                };
              in
                collapseOverrides [ manualOverrides ];
          };
        };
      };
    };
  };
in
  { compiler = compiler;
    nixpkgs = nixpkgs;
  }
