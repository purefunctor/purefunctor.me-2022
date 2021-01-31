{  }:

let
  compiler = "ghc884";

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
                  purefunctor-me = super.callCabal2nix "purefunctor-me" ./. { };

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
  nixpkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/987b80a824261d7bdbb14a46dc8b3814689da56e.tar.gz") { inherit config; };
  }
