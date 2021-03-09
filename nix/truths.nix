{ doCheck ? false 
, doMinimal ? false
}:

let
  compiler = "ghc8103";

  sources = import ./sources.nix { };

  gitignore = import sources."gitignore.nix" { };
  inherit (gitignore) gitignoreFilter;

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
  
                      srcFilter = src:
                        let
                          srcIgnored = gitignoreFilter src;

                          dontIgnore = [
                            "config.toml"
                          ];
                        in
                          path: type:
                            srcIgnored path type
                              || builtins.elem (builtins.baseNameOf path) dontIgnore;

                      src = pkgs.lib.cleanSourceWith {
                        filter = srcFilter ../pf-backend;
                        src = ../pf-backend;
                        name = "purefunctor-me-src";
                      };

                      full = super.callCabal2nix "purefunctor-me" src { };

                      minimal = pkgs.haskell.lib.overrideCabal
                        ( pkgs.haskell.lib.justStaticExecutables full ) ( _: { } );
                    in
                      check (if doMinimal then minimal else full);

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

  nixpkgs = import (fetchTarball sources.nixpkgs.url) { inherit config; };
  easy-purescript-nix = import sources.easy-purescript-nix { pkgs = nixpkgs; };

  hsPkgs = nixpkgs.haskell.packages.${compiler};

  pursPkgs = {
    purs = easy-purescript-nix.purs-0_13_8;
    spago = easy-purescript-nix.spago;
    zephyr = easy-purescript-nix.zephyr;
  };

in
  { inherit compiler nixpkgs hsPkgs pursPkgs;
  }
