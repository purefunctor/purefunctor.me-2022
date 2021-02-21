{ doCheck ? false 
, doMinimal ? false
}:

let
  compiler = "ghc884";

  sources = import ./nix/sources.nix { };

  gitignore = import sources."gitignore.nix" { };
  inherit (gitignore) gitignoreFilter;

  nixpkgs = import (fetchTarball sources.nixpkgs.url) { };
  hsStaticPkgs = nixpkgs.pkgsMusl.haskell.packages."${compiler}".extend (
    let
      pkgs = nixpkgs.pkgsMusl;

      collapseOverrides = 
        pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

      manualOverrides = self: super: {
        # Make sure that our project has its own derivation.
        purefunctor-me = 
          let
            check = if doCheck then pkgs.haskell.lib.doCheck else pkgs.lib.id;
  
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
              filter = srcFilter ./.;
              src = ./.;
              name = "purefunctor-me-src";
            };

            full = super.callCabal2nix "purefunctor-me" src { };

            minimal = pkgs.haskell.lib.overrideCabal
            full
            ( _: { 
              enableSharedExecutables = false;
              enableSharedLibraries = false;
              configureFlags = [
                "--ghc-option=-optl=-static"
                "--ghc-option=-optl=-pthread"
                "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
                "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
                "--ghc-option=-optl=-L${pkgs.libffi.overrideAttrs ( _ : { dontDisableStatic = true; } )}"
                "--ghc-option=-optl=-L${pkgs.libidn2.overrideAttrs ( _ : { dontDisableStatic = true; } )}"
                "--ghc-option=-optl=-L${pkgs.libunistring.overrideAttrs ( _ : { dontDisableStatic = true; } )}"
                "--ghc-option=-optl=-L${pkgs.sqlite.overrideAttrs ( _ : { dontDisableStatic = true; } )}"
                "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
              ];
            } );

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
      collapseOverrides [ manualOverrides ] );

  easy-purescript-nix = import sources.easy-purescript-nix { pkgs = nixpkgs; };
in
  { inherit compiler nixpkgs hsStaticPkgs easy-purescript-nix;
  }
