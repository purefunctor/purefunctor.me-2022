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

                autoOverrides = self: super:
                  let
                    toPackage = file: _: {
                      name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                      value = haskell.lib.dontCheck (self.callPackage (./. + "/nix/${file}") {  });
                    };
                  in
                    pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

                manualOverrides = self: super: {
                  base64 = pkgs.haskell.lib.dontCheck pkgs.haskell.packages."${compiler}".base64;
                };
              in
                collapseOverrides [ autoOverrides manualOverrides ];
          };
        };
      };
    };
  };
in
  { compiler = compiler;
    nixpkgs = import <nixpkgs> { inherit config; };
  }
