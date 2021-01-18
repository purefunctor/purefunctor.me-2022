{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, persistent, stdenv, yesod }:
      mkDerivation {
        pname = "purefunctor-me";
        version = "1.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ aeson base persistent yesod ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/PureFunctor/purefunctor.me";
        description = "My personal portfolio website written in PureScript and Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
