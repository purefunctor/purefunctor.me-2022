{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, bytestring, cookie, cron
      , hspec, hspec-wai, http-types, jose, lens, lib, monad-logger, mtl
      , password, persistent, persistent-sqlite, persistent-template, req
      , servant, servant-auth, servant-auth-server, servant-server, text
      , time, tomland, transformers, wai, wai-extra
      , wai-middleware-static, warp
      }:
      mkDerivation {
        pname = "purefunctor-me";
        version = "1.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson async base cron jose lens monad-logger mtl password
          persistent persistent-sqlite persistent-template req servant
          servant-auth servant-auth-server servant-server text time tomland
          transformers wai wai-middleware-static warp
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          aeson base bytestring cookie hspec hspec-wai http-types lens
          persistent persistent-sqlite text time wai wai-extra
        ];
        homepage = "https://github.com/PureFunctor/purefunctor.me";
        description = "My personal portfolio website written in PureScript and Haskell";
        license = lib.licenses.bsd3;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
        ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
