{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc884"
}:
let
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (pkgs: with pkgs; [base persistent yesod]);
in
  pkgs.mkShell {
    buildInputs = [ ghc ];
  }
