{ doCheck ? true }:

let
  config = import ./config.nix { inherit doCheck; };

in
  { purefunctor-me = config.nixpkgs.haskell.packages.${config.compiler}.purefunctor-me;
  }
