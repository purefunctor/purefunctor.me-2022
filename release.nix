{ doCheck ? false
, doMinimal ? true
}:

let
  config = import ./config.nix { inherit doCheck doMinimal; };

in
  { purefunctor-me = config.nixpkgs.haskell.packages.${config.compiler}.purefunctor-me;
  }
