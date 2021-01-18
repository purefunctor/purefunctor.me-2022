{  }:

let
  config = import ./config.nix {  };

in
  { purefunctor-me = config.nixpkgs.haskell.packages.${config.compiler}.purefunctor-me;
  }
