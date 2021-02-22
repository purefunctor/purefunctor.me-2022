{ doCheck ? false
, doMinimal ? true
}:
let
  config = import ./config.nix { inherit doCheck doMinimal; };
in
  config.nixpkgs.dockerTools.streamLayeredImage {
    name = "purefunctor-me";
    tag = "latest";
    contents = config.nixpkgs.haskell.packages."${config.compiler}".purefunctor-me;
  }
