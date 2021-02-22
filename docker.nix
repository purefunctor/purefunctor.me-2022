{ doCheck ? false
, doMinimal ? true
}:
let
  config = import ./config.nix { inherit doCheck doMinimal; };
  inherit (config) nixpkgs hsPkgs;
in
  nixpkgs.dockerTools.streamLayeredImage {
    name = "site-backend";
    tag = "latest";
    contents = hsPkgs.purefunctor-me;
  }
