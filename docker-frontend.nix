let
  config = import ./config.nix { };
  inherit (config) nixpkgs;
in
  nixpkgs.dockerTools.streamLayeredImage {
    name = "site-frontend";
    tag = "latest";
    contents = import ./release-frontend.nix;
  }
