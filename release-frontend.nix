let
  config = import ./config.nix { };
  inherit (config) nixpkgs pursPkgs;
in
  nixpkgs.mkYarnPackage {
    name = "site-frontend";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
  }
