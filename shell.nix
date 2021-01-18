{  }:

let

  config = import ./config.nix {  };

  haskellPackages = config.nixpkgs.haskell.packages.${config.compiler};

  project = haskellPackages.callPackage ./project.nix { };

in

  if config.nixpkgs.lib.inNixShell then project.env else project
