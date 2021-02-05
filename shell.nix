{ doCheck ? false }:

let

  config = import ./config.nix { inherit doCheck; };

  haskellPackages = config.nixpkgs.haskell.packages.${config.compiler};

  project = haskellPackages.purefunctor-me;

in

  if config.nixpkgs.lib.inNixShell then project.env else project
