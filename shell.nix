{ doCheck ? false }:

let

  config = import ./config.nix { inherit doCheck; };

  haskellPackages = config.nixpkgs.haskell.packages.${config.compiler};

in
  haskellPackages.shellFor {
    name = "development-shell";
    packages = _: [
      haskellPackages.purefunctor-me
    ];
    buildInputs = [
      config.nixpkgs.cabal-install
    ];
  }
