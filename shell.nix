{ doCheck ? false 
, doMinimal ? false
}:

let

  config = import ./config.nix { inherit doCheck doMinimal; };
  inherit (config) compiler nixpkgs easy-purescript-nix ;

  haskellPackages = nixpkgs.haskell.packages.${compiler};

in
  haskellPackages.shellFor {
    name = "development-shell";
    packages = _: [
      haskellPackages.purefunctor-me
    ];
    buildInputs = [
      nixpkgs.cabal-install
      easy-purescript-nix.purs
      easy-purescript-nix.spago
      easy-purescript-nix.zephyr
      nixpkgs.nodejs-15_x
    ];
  }
