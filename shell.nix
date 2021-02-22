{ doCheck ? false 
, doMinimal ? false
}:

let

  config = import ./config.nix { inherit doCheck doMinimal; };
  inherit (config) compiler nixpkgs easy-purescript-nix spago2nix;

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
      spago2nix
      easy-purescript-nix.zephyr
      nixpkgs.nodejs-15_x
      nixpkgs.yarn
    ];
  }
