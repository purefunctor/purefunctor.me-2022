{ doCheck ? false 
, doMinimal ? false
}:

let
  config = import ./config.nix { inherit doCheck doMinimal; };
  inherit (config) compiler nixpkgs hsPkgs pursPkgs;

in
  hsPkgs.shellFor {
    name = "development-shell";
    packages = _: [
      hsPkgs.purefunctor-me
    ];
    buildInputs = [
      nixpkgs.cabal-install
      pursPkgs.purs
      pursPkgs.spago
      pursPkgs.spago2nix
      pursPkgs.zephyr
      nixpkgs.nodejs-15_x
      nixpkgs.yarn
    ];
  }
