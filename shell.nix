{ doCheck ? false 
, doMinimal ? false
, forHsDev ? true
, forPsDev ? true
, forBuild ? false
}:

let
  config = import ./nix/truths.nix { inherit doCheck doMinimal; };
  inherit (config) compiler nixpkgs hsPkgs pursPkgs;

  sources = import ./nix/sources.nix { };

  gitignore = import sources."gitignore.nix" { };
  inherit (gitignore) gitignoreSource;

  name = "development-shell";

  backendPkgs = [
    nixpkgs.cabal-install
  ];

  frontendPkgs = [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    nixpkgs.nodejs-15_x
    nixpkgs.yarn
  ];

  buildPkgs = [
    nixpkgs.bash nixpkgs.git
  ];

  buildInputs = [
    ( if forHsDev then backendPkgs  else [ ] )
    ( if forPsDev then frontendPkgs else [ ] )
    ( if forBuild then buildPkgs    else [ ] )
  ];

in
  if forHsDev
  then hsPkgs.shellFor {
    inherit name buildInputs;
    packages = _: [
      hsPkgs.purefunctor-me
    ];
  }
  else nixpkgs.stdenv.mkDerivation {
    inherit name buildInputs;
    src = gitignoreSource ./.;
  }
