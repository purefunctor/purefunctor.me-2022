{ doCheck ? false
, doMinimal ? true
}:

let
  config = import ./config.nix { inherit doCheck doMinimal; };

in
  { purefunctor-me = config.survey.haskellPackages.purefunctor-me;
  }
