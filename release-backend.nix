{ doCheck ? false
, doMinimal ? true
}:

let
  config = import ./config.nix { inherit doCheck doMinimal; };
  inherit (config) hsPkgs;

in
  { purefunctor-me = hsPkgs.purefunctor-me;
  }
