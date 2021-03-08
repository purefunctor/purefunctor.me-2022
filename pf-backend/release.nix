{ doCheck ? false
, doMinimal ? true
}:

let
  config = import ../nix/truths.nix { inherit doCheck doMinimal; };
  inherit (config) hsPkgs;

in
  { purefunctor-me = hsPkgs.purefunctor-me;
  }
