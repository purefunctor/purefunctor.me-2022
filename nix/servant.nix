{ mkDerivation, aeson, attoparsec, base, base-compat, bifunctors
, bytestring, case-insensitive, deepseq, hspec, hspec-discover
, http-api-data, http-media, http-types, mmorph, mtl, network-uri
, QuickCheck, quickcheck-instances, singleton-bool, sop-core
, stdenv, string-conversions, tagged, text, transformers, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.18.2";
  sha256 = "65d697c2dafd854d47716cf84b3377d307478da7401a75e4cbe921f59394aea2";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bifunctors bytestring
    case-insensitive deepseq http-api-data http-media http-types mmorph
    mtl network-uri QuickCheck singleton-bool sop-core
    string-conversions tagged text transformers vault
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring hspec http-media mtl QuickCheck
    quickcheck-instances string-conversions text transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
