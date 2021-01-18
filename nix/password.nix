{ mkDerivation, base, base-compat, base64, bytestring, Cabal
, cabal-doctest, cryptonite, doctest, memory, QuickCheck
, quickcheck-instances, scrypt, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text
}:
mkDerivation {
  pname = "password";
  version = "2.1.0.0";
  sha256 = "0cd71f3ef565c99f024df6f6ef4a0abd29c8d31703110ab261809a0cbc5c10ed";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base base64 bytestring cryptonite memory template-haskell text
  ];
  testHaskellDepends = [
    base base-compat bytestring cryptonite doctest memory QuickCheck
    quickcheck-instances scrypt tasty tasty-hunit tasty-quickcheck
    template-haskell text
  ];
  homepage = "https://github.com/cdepillabout/password/password#readme";
  description = "Hashing and checking of passwords";
  license = stdenv.lib.licenses.bsd3;
}
