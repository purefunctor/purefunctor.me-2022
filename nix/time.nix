{ mkDerivation, base, criterion, deepseq, QuickCheck, random
, stdenv, tasty, tasty-hunit, tasty-quickcheck, unix
}:
mkDerivation {
  pname = "time";
  version = "1.11.1.1";
  sha256 = "3775168c863821032e54e64175e3a8d1a8d4083ff041fe3e4abf4fed884c3a77";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [
    base deepseq QuickCheck random tasty tasty-hunit tasty-quickcheck
    unix
  ];
  benchmarkHaskellDepends = [ base criterion deepseq ];
  homepage = "https://github.com/haskell/time";
  description = "A time library";
  license = stdenv.lib.licenses.bsd3;
}
