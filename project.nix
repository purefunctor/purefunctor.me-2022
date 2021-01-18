{ mkDerivation, aeson, base, persistent, stdenv, time, yesod }:
mkDerivation {
  pname = "purefunctor-me";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base persistent time yesod ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/PureFunctor/purefunctor.me";
  description = "My personal portfolio website written in PureScript and Haskell";
  license = stdenv.lib.licenses.bsd3;
}
