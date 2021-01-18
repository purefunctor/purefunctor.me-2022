{ mkDerivation, aeson, base, bytestring, conduit
, data-default-class, directory, fast-logger, file-embed
, monad-logger, shakespeare, stdenv, streaming-commons
, template-haskell, text, unix, unordered-containers, wai
, wai-extra, wai-logger, warp, yaml, yesod-core, yesod-form
, yesod-persistent
}:
mkDerivation {
  pname = "yesod";
  version = "1.6.1.0";
  sha256 = "dc57ea66007c40959fb4712e0269d6cb2b31f1b08833056732977b54aa2b65ca";
  libraryHaskellDepends = [
    aeson base bytestring conduit data-default-class directory
    fast-logger file-embed monad-logger shakespeare streaming-commons
    template-haskell text unix unordered-containers wai wai-extra
    wai-logger warp yaml yesod-core yesod-form yesod-persistent
  ];
  homepage = "http://www.yesodweb.com/";
  description = "Creation of type-safe, RESTful web applications";
  license = stdenv.lib.licenses.mit;
}
