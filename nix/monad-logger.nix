{ mkDerivation, base, bytestring, conduit, conduit-extra
, exceptions, fast-logger, lifted-base, monad-control, monad-loops
, mtl, resourcet, stdenv, stm, stm-chans, template-haskell, text
, transformers, transformers-base, transformers-compat
, unliftio-core
}:
mkDerivation {
  pname = "monad-logger";
  version = "0.3.36";
  sha256 = "706d403f37a84d87ac83b79320e18f55cf15739daf4327aac411ce17c0043c8b";
  libraryHaskellDepends = [
    base bytestring conduit conduit-extra exceptions fast-logger
    lifted-base monad-control monad-loops mtl resourcet stm stm-chans
    template-haskell text transformers transformers-base
    transformers-compat unliftio-core
  ];
  homepage = "https://github.com/snoyberg/monad-logger#readme";
  description = "A class of monads which can log messages";
  license = stdenv.lib.licenses.mit;
}
