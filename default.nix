{ mkDerivation, aeson, base, base-unicode-symbols, brick
, bytestring, cabal-doctest, classy-prelude, containers
, data-default, dependent-sum, doctest, either, fuzzy, ghc-prim
, hashable, hourglass, http-api-data, http-client, http-client-tls
, lens, libmpd, microlens, microlens-platform, microlens-th
, monad-control, QuickCheck, safe, safe-exceptions, semigroupoids
, semigroups, servant, servant-client, singletons, split, stdenv
, template-haskell, text, text-zipper, time, tinylog, transformers
, unordered-containers, vector, vty, websockets, wuss, zippers
}:
mkDerivation {
  pname = "harum";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base-unicode-symbols brick bytestring cabal-doctest
    classy-prelude containers data-default dependent-sum doctest either
    fuzzy ghc-prim hashable hourglass http-api-data http-client
    http-client-tls lens libmpd microlens microlens-platform
    microlens-th monad-control QuickCheck safe safe-exceptions
    semigroupoids semigroups servant servant-client singletons split
    template-haskell text text-zipper time tinylog transformers
    unordered-containers vector vty websockets wuss zippers
  ];
  description = "Type-safe market computation thing";
  license = stdenv.lib.licenses.agpl3;
}
