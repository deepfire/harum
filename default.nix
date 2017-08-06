{ mkDerivation, aeson, base, base-unicode-symbols, brick
, cabal-doctest, classy-prelude, containers, data-default
, dependent-sum, doctest, either, fuzzy, ghc-prim, hashable
, http-api-data, http-client, http-client-tls, lens, libmpd
, microlens, microlens-platform, microlens-th, QuickCheck, safe
, safe-exceptions, semigroupoids, semigroups, servant
, servant-client, split, stdenv, template-haskell, text
, text-zipper, time, transformers, unordered-containers, vector
, vty, zippers
}:
mkDerivation {
  pname = "harum";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base-unicode-symbols brick cabal-doctest classy-prelude
    containers data-default dependent-sum doctest either fuzzy ghc-prim
    hashable http-api-data http-client http-client-tls lens libmpd
    microlens microlens-platform microlens-th QuickCheck safe
    safe-exceptions semigroupoids semigroups servant servant-client
    split template-haskell text text-zipper time transformers
    unordered-containers vector vty zippers
  ];
  executableHaskellDepends = [
    aeson base base-unicode-symbols brick cabal-doctest classy-prelude
    containers data-default dependent-sum doctest either fuzzy ghc-prim
    hashable http-api-data http-client http-client-tls lens libmpd
    microlens microlens-platform microlens-th QuickCheck safe
    safe-exceptions semigroupoids semigroups servant servant-client
    split template-haskell text time transformers unordered-containers
    vector vty zippers
  ];
  description = "Type-safe market computation thing";
  license = stdenv.lib.licenses.agpl3;
}
