{ mkDerivation, aeson, base, base-unicode-symbols, brick, clock
, containers, data-default, dependent-sum, either, ghc-prim
, hashable, http-api-data, http-client, http-client-tls, lens, pgdl
, QuickCheck, random, safe, semigroupoids, semigroups, servant
, servant-client, split, stdenv, text, time, transformers
, unordered-containers, vector, vty
}:
mkDerivation {
  pname = "harum";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base-unicode-symbols brick clock containers data-default
    dependent-sum either ghc-prim hashable http-api-data http-client
    http-client-tls lens pgdl QuickCheck random safe semigroupoids
    semigroups servant servant-client split text time transformers
    unordered-containers vector vty
  ];
  description = "Type-safe market computation thing";
  license = stdenv.lib.licenses.agpl3;
}
