{ mkDerivation, base, stdenv, servant, servant-server, wai, warp, text, aeson, hasql, hasql-pool, hasql-transaction, random, lens, modern-uri, megaparsec, time, bytestring, hspec, http-client, servant-client}:
mkDerivation {
  pname = "feeld-urlshortener";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  doCheck = false;
  executableHaskellDepends = [ base servant servant-server wai warp text aeson hasql hasql-pool hasql-transaction random lens modern-uri megaparsec time bytestring hspec http-client servant-client];
  license = stdenv.lib.licenses.bsd3;
}
