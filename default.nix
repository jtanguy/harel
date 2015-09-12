{ mkDerivation, aeson, base, bytestring, either, hedis, lucid, mtl
, network-uri, random, servant, servant-lucid, servant-server
, stdenv, text, warp
}:
mkDerivation {
  pname = "harel";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring either hedis lucid mtl network-uri random
    servant servant-lucid servant-server text warp
  ];
  homepage = "http://github.com/jtanguy/harel";
  description = "Haskell + Redis Url shortener powered by servant";
  license = stdenv.lib.licenses.bsd3;
}
