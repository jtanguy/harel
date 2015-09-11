{ mkDerivation, aeson, base, bytestring, either, hedis, mtl
, network-uri, random, servant, servant-server, stdenv, text, warp
}:
mkDerivation {
  pname = "harel";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring either hedis mtl network-uri random servant
    servant-server text warp
  ];
  homepage = "http://github.com/jtanguy/harel";
  description = "Haskell + Redis Url shortener powered by servant)";
  license = stdenv.lib.licenses.bsd3;
}
