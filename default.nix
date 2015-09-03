{ mkDerivation, base, hedis, random, servant, servant-lucid
, servant-server, stdenv
}:
mkDerivation {
  pname = "harel";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base hedis random servant servant-lucid servant-server
  ];
  homepage = "http://github.com/jtanguy/harel";
  description = "Haskell + Redis Url shortener powered by servant)";
  license = stdenv.lib.licenses.bsd3;
}
