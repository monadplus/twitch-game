{ mkDerivation, aeson, base, servant, servant-server, stdenv, text
, wai, wai-extra, warp, stm, parsec, containers, mtl, lens, async, uuid-types
, random
}:
mkDerivation {
  pname = "twitch-game";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base servant servant-server text wai wai-extra warp
    stm parsec containers mtl lens async uuid-types
    random
  ];
  homepage = "https://github.com/monadplus/twitch-game";
  description = "Twitch IRC game";
  license = stdenv.lib.licenses.bsd3;
}
