{ mkDerivation, aeson, base, servant, servant-server, stdenv, text
, wai, wai-extra, warp, stm, parsec, containers, mtl, lens, async, uuid-types
, random, network_2_6_3_1, bytestring
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
    random network_2_6_3_1 bytestring
  ];
  homepage = "https://github.com/monadplus/twitch-game";
  description = "Twitch IRC game";
  license = stdenv.lib.licenses.bsd3;
}
