let
  pkgs = import (import ./pinned-nixpkgs.nix) { inherit config; };
  config = import ./config.nix;
in
  {
    inherit (pkgs.haskellPackages) twitch-game;

    shell = (pkgs.haskellPackages.twitch-game).env;
  }
