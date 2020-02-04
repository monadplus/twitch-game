let
  config = import ./config.nix;

  # Warning: not pinned
  pkgs = import <nixpkgs> { inherit config; };

in
  {
    inherit (pkgs.haskellPackages) twitch-game;

    shell = (pkgs.haskellPackages.twitch-game).env;
  }
