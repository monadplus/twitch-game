let
  pkgs = import <nixpkgs> { config = import ./config.nix; };

in
  pkgs.haskellPackages.twitch-game.env
