{ machine = { pkgs, ... }: {
    networking.firewall.allowedTCPPorts = [ 8080 ]; # Allow trafic to that port
                                                    # By default port 22 is concatenated
    nixpkgs.config = import ./config.nix; # Make config.nix accessible to nixOS
                                          # NixOS will use it on the script part

    # Have a look at https://www.shellhacks.com/systemd-service-file-example/
    systemd.services.twitch-game = {
      wantedBy = [ "multi-user.target" ];

      script = ''
        ${pkgs.haskellPackages.twitch-game}/bin/twitch-game
      '';
    };
  };
}
