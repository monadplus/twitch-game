# twitch-game

# TODO

- [ ] Inbound port 8080 is not open by default on physical.nix
- [ ] Requires a much bigger instance due to the compilation of the derivation.

The server should be running on `8080`.

```
curl -X GET ec2-3-248-202-110.eu-west-1.compute.amazonaws.com:8080/hello -H "Content-Type: application/json"
```

Don't forget to add the following line after each change on cabal:

```
cabal2nix . > default.nix
```

### How to deploy?

```bash
$ nixops create --deployment twitch-game logical.nix physical.nix
$ nixops deploy --deployment twitch-game
$ nixops info   --deployment twitch-game

```

SSH Connection to the server:

```
$ nixops ssh --deployment twitch-game machine
ec2:root> systemctl status twitch-game
ec2:root> journalctl -f -u simple-ci-nix
```

### Issues on deployment

```
Only for NixOS users:
To set this option: https://nixos.org/nixos/options.html#distributed
- nix.distributedBuilds to True
- nix.buildMachines  set all fields

If you are not using nixos:
- Not well explained lol

I solved this adding a bigger disk: deployment.ec2.ebsInitialRootDiskSize = 20;
and also deploy to t2.small which about memory overflow on ghc linking.
```
