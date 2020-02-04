# Otter Chaos Repair: twitch chat game 👾
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

This is the back end for the online twitch-chat game [otter chaos](https://github.com/sigr3s/Otter-Chaos).

This game was designed, built and deployed in 24 hours by [Sergi Tortosa](https://github.com/sigr3s), [Adria Abella]() and [me](https://github.com/monadplus) for the [Global Game Jam 2020](https://globalgamejam.org/).

![Screenshot](./screenshot.png)

Demo:

[![Youtube demo](http://img.youtube.com/vi/oyHgUOYpKmQ/0.jpg)](http://www.youtube.com/watch?v=oyHgUOYpKmQ)

### Build

This project is build using `nix` + `cabal-install`.

After installing both `nix` and `cabal-install`, you can compile the project:

```bash
$ nix-build
```

The hack the project you can use `nix-shell`:

```bash
$ nix-shell
shell> cabal v1-configure
shell> cabal v1-build
shell> cabal v1-run
```


### Deployment

This service is deploying using [nixops](https://nixos.org/nixops/).

After installing nix/nixops (google it), you only need to do the following:

1. Add your aws credentials to ~/.aws/credentials (google it)
2. Execute the following commands:

```bash
$ nixops create --deployment twitch-game logical.nix physical.nix
$ nixops deploy --deployment twitch-game
```

This will create a new ec2 machine with this service running as a systemd unit.

You can query the status of your ec2 instance:

```bash
$ nixops info --deployment twitch-game
```

SSH Connection to the server:

```zsh
$ nixops ssh --deployment twitch-game machine

# Check the status of the service
ec2:root> systemctl status twitch-game
ec2:root> journalctl -f -u simple-ci-nix
```

Stop the ec2 instance and remove it (answer 'y' to the questions):

```bash
nixops destroy -d twitch-game && nixops delete -d twitch-game
```

### TODO

- [ ] Requires a much bigger instance due to the compilation of the derivation
- [ ] Unhardcode the port
- [ ] Replace STM for MVar (performance and bugs)
- [ ] TLS on IRC connection

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

### Notes

*STM suffers from performance degradation as the number of concurrent users grows.

** There is possible some race conditions that I need to fix.
