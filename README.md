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

> This is probably not gonna work on a Windows/macOS because nixOps tries to compile
> the derivation locally if the architecture matches but delegates the compilation
> to the server if it doesn't match. The instance is small and resources like memory
> and disk are scarce so when nix tries to compile it, it end up with an OutOfMemory error.

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
ec2:root> journalctl -f -u twitch-game
```

Stop the ec2 instance and remove it (answer 'y' to the questions):

```bash
nixops destroy -d twitch-game && nixops delete -d twitch-game
```

### TODO

- [ ] Unhardcode the port
- [ ] Replace STM for MVar (performance and bugs)
- [ ] TLS on IRC connection

### Notes

*STM suffers from performance degradation as the number of concurrent users grows.

** There is possible some race conditions that I need to fix.
