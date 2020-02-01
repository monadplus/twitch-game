{
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        twitch-game =
          pkgs.haskell.lib.justStaticExecutables (
            haskellPackagesNew.callPackage ./default.nix { }
          );
      };
    };
  };
}
