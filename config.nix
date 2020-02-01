{
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        twitch-game =
          pkgs.haskell.lib.justStaticExecutables (
            haskellPackagesNew.callPackage ./default.nix { }
          );

        network_2_6_3_1 =
          pkgs.haskell.lib.dontCheck (
            haskellPackagesOld.network_2_6_3_1
          );
      };
    };
  };
}
