{
  description = "changeset";

  nixConfig = {
    extra-substituters = [
      "https://changeset.cachix.org"
    ];
    extra-trusted-public-keys = [
      "changeset.cachix.org-1:OsRJ8Eo3VifVZSF6qazmSgkkNKBaAf/yU1Qsw6ZQtRU="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    with builtins;
    with nixpkgs.lib;
    let
      inherit (nixpkgs) lib;
      projectName = "changeset";
      localPackages = {
        changeset = ./changeset;
        changeset-containers = ./changeset-containers;
        changeset-lens = ./changeset-lens;
        changeset-reflex = ./changeset-reflex;
      };

      # Always keep in sync with the tested-with section in the cabal file
      supportedGhcs = [
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        # "ghc912" # Uncomment as soon as nixpkgs is more advanced
      ];

    in
    flake-utils.lib.eachDefaultSystem
      (system:

        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Haskell package overrides for dependencies
          dependenciesOverrides = with pkgs.haskell.lib;
            composeManyExtensions [
              (hfinal: hprev: {
                reflex = dontCheck (doJailbreak hprev.reflex);
              })
            ];

          haskellPackagesFor = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.override (_: {
              overrides = dependenciesOverrides;
            }))
            (genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
            // { default = pkgs.haskellPackages; });

          # Haskell package overrides to set the definitions of the locally defined packages to the current version in this repo
          localPackagesOverrides = hfinal: hprev: with pkgs.haskell.lib;
            (mapAttrs (pname: path: hfinal.callCabal2nix pname path { }) localPackages);

          haskellPackagesExtended = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.override (haskellPackagesPrevious: {
              overrides = composeManyExtensions [
                haskellPackagesPrevious.overrides
                localPackagesOverrides
              ];
            }))
            haskellPackagesFor;

          localPackagesFor = haskellPackages: mapAttrs (pname: _path: haskellPackages.${pname}) localPackages;
          allLocalPackagesFor = ghcVersion: haskellPackages:
            pkgs.linkFarm "${projectName}-all-for-${ghcVersion}"
              (localPackagesFor haskellPackages);
          forEachGHC = mapAttrs allLocalPackagesFor haskellPackagesExtended;
          allGHCs = pkgs.linkFarm "${projectName}-all-ghcs" forEachGHC;
        in
        {
          # "packages" doesn't allow nested sets
          legacyPackages = mapAttrs
            (ghcVersion: haskellPackages: localPackagesFor haskellPackages // {
              "${projectName}-all" = allLocalPackagesFor ghcVersion haskellPackages;
            })
            haskellPackagesExtended // {
            "${projectName}-all" = forEachGHC;
          };

          packages = {
            default = allGHCs;
          };

          devShells = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.shellFor {
              packages = hps: attrValues (localPackagesFor (haskellPackagesExtended.${ghcVersion}));
              nativeBuildInputs = (
                lib.optional (versionAtLeast haskellPackages.ghc.version "9.6")
                  haskellPackages.haskell-language-server)
              ++ (with pkgs;
                [ cabal-install ]
              )
              ;
            })
            haskellPackagesFor;

          formatter = pkgs.nixpkgs-fmt;
        }) // {
      inherit supportedGhcs;
    };
}
