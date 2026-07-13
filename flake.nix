{
  description = "Hydra Registry API — REST service for querying Hydra L2 UTxO state";

  inputs = {
    # Follow hydra's nixpkgs so every package in the dev shell shares one
    # glibc with the GHC toolchain from hydra's cabalOnly shell. A separate
    # nixpkgs pin ends in linker errors like "version `GLIBC_2.42' not found"
    # when hsc2hs/TH code links shell libraries (e.g. libpq) built against a
    # newer nixpkgs.
    nixpkgs.follows = "hydra/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    cardano-node.url = "github:IntersectMBO/cardano-node/11.0.1";
    hydra.url = "github:cardano-scaling/hydra/2.2.0";
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cardano-scaling.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cardano-scaling.cachix.org-1:QNK4nFrowZ/aIJMCBsE35m+O70fV6eewsBNdQnCSMKA="
    ];
    allow-import-from-derivation = true;
  };

  outputs = { self, nixpkgs, flake-utils, cardano-node, hydra }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        # The haskell.nix GHC in hydra's cabalOnly shell was built against
        # the glibc of nixos-24.11 (2.40). Any C library that our build
        # links via hsc2hs/TH (libpq!) must come from the same glibc
        # generation, or the probe binaries fail to load with
        # "version `GLIBC_2.42' not found". hydra pins nixos-24.11 as
        # nixpkgs-2411 for the same reason.
        pkgs2411 = import hydra.inputs.nixpkgs-2411 { inherit system; };
      in
      {
        # Extend hydra's cabalOnly shell with registry-specific tools.
        # This inherits all Cardano C library dependencies (libsodium-vrf,
        # libblst, librust_accumulator, lmdb, liburing, …) without having to
        # enumerate them ourselves.
        devShells.default = hydra.devShells.${system}.cabalOnly.overrideAttrs (old: {
          buildInputs = (old.buildInputs or []) ++ [
            # Database — from nixpkgs-2411 so libpq matches the glibc of
            # the GHC toolchain (see pkgs2411 above). postgresql_17 keeps
            # dev data dirs initialized under postgres 17 compatible.
            pkgs2411.postgresql_17
            pkgs2411.pgcli
            pkgs2411.pgformatter

            # Hydra runtime binaries (already in hydra cabalOnly, but
            # cardano-cli and cardano-node come from cardano-node flake)
            cardano-node.packages.${system}.cardano-cli
            cardano-node.packages.${system}.cardano-node
            hydra.packages.${system}.hydra-node
            hydra.packages.${system}.hydra-tui

            # Node / frontend
            pkgs.nodejs

            # Networking & debugging
            pkgs.websocat
            pkgs.curl
            pkgs.jq
            pkgs.httpie
          ];

          shellHook = ''
            ${old.shellHook or ""}
            echo "Hydra Registry API dev shell (GHC $(ghc --version | grep -oP '\d+\.\d+\.\d+'))"
            echo ""
            echo "Commands:"
            echo "  cabal build        - build the project"
            echo "  cabal run          - run the server"
            echo "  cabal test         - run tests"
            echo "  ./dev.sh           - start full dev environment"
            echo "  ./testnet/run.sh   - start cardano-node + hydra heads for e2e testing"
            echo ""
          '';
        });
      });
}
