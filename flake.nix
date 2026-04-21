{
  description = "Hydra Registry API — REST service for querying Hydra L2 UTxO state";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    cardano-node.url = "github:IntersectMBO/cardano-node";
  };

  outputs = { self, nixpkgs, flake-utils, cardano-node }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskell.packages.ghc910.override {
          overrides = hself: hsuper: {
            rel8 = pkgs.haskell.lib.dontCheck hsuper.rel8;
          };
        };

        hydra-registry-api = haskellPackages.callCabal2nix "hydra-registry-api" ./api { };
      in
      {
        packages.default = hydra-registry-api;

        devShells.default = haskellPackages.shellFor {
          packages = _: [ hydra-registry-api ];

          nativeBuildInputs = [
            # Haskell tooling
            haskellPackages.cabal-install
            haskellPackages.ghc
            haskellPackages.haskell-language-server
            haskellPackages.fourmolu
            haskellPackages.cabal-fmt

            # Database
            pkgs.postgresql
            pkgs.pgcli
            pkgs.pgformatter

            # Cardano
            cardano-node.packages.${system}.cardano-cli

            # Node / frontend
            pkgs.nodejs

            # Networking & debugging
            pkgs.websocat
            pkgs.curl
            pkgs.jq
            pkgs.httpie

            # Utilities
            pkgs.pkg-config
            pkgs.zlib
          ];

          shellHook = ''
            echo "Hydra Registry API dev shell"
            echo "GHC: $(ghc --version)"
            echo ""
            echo "Commands:"
            echo "  cabal build   - build the project"
            echo "  cabal run     - run the server"
            echo "  cabal test    - run tests"
            echo ""
          '';
        };
      });
}
