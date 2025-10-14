{
  description = "Plutarch submissions for UPLC-CAPE benchmark";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus/427e49b0cbfd2ff97cdc91522a09e06edd7a7fa6";
      flake = false;
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      haskell-nix,
      iohk-nix,
      CHaP,
      plutarch,
      treefmt-nix,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor =
        system:
        import haskell-nix.inputs.nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            iohk-nix.overlays.crypto
            iohk-nix.overlays.haskell-nix-crypto
          ];
          inherit (haskell-nix) config;
        };

      treefmtFor =
        system:
        let
          pkgs = nixpkgsFor system;
        in
        treefmt-nix.lib.mkWrapper pkgs {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.fourmolu.enable = true;
        };

      projectFor =
        system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc966";

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
          };

          cabalProjectLocal = ''
            source-repository-package
              type: git
              location: https://github.com/Plutonomicon/plutarch-plutus
              tag: 427e49b0cbfd2ff97cdc91522a09e06edd7a7fa6
              --sha256: sha256-6ZB8eCb9B8hg0msfk+RIIdbX1OrfWIWGDLYftXjBS+E=
          '';

          modules = [
            {
              packages.plutarch.src = pkgs.lib.mkForce (
                pkgs.fetchgit {
                  url = "https://github.com/Plutonomicon/plutarch-plutus";
                  rev = "427e49b0cbfd2ff97cdc91522a09e06edd7a7fa6"; # master
                  sha256 = "sha256-6ZB8eCb9B8hg0msfk+RIIdbX1OrfWIWGDLYftXjBS+E=";
                }
              );
            }
          ];

          shell = {
            withHoogle = true;

            tools = {
              cabal = "latest";
              haskell-language-server = "latest";
            };

            buildInputs =
              let
                buildScript = pkgs.writeShellScriptBin "build" ''
                  set -e

                  echo "Building all executables..."
                  cabal build all

                  echo ""
                  echo "Generating UPLC files..."
                  cabal run factorial
                  cabal run factorial-naive
                  cabal run fibonacci
                  cabal run fibonacci-naive

                  echo ""
                  echo "✓ All UPLC files generated successfully!"
                '';
              in
              with pkgs;
              [
                buildScript
                treefmt
                nixfmt-rfc-style
                haskellPackages.cabal-fmt
                haskellPackages.fourmolu
              ];

            shellHook = ''
              echo "Plutarch UPLC-CAPE benchmark submissions"
              echo "Available executables:"
              echo "  - cabal run factorial           # Optimized factorial (pfix')"
              echo "  - cabal run factorial-naive     # Naive factorial (pfix)"
              echo "  - cabal run fibonacci           # Optimized fibonacci (pfix''')"
              echo "  - cabal run fibonacci-naive     # Naive fibonacci (pfix)"
              echo ""
              echo "Quick commands:"
              echo "  - build         # Compile and run all executables to generate UPLC files"
              echo "  - treefmt       # Format all nix, cabal, and haskell files"
              echo ""
            '';
          };
        };

    in
    {
      packages = perSystem (
        system:
        let
          project = projectFor system;
        in
        {
          factorial = project.getComponent "plutarch-cape-submissions:exe:factorial";
          factorial-naive = project.getComponent "plutarch-cape-submissions:exe:factorial-naive";
          fibonacci = project.getComponent "plutarch-cape-submissions:exe:fibonacci";
          fibonacci-naive = project.getComponent "plutarch-cape-submissions:exe:fibonacci-naive";
          default = project.getComponent "plutarch-cape-submissions:exe:factorial";
        }
      );

      devShells = perSystem (
        system:
        let
          project = projectFor system;
        in
        {
          default = project.shell;
        }
      );

      apps = perSystem (system: {
        factorial = {
          type = "app";
          program = "${self.packages.${system}.factorial}/bin/factorial";
        };
        factorial-naive = {
          type = "app";
          program = "${self.packages.${system}.factorial-naive}/bin/factorial-naive";
        };
        fibonacci = {
          type = "app";
          program = "${self.packages.${system}.fibonacci}/bin/fibonacci";
        };
        fibonacci-naive = {
          type = "app";
          program = "${self.packages.${system}.fibonacci-naive}/bin/fibonacci-naive";
        };
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/factorial";
        };
      });

      formatter = perSystem (system: treefmtFor system);
    };
}
