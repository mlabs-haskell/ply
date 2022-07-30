{
  description = "Ply example";

  inputs = {
    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";

    plutip.url = "github:mlabs-haskell/plutip";

    plutarch.url = "github:Plutonomicon/plutarch?ref=staging";

    ply.url = "../.";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutarch, plutip, ... }:
    let
      # GENERAL
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${plutip.inputs.iohk-nix}/overlays/crypto") ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      deferPluginErrors = true;

      # ONCHAIN / Plutarch

      onchain = rec {
        ghcVersion = "923";
        compiler-nix-name = "ghc${ghcVersion}";

        projectFor = system:
          let
            pkgs = import plutarch.inputs.nixpkgs {
              inherit system;
              inherit (plutarch.inputs.haskell-nix) config;
              overlays = [
                plutarch.inputs.haskell-nix.overlay
                (import "${plutarch.inputs.iohk-nix}/overlays/crypto")
              ];
            };
            pkgs' = nixpkgsFor' system;
            hls = (plutarch.hlsFor compiler-nix-name system);
            myPlutarchHackages = plutarch.inputs.haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name [
              "${inputs.plutarch}"
              "${inputs.ply}/ply-core"
              "${inputs.ply}/ply-plutarch"
            ];
          in
          pkgs.haskell-nix.cabalProject' (plutarch.applyPlutarchDep pkgs {
            inherit compiler-nix-name;
            src = ./onchain;
            index-state = "2022-06-01T00:00:00Z";
            inherit (myPlutarchHackages) extra-hackages extra-hackage-tarballs modules;
            shell = {
              withHoogle = true;

              exactDeps = true;

              # We use the ones from Nixpkgs, since they are cached reliably.
              # Eventually we will probably want to build these with haskell.nix.
              nativeBuildInputs = [
                pkgs'.cabal-install
                pkgs'.fd
                pkgs'.haskellPackages.apply-refact
                pkgs'.haskellPackages.cabal-fmt
                pkgs'.hlint
                pkgs'.nixpkgs-fmt
                pkgs'.haskellPackages.fourmolu
                hls
              ];
            };
          });
      };

      # OFFCHAIN / Testnet, Cardano, ...

      offchain = rec {
        ghcVersion = "ghc8107";

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
            plutipin = inputs.plutip.inputs;
            project = pkgs.haskell-nix.cabalProject' {
              src = ./offchain;
              compiler-nix-name = ghcVersion;
              inherit (plutip) cabalProjectLocal;
              extraSources = plutip.extraSources ++ [
                {
                  src = "${plutip}";
                  subdirs = [ "." ];
                }
                {
                  src = inputs.ply;
                  subdirs = [ "ply-core" ];
                }
              ];
              modules = [
                ({ config, ... }: {
                  packages.template-project-offchain.components.tests.template-project-offchain-test.build-tools = [
                    project.hsPkgs.cardano-cli.components.exes.cardano-cli
                    project.hsPkgs.cardano-node.components.exes.cardano-node
                  ];

                })
              ] ++ plutip.haskellModules;

              shell = {
                withHoogle = true;

                exactDeps = true;

                # We use the ones from Nixpkgs, since they are cached reliably.
                # Eventually we will probably want to build these with haskell.nix.
                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.fd
                  pkgs'.haskellPackages.apply-refact
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.hlint
                  pkgs'.nixpkgs-fmt
                  pkgs'.haskellPackages.fourmolu

                  project.hsPkgs.cardano-cli.components.exes.cardano-cli
                  project.hsPkgs.cardano-node.components.exes.cardano-node
                ];

                tools.haskell-language-server = { };
              };
            };
          in
          project;
      };
    in
    {
      inherit nixpkgsFor;

      onchain = {
        project = perSystem onchain.projectFor;
        flake = perSystem (system: (onchain.projectFor system).flake { });
      };

      offchain = {
        project = perSystem offchain.projectFor;
        flake = perSystem (system: (offchain.projectFor system).flake { });
      };

      packages = perSystem (system:
        self.onchain.flake.${system}.packages
        // self.offchain.flake.${system}.packages
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.flake.${system}.devShell;
      });
    };
}
