{
  description = "Ply example";

  inputs = {
    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";

    plutip.url = "github:mlabs-haskell/plutip";

    plutarch.url = "github:Plutonomicon/plutarch";
    plutarch.inputs.haskell-nix.follows = "plutip/haskell-nix";
    plutarch.inputs.nixpkgs.follows = "plutip/nixpkgs";

    ply = {
      url = "../.";
      inputs = {
        haskell-nix.follows = "plutarch/haskell-nix";
        nixpkgs.follows = "plutarch/nixpkgs";
      };
    };
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

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs'.git
              pkgs'.fd
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
              pkgs'.haskellPackages.fourmolu
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check
          mkdir $out
        ''
      ;

      deferPluginErrors = true;

      # ONCHAIN / Plutarch

      onchain = rec {
        ghcVersion = "ghc921";

        projectFor = system:
          let pkgs = nixpkgsFor system; in
          let pkgs' = nixpkgsFor' system; in
          (nixpkgsFor system).haskell-nix.cabalProject' {
            src = ./onchain;
            compiler-nix-name = ghcVersion;
            inherit (plutarch) cabalProjectLocal;
            extraSources = plutarch.extraSources ++ [
              {
                src = inputs.plutarch;
                subdirs = [ "." ];
              }
              {
                src = inputs.ply;
                subdirs = [ "ply-core" "ply-plutarch" ];
              }
            ];
            modules = [ (plutarch.haskellModule system) ];
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
              ];

              tools = removeAttrs plutarch.tools [ "fourmolu" ];

              additional = ps: [
                ps.plutarch
                ps.ply-core
                ps.ply-plutarch
              ];
            };
          };
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

                additional = ps: [
                  ps.plutip
                  ps.ply-core
                ];
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
      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // self.offchain.flake.${system}.checks
        // {
          formatCheck = formatCheckFor system;
        }
      );
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system}
              ++ [
                self.devShells.${system}.onchain.inputDerivation
                self.devShells.${system}.offchain.inputDerivation
              ];
          } ''
          echo $checksss
          touch $out
        ''
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.flake.${system}.devShell;
      });
    };
}
