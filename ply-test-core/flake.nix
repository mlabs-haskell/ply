{
  description = "Ply tests";

  inputs = {
    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";

    plutip.url = "github:mlabs-haskell/plutip";

    plutarch.url = "github:Plutonomicon/plutarch";
    plutarch.inputs.haskell-nix.follows = "plutip/haskell-nix";
    plutarch.inputs.nixpkgs.follows = "plutip/nixpkgs";

    plutus-extra = {
      url = "github:Liqwid-Labs/plutus-extra";
      inputs = {
        haskell-nix.follows = "plutip/haskell-nix";
        nixpkgs.follows = "plutip/nixpkgs";
      };
    };

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

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc8107";
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
              {
                src = inputs.plutus-extra;
                subdirs = [ "quickcheck-plutus-instances" ];
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
                ps.quickcheck-plutus-instances
                ps.ply-core
              ];
            };
          };
        in
        project;
    in
    {
      inherit nixpkgsFor;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      checks = perSystem (system:
        self.flake.${system}.checks
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
              ++ [self.devShell.inputDerivation];
          } ''
          echo $checksss
          touch $out
        ''
      );

      packages = perSystem (system: self.flake.${system}.packages);
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
