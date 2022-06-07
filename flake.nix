{
  description = "Ply - A helper library for working with compiled, parameterized Plutus Scripts";

  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    plutarch = {
      url = "github:Plutonomicon/plutarch";
      inputs = {
        haskell-nix.follows = "haskell-nix";
        nixpkgs.follows = "nixpkgs";
      };
    };

    plutus = {
      url =
        "github:input-output-hk/plutus/892d9b03a67e3b9f8c452784ab4e758ff3eb2781";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/0b1b5b37e305c4bb10791f843bc8c81686a0cba4";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/07397f0e50da97eaa0575d93bee7ac4b2b2576ec";
      flake = false;
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/fd773f7a58412131512b9f694ab95653ac430852";
      flake = false;
    };
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, iohk-nix, plutarch, ... }:
    let
      extraSources = [
        {
          src = inputs.cardano-prelude;
          subdirs = [
            "cardano-prelude"
          ];
        }
        {
          src = inputs.cardano-crypto;
          subdirs = [ "." ];
        }
        {
          src = inputs.flat;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-base;
          subdirs = [
            "binary"
            "cardano-crypto-class"
          ];
        }
        {
          src = inputs.plutus;
          subdirs = [
            "plutus-core"
            "plutus-ledger-api"
            "plutus-tx"
            "prettyprinter-configurable"
            "word-array"
          ];
        }
      ];

      # GENERAL
      supportedSystems = with nixpkgs.lib.systems.supported;
        tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            (import "${iohk-nix}/overlays/crypto")
          ];
          inherit (haskell-nix) config;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      mkDevEnv = system:
        # Generic environment bringing generic utilities. To be used only as a
        # shell. Include as a dependency to other shells to have the same
        # utilities in the shell.
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.stdenv.mkDerivation {
          name = "Standard-Dev-Environment-with-Utils";
          buildInputs = [
            pkgs'.bashInteractive
            pkgs'.cabal-install
            pkgs'.fd
            pkgs'.git
            pkgs'.gnumake
            pkgs'.haskellPackages.apply-refact
            pkgs'.haskellPackages.cabal-fmt
            pkgs'.haskellPackages.fourmolu
            pkgs'.hlint
            pkgs'.nixpkgs-fmt
          ];
          shellHook = "echo $name";
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          stdDevEnv = mkDevEnv system;
        in
        pkgs.runCommand "format-check"
          {
            buildInputs = stdDevEnv.buildInputs;
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make  format_check
          mkdir $out
        '';

      # Ply core
      ply-core = rec {
        ghcVersion = "ghc8107";

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
            stdDevEnv = mkDevEnv system;
          in
          (nixpkgsFor system).haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = ghcVersion;
            cabalProjectFileName = "cabal.project.core";
            cabalProjectLocal = ''
              allow-newer: size-based:template-haskell
            '';
            inherit extraSources;
            modules = [
              ({ pkgs, ... }:
                {
                  packages = {
                    cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                    cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                  };
                }
              )
            ];
            shell = {
              withHoogle = true;

              exactDeps = true;

              buildInputs = stdDevEnv.buildInputs;

              tools.haskell-language-server = { };

              additional = ps: [
                ps.plutus-ledger-api
              ];

              shellHook = ''
                export NIX_SHELL_TARGET="core"
                ln -fs cabal.project.core cabal.project
              '';
            };
          };
      };

      # Ply x Plutarch
      ply-plutarch = rec {
        ghcVersion = "ghc921";

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
            stdDevEnv = mkDevEnv system;
          in
          (nixpkgsFor system).haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = ghcVersion;
            cabalProjectFileName = "cabal.project.plutarch";
            inherit (plutarch) cabalProjectLocal;
            extraSources = plutarch.extraSources ++ [
              {
                src = inputs.plutarch;
                subdirs = [
                  "."
                ];
              }
            ];
            modules = [ (plutarch.haskellModule system) ];
            shell = {
              withHoogle = true;

              exactDeps = true;

              buildInputs = stdDevEnv.buildInputs;

              tools = removeAttrs plutarch.tools [ "fourmolu" ];

              additional = ps: [
                ps.plutarch
                ps.plutus-ledger-api
              ];

              shellHook = ''
                export NIX_SHELL_TARGET="plutarch"
                ln -fs cabal.project.plutarch cabal.project
              '';
            };
          };
      };

    in
    {
      inherit nixpkgsFor;

      ply-core = {
        project = perSystem ply-core.projectFor;
        flake = perSystem (system: (ply-core.projectFor system).flake { });
      };

      ply-plutarch = {
        project = perSystem ply-plutarch.projectFor;
        flake = perSystem (system: (ply-plutarch.projectFor system).flake { });
      };

      build-all = perSystem (system:
        (nixpkgsFor system).runCommand "build-all"
          (self.ply-core.flake.${system}.packages // self.ply-plutarch.flake.${system}.packages)
          "touch $out");

      test-core = perSystem (system:
        let pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "test-core"
          (pkgs.lib.attrsets.getAttrs
            [ "ply-core:test:ply-core-test" ]
            self.ply-core.flake.${system}.checks) "touch $out");

      test-plutarch = perSystem (system:
        let pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "test-plutarch"
          (pkgs.lib.attrsets.getAttrs
            [ "ply-plutarch:test:ply-plutarch-test" ]
            self.ply-plutarch.flake.${system}.checks) "touch $out");

      packages = perSystem
        (system:
          self.ply-core.flake.${system}.packages //
          self.ply-plutarch.flake.${system}.packages //
          { devEnv = mkDevEnv system; }
        );

      checks = perSystem (system:
        self.ply-core.flake.${system}.checks // self.ply-plutarch.flake.${system}.checks
        // (formatCheckFor system));

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system} ++ [
              self.devShells.${system}.ply-core.inputDerivation
            ];
          } ''
          echo $checksss
          touch $out
        '');

      apps = perSystem (system: self.ply-core.flake.${system}.apps // self.ply-plutarch.flake.${system}.apps);

      devShells = perSystem (system: {
        core = self.ply-core.flake.${system}.devShell;
        plutarch = self.ply-plutarch.flake.${system}.devShell;
        devEnv = self.packages.${system}.devEnv;
      });
    };
}
