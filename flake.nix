{
  description = "Ply - A helper library for working with compiled, parameterized Plutus Scripts";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix/da7acb2662961fd355f0a01a25bd32bf33577fa8";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, CHaP, pre-commit-hooks, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          settings = {
            ormolu.defaultExtensions = [
              "TypeApplications"
              "PatternSynonyms"
            ];
          };

          hooks = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            # Taken from CTL directly
            (final: prev: {
              easy-ps = import inputs.easy-purescript-nix { pkgs = final; };
            })
            (final: prev: {
              easy-ps = prev.easy-ps // {
                spago = prev.easy-ps.spago.overrideAttrs (_: rec {
                  version = "0.20.7";
                  src =
                    if final.stdenv.isDarwin
                    then
                      final.fetchurl
                        {
                          url = "https://github.com/purescript/spago/releases/download/${version}/macOS.tar.gz";
                          sha256 = "0s5zgz4kqglsavyh7h70zmn16vayg30alp42w3nx0zwaqkp79xla";
                        }
                    else
                      final.fetchurl {
                        url = "https://github.com/purescript/spago/releases/download/${version}/Linux.tar.gz";
                        sha256 = "0bh15dr1fg306kifqipnakv3rxab7hjfpcfzabw7vmg0gsfx8xka";
                      };
                });
              };
            })
          ];
          inherit (haskellNix) config;
        };
        ply = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc925";
          shell = {
            # This is used by `nix develop .` to open a shell for use with
            # `cabal`, `hlint` and `haskell-language-server` etc
            tools = {
              cabal = { };
              haskell-language-server = { };
            };
            # Non-Haskell shell tools go here
            buildInputs = with pkgs; [
              nixpkgs-fmt
              fd
              git
              gnumake
              easy-ps.purs-0_14_9
              nodejs-14_x
              easy-ps.purs-tidy
              easy-ps.spago
              easy-ps.pscid
              easy-ps.psa
              easy-ps.spago2nix
            ];
            shellHook = pre-commit-check.shellHook +
              ''
                echo $name
              '';
          };

          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
        };
        flake = ply.flake { };
      in
      flake // {
        checks = flake.checks // { formatting-checks = pre-commit-check; };
      });
}
