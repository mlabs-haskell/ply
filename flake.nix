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
  };

  outputs = { nixpkgs, flake-utils, haskellNix, CHaP, pre-commit-hooks, ... }:
    let
      # NOTE: nix flake (show | check) --allow-import-from-derivation --impure
      systems =
        if builtins.hasAttr "currentSystem" builtins
        then [ builtins.currentSystem ]
        else nixpkgs.lib.systems.flakeExposed;
    in
    flake-utils.lib.eachSystem systems (system:
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
            statix.enable = true;
            deadnix.enable = true;
          };
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
          ];
          inherit (haskellNix) config;
        };

        ply = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc963";
          shell = {
            # This is used by `nix develop .` to open a shell for use with
            # `cabal`, `hlint` and `haskell-language-server` etc
            tools = {
              cabal = { };
              haskell-language-server = { };
            };
            # Non-Haskell shell tools go here
            nativeBuildInputs = with pkgs; [
              nixpkgs-fmt
              fd
              git
              gnumake
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
