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

  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP, pre-commit-hooks }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
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
        overlays = [
          haskellNix.overlay
          (final: prev: {

            # This overlay adds our project to pkgs
            ply =
              final.haskell-nix.cabalProject' {
                src = ./.;
                compiler-nix-name = "ghc925";
                shell = {
                  # This is used by `nix develop .` to open a shell for use with
                  # `cabal`, `hlint` and `haskell-language-server` etc
                  tools = {
                    cabal = { };
                    hlint = { };
                    haskell-language-server = { };
                    fourmolu = { };
                    cabal-fmt = { };
                  };
                  # Non-Haskell shell tools go here
                  buildInputs = with pkgs; [
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
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.ply.flake {
          # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
          # crossPlatforms = p: [p.ghcjs];
        };
      in
      flake // {
        checks = flake.checks // { formatting-checks = pre-commit-check; };
      });
}
