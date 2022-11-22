.PHONY: hoogle checks requires_nix_shell

hoogle: requires_nix_shell
	hoogle server --local --port=8070 > /dev/null &

# Run pre-commit checks
checks: requires_nix_shell
	pre-commit run

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)
