#! /bin/sh
NIXPKGS_ALLOW_BROKEN=1 nix-shell -A 'shell' ./nix/default.nix "$@"
