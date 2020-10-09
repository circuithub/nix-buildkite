let
  pkgs = import ./nix/pkgs.nix;
  nix-buildkite = pkgs.haskellPackages.nix-buildkite;
in
pkgs.writeScript "nix-buildkite" "${nix-buildkite}/bin/nix-buildkite"
