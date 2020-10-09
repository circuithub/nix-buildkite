let pkgs = import ./nix/pkgs.nix;
in {
  nix-buildkite = pkgs.haskellPackages.nix-buildkite;
}
