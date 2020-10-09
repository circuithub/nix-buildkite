let pkgs = import ./nix/pkgs.nix;
in pkgs.haskellPackages.nix-buildkite.env
