self:
super:

let
  inherit (self) haskell haskellPackages;

  inherit (haskellPackages) callCabal2nix;

  inherit (haskell.lib) appendConfigureFlag packagesFromDirectory;

  inherit (super.lib) composeExtensions;
  
  WError =
    drv: appendConfigureFlag drv "--ghc-option=-Werror";

  configurations =
    self: super: {
      nix-buildkite = WError (callCabal2nix "nix-buildkite" (builtins.path { path = ../../.; name = "nix-buildkite"; }) {});
    };

in
{
  haskellPackages =
    super.haskellPackages.override
      (
        old:
          {
            overrides =
              composeExtensions
                (old.overrides or (_: _: {}))
                (
                  composeExtensions
                    (packagesFromDirectory { directory = ./haskell-packages; })
                    configurations
                );
          }
      );
}
