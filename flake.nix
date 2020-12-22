{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = spdx-license-dev.envFunc { withHoogle = true; };
            defaultPackage = spdx-license;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          spdx-license = hpkgs.callCabal2nix "spdx-license" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit spdx-license;
            spdx-license-dev = addBuildTools spdx-license [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
