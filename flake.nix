{
  description = "Clash OrderMap implementation with generic orderbook data structures";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://cache.iog.io";
    extra-trusted-public-keys = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        hpkgs = pkgs.haskellPackages;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            hpkgs.clash-ghc
            hpkgs.clash-lib
            hpkgs.clash-prelude

            (hpkgs.ghcWithPackages (hp: [
              hp.QuickCheck
            ]))

            pkgs.zlib
          ];
        };

        packages.default = hpkgs.callCabal2nix "itch-processor" ./. {};
      }
    );
}
