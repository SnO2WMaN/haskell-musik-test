{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };
  # dev
  inputs = {
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    yamlfmt.url = "github:SnO2WMaN/yamlfmt.nix";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs: (
    flake-utils.lib.eachDefaultSystem (
      system: let
        inherit (pkgs) lib;
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = with inputs; [
            devshell.overlay
            (final: prev: {
              yamlfmt = yamlfmt.packages.${system}.default;
            })
          ];
        };
      in {
        packages.default = pkgs.haskellPackages.callCabal2nix "haskell-musik" ./. {};

        apps.default = flake-utils.lib.mkApp {
          drv = self.packages.${system}.default;
        };

        devShells.default = pkgs.devshell.mkShell {
          packages =
            (
              with pkgs; [
                treefmt
                alejandra
                taplo-cli
                yamlfmt
                ffmpeg
              ]
            )
            ++ (
              with pkgs.haskellPackages; [
                ghc
                cabal-fmt
                cabal-install
                haskell-language-server
                ghcid
                fourmolu
              ]
            );
        };
      }
    )
  );
}
