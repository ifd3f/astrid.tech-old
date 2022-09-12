{
  description = "astrid.tech site";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, flake-utils, nixpkgs, ... }@inputs:
    {
      overlay = (final: prev: {
        at-upload-cli =
          final.haskellPackages.callCabal2nix "at-upload-cli" ./upload-cli { };
      });
    } // (flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
    ] (system:
    let pkgs = import nixpkgs {
      inherit system;
      overlays = [self.overlay];
    };
      in rec {
        packages = { inherit (pkgs) at-upload-cli; };

        devShells.default = devShells.legacy;

        devShells.legacy = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            cargo
            curl
            docker
            docker-compose
            git
            nodejs
            pipenv
            python310
            rustc
            yarn
          ];
        };

        devShells.upload-cli = pkgs.haskellPackages.shellFor {
          packages = p: [ pkgs.at-upload-cli ];
          withHoogle = true;
          buildInputs = with pkgs.haskellPackages; [
            haskell-language-server
            hpack
            ghcid
            cabal-install
          ];
        };
      }));
}
