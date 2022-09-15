{
  description = "astrid.tech site";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    naersk.url = "github:nix-community/naersk/master";
    nixpkgs-mozilla.url = "github:mozilla/nixpkgs-mozilla";
  };

  outputs = { self, flake-utils, nixpkgs, haskellNix, naersk, nixpkgs-mozilla
    , ... }@inputs:
    {
      overlay = (final: prev:
        let
          nightlyRust = (final.rustChannelOf {
            date = "2022-09-14";
            channel = "nightly";
            sha256 = "sha256-IHFlsy4Dzr1HvQyKCL9SQZ01FC0Syf/NCrHkf/ryOqo=";
          }).rust;
        in {
          at-upload-cli' = final.haskell-nix.project' {
            src = ./upload-cli;
            compiler-nix-name = "ghc924";
          };

          at-upload-cli = (final.at-upload-cli'.flake
            { }).packages."upload-cli:exe:upload-cli-app";

          cargo = nightlyRust;
          rustc = nightlyRust;

          at-cms = final.naersk.buildPackage { src = ./.; };
        });
    } // (flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays =
            [ haskellNix.overlay nixpkgs-mozilla.overlays.rust self.overlay ];
        };
        lib = pkgs.lib;
      in rec {
        packages = { inherit (pkgs) at-upload-cli at-cms; };

        devShells.default = devShells.legacy;

        devShells.legacy = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            cargo
            curl
            openssl
            docker
            pkg-config
            docker-compose
            git
            nodejs
            pipenv
            python310
            rustc
            yarn
          ];
        };

        devShells.content = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            at-upload-cli
            nodePackages.prettier
          ];
        };

        devShells.upload-cli = pkgs.at-upload-cli'.shellFor {
          withHoogle = true;

          tools = {
            cabal = "latest";
            hlint =
              "latest"; # Selects the latest version in the hackage.nix snapshot
            hindent = "latest";
            haskell-language-server = "latest";
            hpack = "latest";
          };

          buildInputs = with pkgs; [ nodePackages.prettier ];

          LD_LIBRARY_PATH = lib.makeLibraryPath [ pkgs.zlib ];
        };
      }));
}
