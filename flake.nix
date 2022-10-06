{
  description = "astrid.tech site";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    naersk.url = "github:nix-community/naersk/master";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, flake-utils, nixpkgs, haskellNix, naersk, rust-overlay, ...
    }@inputs:
    {
      overlay = (final: prev: {
        at-upload-cli' = final.haskell-nix.project' {
          src = ./upload-cli;
          compiler-nix-name = "ghc924";
        };

        at-upload-cli = (final.at-upload-cli'.flake
          { }).packages."upload-cli:exe:upload-cli-app";

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
            [ haskellNix.overlay rust-overlay.overlays.default self.overlay ];
        };
        rust-toolchain = pkgs.rust-bin.nightly."2022-09-15";
        lib = pkgs.lib;
      in rec {
        packages = { inherit (pkgs) at-upload-cli at-cms; };

        devShells.default = devShells.legacy;

        devShells.legacy = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            openssl
            docker
            pkg-config
            docker-compose
            git
            nodejs
            nodePackages.prettier
            pipenv
            python310
            yarn

            (rust-toolchain.rust.override { extensions = [ "rust-src" ]; })
            cargo-edit
            diesel-cli
          ];

          # The development database
          DATABASE_URL = "postgres://dev:password@localhost/db";
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
