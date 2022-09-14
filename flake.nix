{
  description = "astrid.tech site";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, flake-utils, nixpkgs, haskellNix, ... }@inputs:
    {
      overlay = (final: prev: {
        at-upload-cli' = final.haskell-nix.project' {
          src = ./upload-cli;
          compiler-nix-name = "ghc924";
          shell.tools = {
            cabal = { };
            hpack = { };
            hlint = { };
            haskell-language-server = { };
          };
          # Non-Haskell shell tools go here
          shell.buildInputs = with final; [ nixpkgs-fmt ];
        };

        at-upload-cli = (final.at-upload-cli'.flake {}).packages."upload-cli:exe:upload-cli-app";
      });
    } // (flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay self.overlay ];
        };
        lib = pkgs.lib;
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

        devShells.upload-cli = pkgs.at-upload-cli'.shellFor {
          withHoogle = true;
          
          tools = {
            cabal = "latest";
            hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
            hindent = "latest";
            haskell-language-server = "latest";
            hpack = "latest";
          };

          LD_LIBRARY_PATH = lib.makeLibraryPath [ pkgs.zlib ];
        };
      }));
}
