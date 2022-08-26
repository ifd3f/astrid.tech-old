{
  description = "astrid.tech site";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, flake-utils, nixpkgs, ... }@inputs:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
    ] (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.seams = pkgs.callPackage ./seams { };

        devShells = {
          default = import ./shell.nix { inherit pkgs; };

          seams = pkgs.haskellPackages.shellFor {
            packages = p: [ packages.seams ];
            withHoogle = true;
            # postgresql-typed and dbmate params
            TPG_DB = "seams-db";
            TPG_USER = "postgres";
            TPG_PASS = "developer";
            TPG_HOST = "localhost";
            TPG_PORT = 5432;
            buildInputs = with pkgs; [
              haskellPackages.haskell-language-server
              postgresql
              cabal2nix
              nixfmt
              haskellPackages.hindent
              haskellPackages.hlint
              haskellPackages.cabal-install
              zlib
            ];
          };
        };
      });
}
