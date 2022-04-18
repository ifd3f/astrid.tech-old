{
  description = "astrid.tech site";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, flake-utils, nixpkgs, ... }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" ] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShell = import ./shell.nix { inherit pkgs; };
      packages.seams = pkgs.callPackage ./seams {};
    });
}
