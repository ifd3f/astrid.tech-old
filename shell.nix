{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    cargo
    curl
    docker
    docker-compose
    ghc
    git
    nodejs
    pipenv
    python310
    rustc
    stack
    yarn
  ];
}
