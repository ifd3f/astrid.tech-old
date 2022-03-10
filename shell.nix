{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    cargo
    curl
    dbmate
    docker
    docker-compose
    ghc
    git
    nodejs
    pipenv
    postgresql
    python310
    rustc
    stack
    yarn
  ];
}
