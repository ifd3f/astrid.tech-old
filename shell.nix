{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
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
}
