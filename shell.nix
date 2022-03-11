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
    haskell-language-server
    haskellPackages.hlint
    haskellPackages.implicit-hie
    nodejs
    pipenv
    postgresql
    python310
    rustc
    stack
    yarn
  ];
}
