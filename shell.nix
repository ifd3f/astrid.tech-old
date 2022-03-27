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
    zlib
  ];

  # postgresql-typed and dbmate params
  TPG_DB = "seams-db";
  TPG_USER = "postgres";
  TPG_PASS = "developer";
  TPG_HOST = "localhost";
  TPG_PORT = 5432;
  DATABASE_URL = "postgres://postgres:developer@localhost:5432/seams-dev?sslmode=disable";
}
