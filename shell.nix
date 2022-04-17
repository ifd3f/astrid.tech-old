{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = let
    customghc = (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      cabal-install
      zlib
    ]));
  in with pkgs; [
    cargo
    curl
    dbmate
    docker
    docker-compose
    customghc
    git
    haskell-language-server
    haskellPackages.hlint
    haskellPackages.hindent
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

  libraryHaskellDepends = with pkgs; [
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
