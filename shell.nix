{ pkgs ? import <nixpkgs> {}, docTools ? true }:

pkgs.mkShell {
  LANG="C.UTF-8";

  buildInputs = with pkgs; [
    nix
    which
    python3
    bazel_4
    jdk11
    stack
    haskell.packages.ghc8104.ghc
    git
    glibcLocales
    binutils
    file
    gmpxx
    openlibm
    libffi
  ]; 

  shellHook = ''
    # Add nix config flags to .bazelrc.local.
    #
    BAZELRC_LOCAL=".bazelrc.local"
    if [ ! -e "$BAZELRC_LOCAL" ]
    then
      echo "[!] It looks like you are using a Nix-based system."
      echo "In order to build this project, you need to add the two"
      echo "following host_platform entries to your .bazelrc.local file:"
      echo
      echo "build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host"
      echo "run --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host"
    fi
    # source bazel bash completion
    source ${pkgs.bazel_4}/share/bash-completion/completions/bazel.bash
  '';
}