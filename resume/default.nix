let pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    python39Packages.jinja2
    jq
    yq
    texlive.combined.scheme-full
  ];
}
