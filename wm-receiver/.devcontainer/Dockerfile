# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.194.0/containers/rust/.devcontainer/base.Dockerfile

FROM mcr.microsoft.com/vscode/devcontainers/rust:0-1

# Rocket requires nightly rust
RUN rustup default nightly
RUN rustup component add rustfmt

# Useful CLI tools
RUN cargo install diesel_cli cargo-add

# [Optional] Uncomment this section to install additional packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>
