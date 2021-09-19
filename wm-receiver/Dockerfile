### Builder image
FROM ghcr.io/rust-lang/rust:nightly-bullseye-slim as builder

# Install OS libraries
RUN apt-get update && apt-get install -y libssl-dev libsqlite3-dev pkg-config

# Create project directory for downloading dependencies
WORKDIR /
RUN USER=root cargo new --bin app --name wm-receiver

# Install dependencies
WORKDIR /app
COPY ./Cargo.toml ./Cargo.lock ./
# Workaround: https://github.com/rust-lang/cargo/issues/6513#issuecomment-920920238
RUN CARGO_HTTP_MULTIPLEXING=false cargo build --release

# Build the app
RUN rm src/*.rs
ADD ./src ./src
ADD ./migrations ./migrations
RUN rm ./target/release/deps/wm_receiver*
# Workaround: https://github.com/rust-lang/cargo/issues/6513#issuecomment-920920238
RUN CARGO_HTTP_MULTIPLEXING=false WEBMENTION_RESOURCES_DIR=/usr/local/share/wm-receiver/ cargo build --release

EXPOSE 8000

# Default runner for builder (for debugging only)
CMD cargo run --release

### Create a runner image with only the binary
FROM debian:bullseye-slim as runner

RUN apt-get update && apt-get install -y git libsqlite3-0

COPY --from=builder /app/target/release/wm-receiver /usr/local/bin/
COPY ./resources/main /usr/local/share/wm-receiver/

WORKDIR /app
COPY ./entrypoint.sh /app/

EXPOSE 8000
CMD ./entrypoint.sh
