### Builder image
FROM ghcr.io/rust-lang/rust:nightly-bullseye-slim as builder

# Install OS libraries
RUN apt-get update && apt-get install -y libssl-dev libsqlite3-dev pkg-config

# Create project directory for downloading dependencies
WORKDIR /
RUN USER=root cargo new --bin app --name wm-receiver

# Install dependencies
WORKDIR /app
COPY ./Cargo.toml .
RUN cargo build --release

# Build the app
RUN rm src/*.rs
ADD . ./
RUN rm ./target/release/deps/wm_receiver*
RUN WEBMENTION_RESOURCES_DIR=/usr/local/share/wm-receiver/ cargo build --release

# Default runner for builder (for debugging only)
CMD cargo run --release

### Create a runner image with only the binary
FROM debian:bullseye-slim as runner

RUN apt-get update && apt-get install -y git libsqlite3-0

COPY --from=builder /app/target/release/wm-receiver /usr/local/bin/
COPY --from=builder /app/resources/main /usr/local/share/wm-receiver/

WORKDIR /app
CMD wm-receiver