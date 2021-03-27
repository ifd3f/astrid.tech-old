### Builder image
FROM rustlang/rust:nightly-buster-slim as builder

# Install OS libraries
RUN apt-get update && apt-get install -y libssl-dev pkg-config

# Create project directory for downloading dependencies
WORKDIR /
RUN USER=root cargo new --bin app --name aay-tw

# Install dependencies
WORKDIR /app
COPY ./Cargo.toml .
RUN cargo build --release

# Build the app
RUN rm src/*.rs
ADD . ./
RUN rm ./target/release/deps/aay_tw*
RUN cargo build --release

### Create a runner image with only the binary
FROM debian:buster-slim as runner

# Create a user
RUN useradd -ms /bin/bash shorty

# Prepare the binary
WORKDIR /app
COPY --from=builder /app/target/release/aay-tw /usr/local/bin

# Run as the user for security
USER shorty
CMD aay-tw