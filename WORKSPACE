load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_rust",
    sha256 = "82b30cbb46c61a9014de0a8d0443a45e6eb6bd9add35ab421cfb1943dc3271f5",
    strip_prefix = "rules_rust-e589105b4e8181dd1d0d8ccaa0cf3267efb06e86",
    urls = [
        # `main` branch as of 2021-08-23
        "https://github.com/bazelbuild/rules_rust/archive/e589105b4e8181dd1d0d8ccaa0cf3267efb06e86.tar.gz",
    ],
)

load("@rules_rust//rust:repositories.bzl", "rust_repositories")

rust_repositories()
