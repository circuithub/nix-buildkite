#!/usr/bin/env bash

NIX_BUILDKITE=$(nix-build --no-out-link)
"$NIX_BUILDKITE" | buildkite-agent pipeline upload
