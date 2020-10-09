# nix-buildkite [![Build status](https://badge.buildkite.com/7918a1ba68d299f83ccc990292a97fa6eecd251703b2ca9427.svg)](https://buildkite.com/circuithub/nix-buildkite)

`nix-buildkite` is a tool that can take a Nix expression that describes a set of
builds and transforms them into separate Buildkite jobs. `nix-buildkite`
evaluates Nix to create derivations and then analyses these derivations to find
a topological ordering that will ensure steps have the correct dependencies
between them.

# Getting Started

## `jobs.nix`

First, create a `jobs.nix` file in your repository. This file will contain a
tree of all builds that you are interested in. We create this tree using nested
attrsets that eventually have leaves that are derivations.

For this example, we'll start by building the `nix-buildkite` project. Our
`jobs.nix` file is:

``` nix
let pkgs = import ./nix/pkgs {};
in
{
  nix-buildkite = pkgs.haskellPackages.nix-buildkite;
}
```

## `.buildkite/pipeline.sh`

Next, add a `.buildkite/pipeline.sh` script with the following contents:

``` shell
#!/usr/bin/env bash

nix-build -E 'import (builtins.fetchGit { url = git://github.com/circuithub/nix-buildkite; })' -o nix-buildkite
./nix-buildkite | buildkite-agent pipeline upload
rm nix-buildkite
```

## Add Your Pipeline

The final step is to add your pipeline to Buildkite. See
https://buildkite.com/docs/pipelines/defining-steps#getting-started for details
on how to do this. Once you have a pipeline created, make sure that the only
step declared in the pipeline configuration in Buildkites UI is:

``` yaml
steps:
  - command: .buildkite/pipeline.sh
    label: ":pipeline: Upload"
```

## Sit Back and Enjoy!

That's it! The following steps should give you a working pipeline that builds
`nix-buildkite`.
