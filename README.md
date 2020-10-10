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

## `.buildkite/pipeline.yml`

Next, add a `.buildkite/pipeline.yml` file with the following contents:

``` yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      circuithub/nix-buildkite:
        file: jobs.nix
```

## Add Your Pipeline

The final step is to add your pipeline to Buildkite. See
https://buildkite.com/docs/pipelines/defining-steps#getting-started for details
on how to do this. Once you have a pipeline created, make sure that the only
step declared in the pipeline configuration in Buildkite's UI is:

``` yaml
steps:
  - command: buildkite-agent pipeline upload
    label: ":pipeline:"
```

## Sit Back and Enjoy!

That's it! Following these steps should give you a working pipeline that builds
`nix-buildkite`.
