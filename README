
# Hoobuddy

Hoobuddy is a simple too which, given a cabal file as input, will fetch hoogle databases for all packages specified as dependencies.
All databases will then be merged into a file called `default.hoo` in your current working directory.

Hoogle always searches the current directory first for a file called default.hoo. This allows for searches which include all functions
available in your current project.

In addition to the files listed in your cabal file hoobuddy will include a set of common packages to include in the search. Furthermore
the hoobuddy configuration file (~/.hoobuddy) can be used to configure a custom set of packages to include.

You can print a default configuration using the `--default` switch.

## Install

Check out the git repository and install via:

    $ cabal install

Alternatively you may want to install hoobuddy in a sandbox and copy the binary to somewhere in your PATH.

## Usage

**Note** Before running hoobudy for the first time you must have invoked `hoogle data` once.

When run without arguments hoobuddy prints the help:

    Usage : hoobuddy [deps|build] <cabal-file>
                     [--help]
                     [--default]

    deps         list configured dependencies
    build        do stuff yet to be defined

    --default    prints the default configuration
    --help       prints this help
