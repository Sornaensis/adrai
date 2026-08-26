# Installation

ADRAI is currently installed from source.

## Requirements

- Git 2.31 or newer
- Stack 3.11.1

The project pins its compiler and package set in `stack.yaml` (`lts-24.52`). Stack downloads the matching GHC toolchain when needed. Python and libgit2 are not runtime dependencies.

## Build

From the repository root:

```console
stack build
stack exec adrai -- --help
```

To copy the executable into Stack's local binary directory:

```console
stack install
adrai --help
```

If Stack's binary directory is not on `PATH`, use `stack path --local-bin` to locate the installed executable.

See [Usage](USAGE.md) for the repository setup flow.
