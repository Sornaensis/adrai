# Installation

ADRAI does not yet provide an installable release. Packaging and release installation instructions remain deferred.

The implemented read-only repository layer has one external runtime prerequisite: Git 2.31 or newer, including support for `git rev-parse --path-format=absolute`. Git is invoked directly with argument arrays; no shell, Python runtime, or libgit2 installation is required.

Building from source additionally requires the pinned Stack/GHC toolchain described by `stack.yaml`. Stack and GHC are build-time dependencies, not runtime prerequisites of repository observation.
