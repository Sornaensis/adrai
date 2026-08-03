# Integration tests

This directory owns tests spanning Haskell components and local dependencies.

The P4-01 Git suites generate temporary Haskell-owned repositories at test time. Fixture setup invokes the installed Git executable through `typed-process` argument arrays with controlled local identity; it never uses a shell, network remote, Python oracle, or committed `.git` directory. The repositories exercise normal, bare, linked, bare-origin-linked, sparse, shallow, spaces, and Unicode layouts. Temporary repositories are removed with their enclosing test directory.
