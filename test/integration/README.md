# Integration tests

This directory owns tests spanning Haskell components and local dependencies.

The P4-01 and P4-02 Git suites generate temporary Haskell-owned repositories at test time. Fixture setup invokes the installed Git executable through `typed-process` argument arrays with controlled local identity; it never uses a shell, network remote, Python oracle, or committed `.git` directory. The repositories exercise normal, bare, linked, bare-origin-linked, sparse, shallow, detached, branch-switched, reset, dirty/index-isolated, spaces, and Unicode layouts. P4-02 observations bind configuration, managed paths, and exact blob bytes to one resolved commit OID; the fixtures do not parse ADRAI documents, compile graphs, create databases, or exercise mutation semantics. Temporary repositories are removed with their enclosing test directory.
