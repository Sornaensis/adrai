# Elm tests

This directory is reserved for focused Elm tests as behavior is introduced. The
current milestone establishes only a compilable module boundary, so it does not
assert product behavior that has not been implemented.

Future tests should live here, exercise public module behavior, and remain
independent of the Python prototype at runtime. End-to-end parity belongs in the
top-level `browser-tests` project.
