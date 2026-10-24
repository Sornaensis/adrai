sqlite-vec v0.1.6, upstream source 639fca5739fe056fdc98f3d539c4cd79328d7dc7.

Source: https://github.com/asg017/sqlite-vec/releases/download/v0.1.6/sqlite-vec-0.1.6-amalgamation.zip

The unmodified amalgamation is built with SQLITE_VEC_STATIC and SQLITE_CORE against the bundled
direct-sqlite SQLite. register.c is ADRAI's registration adapter. The upstream
MIT and Apache 2.0 licenses are included. No extension path or runtime download
is required.

connect.c is ADRAI's Windows-only nonblocking connect/select adapter for bounded local HTTP transport. It does not modify the upstream amalgamation. Socket creation, address marshalling and closure use the installed network package.
