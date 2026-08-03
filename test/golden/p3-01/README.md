# P3-01 vector goldens

These fixtures are Haskell-owned contract evidence. They were not produced by
executing or importing the protected Python prototype, and normal test runs only
read and compare them.

Inputs and spaces:

- `semantic-memoized-compiler.f32le`: UTF-8 text `memoized compiler`, semantic
  space, 1024 dimensions, seeds `adrai-semantic-a` and
  `adrai-semantic-b`, exactly 4096 bytes. SHA-256 (hex):
  `ED1B375EA43EE9DEB864685B63093C0F2ED26AE28E1FB9113663F2346D138A0B`.
- `identifier-dropwire-lease.f32le`: UTF-8 text `DropwireLease`, identifier
  space, 768 dimensions, seeds `adrai-identifier-a` and
  `adrai-identifier-b`, exactly 3072 bytes. SHA-256 (hex):
  `D7784E3AA4FC6AFB0F0ADD5A1DA8F7A1E539F1FCD6315F71A9BD50C13DA6C219`.

Each feature hashes `seed <> NUL <> feature` with native-width BLAKE2b-128.
Bytes 0 through 7 interpreted as an unsigned little-endian integer select the
slot modulo the half dimension; byte 8 bit 0 selects the sign. Each nonzero half
is scaled to norm `sqrt(0.5)`. Persistence narrows every `Double` to IEEE-754
binary32 and writes its `Word32` bits in little-endian order, preserving negative
zero.

`vector-contract.golden` records the exact SHA-256 digests of both binary files,
their byte lengths and UTF-8 inputs, configured fingerprints and vector IDs,
BLAKE2b-128 anchor, semantic LSH signatures/probes, and three qualitative
semantic comparisons. The source anchors were reviewed statically at
`adrai_core/vectors.py:20-553` and `adrai_core/formats.py:37`; they are never used
at test runtime.

The retained release-ID anchors were also reviewed statically in
`ADRAI_1_Source/verification/ADRAI_1_Search_Enhanced_Dropwire_Evaluation_24.json`:
the semantic ID `adrai-semantic:T_yApQ2sJvyBH21HYYBYuQ1X:1024` appears near
line 547, and the identifier ID
`adrai-identifier:-JSyvxW6dxoDl9Pzpm6Q_aoG:768` appears near line 509. This
protected evaluation artifact is provenance only and is never loaded by tests.

`vector-contract.golden` is exactly 939 bytes with SHA-256 (hex)
`2DAC5ED02CBD37B98A4E5D40A8294188E97D30CF2712F2CB61BA15715560997F`.

The only regeneration path is the explicit Haskell test-runner argument
`--write-p3-01-goldens`. Review the printed SHA-256 values and this provenance
before accepting regenerated artifacts.
