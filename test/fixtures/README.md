# Test fixtures

This directory contains immutable, Haskell-owned contract fixtures and compact result baselines. Their provenance manifests were transcribed statically; tests never execute or import the Python prototype.

The generator-backed fixtures live in `test/support` because they are declarative Haskell values rather than checked-in generated repositories. The four distinct fixture shapes are:

- six ADRs for focused relevance behavior;
- 2,000 commits with 200 created ADRs for the bundled production-shape exercise;
- 12,000 commits for large history and branch stress;
- 2,000 logical ADRs for bounded retrieval scale.

These counts are not interchangeable. In particular, the retrieval corpus has 2,000 ADRs, while the bundled production shape has 2,000 commits and only 200 created ADRs.

`contracts/v1/search/dropwire-current.json` is only the current Dropwire result baseline. The external Dropwire source corpus used to produce the historical evaluation is not present in this repository, so that JSON cannot reconstruct or regenerate the source corpus. Haskell E2E parity must either use an independently available authorized Dropwire corpus or treat the committed JSON strictly as result-level acceptance evidence.

Generated repositories are future interpreter outputs and must not be committed as golden fixtures. Stable contracts are asserted through the compact Haskell summary and its SHA-256 digest.
