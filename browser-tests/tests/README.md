# Browser tests

Playwright end-to-end specifications will live in this directory. No browser
behavior is asserted during the scaffold milestone because neither a served UI
nor feature-parity flows exist yet.

The configuration can be parsed without launching or downloading a browser:

```powershell
npm.cmd exec -- playwright test --list --pass-with-no-tests
```

As product behavior is implemented, add stable user-visible flows here and keep
the suite independent of a Python runtime. The Python prototype may be consulted
as a reference only until the parity gate permits its deletion.
