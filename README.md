# ADRAI

ADRAI stores and searches Architecture Decision Records (ADRs) in Git. It is for developers and teams who want to record software decisions and find them later, using a command-line, terminal, or web interface.

## Install and set up

Use Git 2.31 or newer, Stack 3.11.1, Node 24.15.0 or newer, and npm 11.12.1 or newer. Linux also needs a C toolchain; see [Installation](docs/INSTALL.md) for platform requirements.

From the ADRAI source root:

```powershell
npm --prefix web ci
npm --prefix web run build
stack install
adrai --help
```

`npm ci` installs Elm, and the web build generates assets embedded in the executable. Stack downloads the matching GHC toolchain when needed. Add the directory printed by `stack path --local-bin` to `PATH` if `adrai` is not found.

Then, from the existing Git repository where you want to keep decisions, initialize ADRAI once:

```powershell
adrai init
```

## Examples

Suppose a team chooses PostgreSQL because orders and payments need consistent transactions. Recording that choice, its reasons, and the files it applies to gives the next developer context when changing storage code. An LLM agent can read the same decisions through JSON search results before proposing changes, so it can account for existing constraints instead of guessing from the code alone.

Run these examples in that repository. They use PowerShell; see [Usage](docs/USAGE.md) for more options.

Create a decision with directory and glob scopes:

```powershell
adrai create --title "Use PostgreSQL" `
  --summary "Keep transactional data in PostgreSQL." `
  --body "Transactions keep orders and payments consistent." `
  --applies-to "src/storage/" --applies-to "src/**/*.sql" --actor human:alice
```

Replace `ADR_ID` with the identifier printed by `create`. Amend the decision when its requirements change:

```powershell
adrai amend ADR_ID `
  --body "Transactions keep orders and payments consistent; migrations must remain backward compatible." `
  --change-summary "Document migration requirements." --actor human:alice
```

Both commands create Git commits. `amend` replaces the current body and keeps the omitted title and summary. Earlier versions remain in Git; `adrai history ADR_ID` shows the change history.

Add `--preview` or `--dry-run` to any of `init`, `create`, `amend`, `scope`,
`domain`, `obsolete`, and `reactivate` to review proposed files without writes
or commits. For example, append `--preview --json` to the create command above.
The proposal reports its repository basis and full file bytes. IDs and
timestamps are provisional; rerun without the flag to commit. See
[preview output and validation](docs/USAGE.md#review-before-committing).

Search by topic, find decisions whose declared scopes match a file, or rank decisions against the file's current contents:

```powershell
adrai search "transactions"
adrai domains
adrai domains platform.storage
adrai search --domain platform.storage
adrai search --file src/storage/Orders.hs --json
adrai relevant src/storage/Orders.hs --worktree --json
```

`--file` and `relevant` take concrete file paths. To look across a directory or pattern, expand tracked files with Git and query each one; results are separate for each file:

```powershell
git ls-files -- ':(glob)src/storage/**' | ForEach-Object { adrai search --file $_ --json }
git ls-files -- ':(glob)src/**/*.hs' | ForEach-Object { adrai relevant $_ --worktree --json }
```

- [Installation](docs/INSTALL.md)
- [Usage](docs/USAGE.md)
- [Documentation](docs/README.md)
- [Development](docs/DEVELOPMENT.md)
