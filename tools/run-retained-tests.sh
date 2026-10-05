#!/bin/sh
set -eu
# Tools are caller-selected, prepared outside the repository and verified by
# the runner manifest. This entrypoint does not install or discover substitutes.
: "${PWSH_EXE:?Set PWSH_EXE to the selected absolute Linux PowerShell executable}"
: "${ADRAI_RETAINED_OWNER_EXE:?Set ADRAI_RETAINED_OWNER_EXE to the built Linux ownership helper}"
case "$PWSH_EXE" in /*) ;; *) echo 'PWSH_EXE must be absolute' >&2; exit 2 ;; esac
case "$ADRAI_RETAINED_OWNER_EXE" in /*) ;; *) echo 'ADRAI_RETAINED_OWNER_EXE must be absolute' >&2; exit 2 ;; esac
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec "$PWSH_EXE" -NoLogo -NoProfile -NonInteractive -File "$script_dir/RunRetainedTests.ps1" \
  -LinuxOwnerExe "$ADRAI_RETAINED_OWNER_EXE" "$@"
