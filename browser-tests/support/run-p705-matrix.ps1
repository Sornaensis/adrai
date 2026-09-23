param(
    [ValidateSet('all', 'B01', 'B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B09', 'B10', 'B11', 'B12', 'B13', 'B14', 'B15')]
    [string] $Scenario = 'all',
    [string] $AdraiExe = $env:ADRAI_EXE,
    [string] $WindowFixtureExe = $env:P705_WINDOW_FIXTURE_EXE,
    [string] $AssetGateReceipt,
    [ValidateSet('normal', 'spawn-failure', 'timeout', 'early-success', 'early-error', 'finalization-timeout')]
    [string] $Probe = 'normal'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$clock = [Diagnostics.Stopwatch]::StartNew()
$deadlineMilliseconds = 600000
$browserRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$repositoryRoot = [IO.Path]::GetFullPath((Join-Path $browserRoot '..'))
$helperPath = Join-Path $repositoryRoot 'tools\RetainedTests\OwnedJob.cs'
$cliPath = Join-Path $browserRoot 'node_modules\@playwright\test\cli.js'
$probePath = Join-Path $PSScriptRoot 'p704-owned-probe.mjs'
$reporterPath = Join-Path $PSScriptRoot 'p705-reporter.cjs'
$inventoryPath = Join-Path $browserRoot 'tests\p705-scenarios.json'
$evidenceDirectory = Join-Path ([IO.Path]::GetTempPath()) ('adrai-p705-browser-evidence-' + [Guid]::NewGuid().ToString('N'))
$ownedTemporary = Join-Path ([IO.Path]::GetTempPath()) ('adrai-p705-owned-' + [Guid]::NewGuid().ToString('N'))
$stdoutPath = 'NUL'
$stderrPath = 'NUL'
$evidencePath = Join-Path $evidenceDirectory 'result.json'
$ownedScriptPath = Join-Path $ownedTemporary 'run-matrix.ps1'

function Get-RemainingMilliseconds {
    return [Math]::Max(0, $deadlineMilliseconds - [int]$clock.ElapsedMilliseconds)
}

function Get-WaitBudget([int] $reserve) {
    return [Math]::Max(0, (Get-RemainingMilliseconds) - $reserve)
}

function Test-ListenerClosed([int] $port) {
    if ($port -le 0) { return $false }
    $socket = [Net.Sockets.TcpClient]::new()
    try {
        $connection = $socket.ConnectAsync('127.0.0.1', $port)
        try {
            return (-not $connection.Wait(1000)) -or (-not $socket.Connected)
        }
        catch [AggregateException] {
            return $true
        }
    }
    finally {
        $socket.Dispose()
    }
}

function Assert-NoReparseEntries([string] $root) {
    $pending = [Collections.Generic.Stack[string]]::new()
    $pending.Push($root)
    while ($pending.Count -gt 0) {
        $current = $pending.Pop()
        foreach ($entry in [IO.Directory]::EnumerateFileSystemEntries($current)) {
            $attributes = [IO.File]::GetAttributes($entry)
            if (($attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {
                throw "Refusing to remove reparse entry inside owned browser temporary root: $entry"
            }
            if (($attributes -band [IO.FileAttributes]::Directory) -ne 0) { $pending.Push($entry) }
        }
    }
}

function Add-SafeError([string] $code) {
    if ($code -notmatch '^[a-z-]+$') { throw 'Invalid runner error code.' }
    $record.error = (@($record.error, $code) | Where-Object { $_ }) -join ';'
}

$record = [ordered]@{
    scenario = $Scenario
    probe = $Probe
    deadline_ms = $deadlineMilliseconds
    probe_effective_deadline_ms = $null
    cleanup_reserve_ms = 60000
    root_pid = $null
    root_exited = $false
    root_exit_code = $null
    job_empty_before_cleanup = $null
    job_active_before_cleanup = $null
    job_empty_after_cleanup = $false
    launch_failure_cleanup_verified = $null
    launch_attempted = $false
    launch_failure_type = $null
    launch_failure_native_error = $null
    timed_out = $false
    listener_port = $null
    listener_closed = $false
    owned_temporary_removed = $false
    cleanup_verified = $false
    error = $null
    hashes = [ordered]@{}
    inventory_ids = @()
    discovery_ids = @()
    execution_ids = @()
    execution_status = $null
    execution_cases = @()
    safe_facts = @()
    browser_pin = $null
    asset_inputs_verified = $false
    asset_gate = $null
    owned_temporary = $ownedTemporary
}
$job = $null
$root = $null

try {
    [IO.Directory]::CreateDirectory($evidenceDirectory) | Out-Null
    [IO.Directory]::CreateDirectory($ownedTemporary) | Out-Null
    $inventory = Get-Content -LiteralPath $inventoryPath -Raw | ConvertFrom-Json
    $record.inventory_ids = @($inventory.browser | ForEach-Object { $_.id })
    if ($record.inventory_ids.Count -ne 15 -or (@($record.inventory_ids | Sort-Object -Unique)).Count -ne 15) { throw 'P7-05 scenario inventory must have exactly 15 distinct IDs.' }
    if ([string]::IsNullOrWhiteSpace($AdraiExe)) { throw 'ADRAI_EXE or -AdraiExe is required.' }
    $adraiExe = [IO.Path]::GetFullPath($AdraiExe)
    if (-not (Test-Path -LiteralPath $adraiExe -PathType Leaf)) { throw 'ADRAI_EXE must identify the fresh built executable.' }
    if ([string]::IsNullOrWhiteSpace($WindowFixtureExe)) {
        $buildDirectory = Split-Path -Parent (Split-Path -Parent $adraiExe)
        $WindowFixtureExe = Join-Path $buildDirectory 'adrai-window-fixture\adrai-window-fixture.exe'
    }
    $windowFixtureExe = [IO.Path]::GetFullPath($WindowFixtureExe)
    if ($Scenario -in @('all', 'B03', 'B05', 'B12') -and -not (Test-Path -LiteralPath $windowFixtureExe -PathType Leaf)) { throw 'P7-05 paging and conflict scenarios require the exact built fixture executable.' }
    foreach ($pair in @(
        @('helper', $helperPath), @('supervisor', $PSCommandPath),
        @('probe', $probePath), @('reporter', $reporterPath), @('inventory', $inventoryPath),
        @('read_spec', (Join-Path $browserRoot 'tests\p705-read.spec.ts')),
        @('mutation_spec', (Join-Path $browserRoot 'tests\p705-mutations.spec.ts')),
        @('live_spec', (Join-Path $browserRoot 'tests\p705-live.spec.ts')),
        @('server_fixture', (Join-Path $PSScriptRoot 'p705-server.ts')),
        @('playwright_config', (Join-Path $browserRoot 'playwright.config.ts')),
        @('package_yaml', (Join-Path $repositoryRoot 'package.yaml')),
        @('generated_cabal', (Join-Path $repositoryRoot 'adrai.cabal')),
        @('window_fixture_main', (Join-Path $repositoryRoot 'test\web-api\WindowFixtureMain.hs')),
        @('window_documents', (Join-Path $repositoryRoot 'test\web-api\Adrai\WindowDocuments.hs')),
        @('app_bundle', (Join-Path $repositoryRoot 'web\dist\app.js')),
        @('asset_provenance', (Join-Path $repositoryRoot 'web\dist\provenance.json')),
        @('adrai_executable', $adraiExe)
    )) {
        $record.hashes[$pair[0]] = (Get-FileHash -LiteralPath $pair[1] -Algorithm SHA256).Hash.ToLowerInvariant()
    }
    if (Test-Path -LiteralPath $windowFixtureExe -PathType Leaf) {
        $record.hashes['window_fixture_executable'] = (Get-FileHash -LiteralPath $windowFixtureExe -Algorithm SHA256).Hash.ToLowerInvariant()
    }
    $installedPlaywright = Get-Content -Raw -LiteralPath (Join-Path $browserRoot 'node_modules\@playwright\test\package.json') | ConvertFrom-Json
    $installedPlaywrightRuntime = Get-Content -Raw -LiteralPath (Join-Path $browserRoot 'node_modules\playwright\package.json') | ConvertFrom-Json
    $installedPlaywrightCore = Get-Content -Raw -LiteralPath (Join-Path $browserRoot 'node_modules\playwright-core\package.json') | ConvertFrom-Json
    $installedBrowsers = Get-Content -Raw -LiteralPath (Join-Path $browserRoot 'node_modules\playwright-core\browsers.json') | ConvertFrom-Json
    [object[]]$chromium = @($installedBrowsers.browsers | Where-Object { $_.name -eq 'chromium-headless-shell' })
    if ($installedPlaywright.version -ne '1.61.1' -or $installedPlaywrightRuntime.version -ne '1.61.1' -or
        $installedPlaywrightCore.version -ne '1.61.1' -or
        $chromium.Count -ne 1 -or $chromium[0].revision -ne '1228') {
        throw 'Installed Playwright/Chromium does not match the pinned browser matrix.'
    }
    $browserCache = Join-Path $env:LOCALAPPDATA 'ms-playwright'
    $chromiumExe = Join-Path $browserCache 'chromium_headless_shell-1228\chrome-headless-shell-win64\chrome-headless-shell.exe'
    if (-not (Test-Path -LiteralPath $chromiumExe -PathType Leaf)) { throw 'Pinned Chromium headless executable is missing.' }
    $record.browser_pin = [ordered]@{
        playwright = [string]$installedPlaywright.version
        playwright_runtime = [string]$installedPlaywrightRuntime.version
        playwright_core = [string]$installedPlaywrightCore.version
        chromium_headless_shell_revision = [string]$chromium[0].revision
        chromium_version = [string]$chromium[0].browserVersion
        executable_sha256 = (Get-FileHash -LiteralPath $chromiumExe -Algorithm SHA256).Hash.ToLowerInvariant()
    }
    $assetRoot = Join-Path $repositoryRoot 'web'
    $assetReceipt = Get-Content -Raw -LiteralPath (Join-Path $assetRoot 'dist\provenance.json') | ConvertFrom-Json
    [object[]]$assetInputs = @($assetReceipt.inputs.PSObject.Properties)
    if ($assetReceipt.schema -ne 'adrai/assets/v1' -or $assetInputs.Count -ne 12) { throw 'Asset provenance schema or input inventory is invalid.' }
    foreach ($input in $assetInputs) {
        $relative = [string]$input.Name
        if ($relative -notmatch '^[A-Za-z0-9._/-]+$' -or @($relative -split '/' | Where-Object { $_ -eq '..' }).Count -ne 0) {
            throw 'Asset provenance input path is invalid.'
        }
        $path = [IO.Path]::GetFullPath((Join-Path $assetRoot ($relative.Replace('/', [IO.Path]::DirectorySeparatorChar))))
        if (-not $path.StartsWith($assetRoot.TrimEnd('\') + '\', [StringComparison]::OrdinalIgnoreCase) -or
            -not (Test-Path -LiteralPath $path -PathType Leaf) -or
            (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant() -ne [string]$input.Value) {
            throw 'Asset provenance input differs from the current source.'
        }
    }
    $appHash = (Get-FileHash -LiteralPath (Join-Path $assetRoot 'dist\app.js') -Algorithm SHA256).Hash.ToLowerInvariant()
    if ($appHash -ne [string]$assetReceipt.output.'dist/app.js') { throw 'Asset provenance output differs from the current bundle.' }
    $record.asset_inputs_verified = $true
    if ($Scenario -eq 'all' -and [string]::IsNullOrWhiteSpace($AssetGateReceipt)) { throw 'Final browser aggregate requires a persisted G01 asset gate receipt.' }
    if (-not [string]::IsNullOrWhiteSpace($AssetGateReceipt)) {
        $gatePath = [IO.Path]::GetFullPath($AssetGateReceipt)
        $gate = Get-Content -Raw -LiteralPath $gatePath | ConvertFrom-Json
        if ($gate.schema -ne 'adrai/p705-asset-gate/v1' -or $gate.result -ne 'passed' -or
            $gate.verify_assets_exit_code -ne 0 -or $gate.test_assets_exit_code -ne 0 -or
            $gate.asset_tests_passed -ne 2 -or $gate.copied_input_negative -ne $true -or
            $gate.app_sha256 -ne $appHash -or $gate.provenance_sha256 -ne $record.hashes['asset_provenance']) {
            throw 'G01 asset gate receipt does not match the current verified bundle.'
        }
        $record.asset_gate = [ordered]@{ verified = $true; receipt_sha256 = (Get-FileHash -LiteralPath $gatePath -Algorithm SHA256).Hash.ToLowerInvariant() }
    }
    Add-Type -Path $helperPath
    $job = [Adrai.RetainedTests.OwnedJob]::new()
    $node = (Get-Command node -ErrorAction Stop).Source
    $ownedScript = @'
param([string] $NodePath, [string] $CliPath, [string] $InventoryPath, [string] $Scenario, [string] $EvidenceDirectory)
$ErrorActionPreference = 'Stop'
$specs = @('tests/p705-read.spec.ts', 'tests/p705-mutations.spec.ts', 'tests/p705-live.spec.ts')
$inventory = Get-Content -LiteralPath $InventoryPath -Raw | ConvertFrom-Json
$listed = & $NodePath $CliPath test $specs --list --config playwright.config.ts 2>&1
if ($LASTEXITCODE -ne 0) { throw 'Playwright discovery failed before execution.' }
$lines = @($listed | ForEach-Object { [string] $_ })
$found = @($lines | Where-Object { $_ -match '\bB(0[1-9]|1[0-5])\b' } | ForEach-Object { [regex]::Match($_, '\bB(0[1-9]|1[0-5])\b').Value })
$expected = @($inventory.browser | ForEach-Object { $_.id })
if ($found.Count -ne 15 -or (($found | Sort-Object) -join ',') -ne (($expected | Sort-Object) -join ',')) {
    throw "Playwright discovery/inventory mismatch: found=$($found -join ',') expected=$($expected -join ',')"
}
foreach ($entry in $inventory.browser) {
    if (-not @($lines | Where-Object { $_.Contains("$($entry.id) $($entry.name)") -and $_.Contains($entry.spec) }).Count) {
        throw "Playwright discovery title/spec mismatch for $($entry.id)"
    }
}
Write-Output "P705_DISCOVERY_IDS=$($found -join ',')"
[IO.File]::WriteAllText((Join-Path $EvidenceDirectory 'discovery.json'), (ConvertTo-Json -InputObject ([ordered]@{ ids = $found; names = @($inventory.browser | ForEach-Object { $_.name }); spec_count = 3 }) -Depth 5), [Text.Encoding]::UTF8)
$arguments = @('test') + $specs + @('--config', 'playwright.config.ts', '--workers=1', '--retries=0', '--reporter=./support/p705-reporter.cjs', '--output', (Join-Path $env:P705_OWNED_TEMP_ROOT 'playwright'))
if ($Scenario -ne 'all') { $arguments += @('--grep', "$Scenario ") }
& $NodePath $CliPath @arguments
exit $LASTEXITCODE
'@
    [IO.File]::WriteAllText($ownedScriptPath, $ownedScript, [Text.Encoding]::UTF8)
    $executable = if ($Probe -eq 'spawn-failure') { Join-Path $PSScriptRoot 'missing-p705-owned-probe.exe' } elseif ($Probe -eq 'normal') { (Get-Command powershell.exe -ErrorAction Stop).Source } else { $node }
    $arguments = if ($Probe -eq 'normal') {
        [string[]]@('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', $ownedScriptPath, $node, $cliPath, $inventoryPath, $Scenario, $evidenceDirectory)
    } else {
        [string[]]@($probePath, $(if ($Probe -in @('spawn-failure', 'finalization-timeout')) { 'early-success' } else { $Probe }))
    }
    $environment = @{
        TMP = $ownedTemporary
        TEMP = $ownedTemporary
        P705_OWNED_JOB = '1'
        P705_OWNED_TEMP_ROOT = $ownedTemporary
        P705_EVIDENCE_DIR = $evidenceDirectory
        ADRAI_EXE = $adraiExe
        P705_WINDOW_FIXTURE_EXE = $windowFixtureExe
        PLAYWRIGHT_BROWSERS_PATH = $browserCache
    }
    if ($Probe -ne 'normal') { $environment['P704_OWNED_TEMP_ROOT'] = $ownedTemporary }
    $launchCleanup = [Math]::Min(5000, (Get-WaitBudget 10000))
    if ($launchCleanup -le 0) { throw 'No launch cleanup budget remains.' }
    try {
        $record.launch_attempted = $true
        $root = $job.Launch($executable, $arguments, $browserRoot, $environment, $stdoutPath, $stderrPath, $launchCleanup)
    }
    catch [Adrai.RetainedTests.OwnedLaunchException] {
        $record.launch_failure_type = 'OwnedLaunchException'
        $record.launch_failure_native_error = $_.Exception.NativeErrorCode
        $record.launch_failure_cleanup_verified = $_.Exception.ProcessCleanupVerified
        throw
    }
    catch [ComponentModel.Win32Exception] {
        $record.launch_failure_type = 'Win32Exception'
        $record.launch_failure_native_error = $_.Exception.NativeErrorCode
        throw
    }
    $record.root_pid = $root.ProcessId
    $rootBudget = if ($Probe -eq 'timeout') { [Math]::Min(1500, (Get-WaitBudget 60000)) } else { Get-WaitBudget 60000 }
    if ($rootBudget -le 0) { throw 'No root wait budget remains.' }
    $record.root_exited = $root.WaitForExit($rootBudget)
    if ($record.root_exited) {
        $record.root_exit_code = $root.GetExitCode()
        $record.job_empty_before_cleanup = $job.WaitForEmpty([Math]::Min(3000, (Get-WaitBudget 5000)))
    } else {
        $record.timed_out = $true
        $record.job_empty_before_cleanup = $false
    }
}
catch {
    Add-SafeError 'runner-error'
}
finally {
    if ($null -ne $job) {
        try {
            $record.job_active_before_cleanup = $job.ActiveProcessCount()
            if ($record.job_active_before_cleanup -gt 0) { $job.Terminate(125) }
            $record.job_empty_after_cleanup = $job.WaitForEmpty((Get-WaitBudget 2000))
        }
        catch {
            Add-SafeError 'job-cleanup-error'
        }
    } else {
        $record.job_empty_after_cleanup = $true
    }
    if ($null -ne $root) {
        try {
            $record.root_exited = $root.WaitForExit((Get-WaitBudget 2000))
            if ($record.root_exited -and $null -eq $record.root_exit_code) { $record.root_exit_code = $root.GetExitCode() }
        }
        catch {
            Add-SafeError 'root-cleanup-error'
        }
    }
    $discoveryPath = Join-Path $evidenceDirectory 'discovery.json'
    if (Test-Path -LiteralPath $discoveryPath) {
        try { $record.discovery_ids = @((Get-Content -LiteralPath $discoveryPath -Raw | ConvertFrom-Json).ids) }
        catch { Add-SafeError 'discovery-read-error' }
    }
    $executionPath = Join-Path $evidenceDirectory 'execution.json'
    if (Test-Path -LiteralPath $executionPath) {
        try {
            $execution = Get-Content -LiteralPath $executionPath -Raw | ConvertFrom-Json
            if ($execution.schema -ne 'adrai/p705-browser-execution/v1') { throw 'Unexpected P7-05 execution schema.' }
            $record.execution_status = $execution.status
            $record.execution_cases = [object[]]@($execution.cases | Where-Object { $null -ne $_ })
            $record.execution_ids = [string[]]@($record.execution_cases | ForEach-Object { $_.id })
            $record.safe_facts = @($execution.safe_facts)
            if ($execution.global_errors -ne 0) { throw 'P7-05 reporter recorded global errors.' }
        }
        catch { Add-SafeError 'execution-read-error' }
    }
    $listenerMarker = Join-Path $ownedTemporary 'listener.json'
    if (Test-Path -LiteralPath $listenerMarker) {
        try { $record.listener_port = [int]((Get-Content -LiteralPath $listenerMarker -Raw | ConvertFrom-Json).port) }
        catch { Add-SafeError 'listener-read-error' }
    }
    if ($null -ne $record.listener_port -and $record.job_empty_after_cleanup) {
        $record.listener_closed = Test-ListenerClosed $record.listener_port
    }
    $record.cleanup_verified = $record.job_empty_after_cleanup -and ($null -eq $root -or $record.root_exited) -and ($record.launch_failure_cleanup_verified -ne $false)
    if ($null -ne $root) { $root.Dispose() }
    if ($null -ne $job) { $job.Dispose() }
    if ($record.cleanup_verified) {
        try {
            $tempRoot = [IO.Path]::GetFullPath([IO.Path]::GetTempPath()).TrimEnd('\')
            $target = [IO.Path]::GetFullPath($ownedTemporary)
            if (-not $target.StartsWith($tempRoot + '\', [StringComparison]::OrdinalIgnoreCase) -or -not ([IO.Path]::GetFileName($target)).StartsWith('adrai-p705-owned-', [StringComparison]::Ordinal)) {
                throw "Refusing to remove unexpected browser temporary root: $target"
            }
            if (([IO.File]::GetAttributes($target) -band [IO.FileAttributes]::ReparsePoint) -ne 0) { throw 'Owned browser temporary root is a reparse point.' }
            Assert-NoReparseEntries $target
            Remove-Item -LiteralPath $target -Recurse -Force
            $record.owned_temporary_removed = -not (Test-Path -LiteralPath $target)
        }
        catch {
            Add-SafeError 'owned-temp-cleanup-error'
        }
    }
    [object[]]$expected = @()
    if ($Scenario -eq 'all') { $expected = [object[]]@($inventory.browser) }
    else { $expected = [object[]]@($inventory.browser | Where-Object { $_.id -eq $Scenario }) }
    [object[]]$executedCases = @($record.execution_cases | Where-Object { $null -ne $_ })
    [string[]]$executedIds = @($executedCases | ForEach-Object { $_.id })
    $executionPassed = $record.execution_status -eq 'passed' -and $executedCases.Count -eq $expected.Count -and
        (($executedIds | Sort-Object) -join ',') -eq (($expected | ForEach-Object { $_.id } | Sort-Object) -join ',')
    if ($executionPassed) {
        foreach ($case in $executedCases) {
            [object[]]$entry = @($expected | Where-Object { $_.id -eq $case.id })
            if ($entry.Count -ne 1 -or $case.title -ne "$($case.id) $($entry[0].name)" -or
                $case.spec -ne $entry[0].spec -or $case.status -ne 'passed' -or
                $case.expected_status -ne 'passed' -or $case.retry -ne 0) { $executionPassed = $false; break }
        }
    }
    $record.elapsed_ms = [int]$clock.ElapsedMilliseconds
    $normalPassed = $Probe -eq 'normal' -and $record.root_exit_code -eq 0 -and $record.job_empty_before_cleanup -eq $true -and $record.discovery_ids.Count -eq 15 -and $executionPassed -and -not $record.timed_out -and -not $record.error
    $timeoutPassed = $Probe -eq 'timeout' -and $record.timed_out -and $record.job_empty_before_cleanup -eq $false
    $earlyPassed = $Probe -in @('early-success', 'early-error', 'finalization-timeout') -and $record.root_exit_code -eq $(if ($Probe -eq 'early-error') { 17 } else { 0 }) -and $record.job_empty_before_cleanup -eq $false
    $spawnPassed = $Probe -eq 'spawn-failure' -and $record.launch_attempted -and $null -eq $root -and $record.launch_failure_type -eq 'Win32Exception' -and $record.launch_failure_native_error -eq 2 -and $record.job_active_before_cleanup -eq 0
    $needsListener = $Probe -ne 'spawn-failure'
    $record.exit_code = if (($normalPassed -or $timeoutPassed -or $earlyPassed -or $spawnPassed) -and $record.cleanup_verified -and $record.owned_temporary_removed -and (-not $needsListener -or $record.listener_closed) -and $record.elapsed_ms -le $deadlineMilliseconds) { 0 } else { 1 }
    try {
        $finalizationDeadline = $deadlineMilliseconds
        if ($Probe -eq 'finalization-timeout') {
            $finalizationDeadline = [int]$clock.ElapsedMilliseconds + 5
            $record.probe_effective_deadline_ms = $finalizationDeadline
        }
        $record.finalized_elapsed_ms = [int]$clock.ElapsedMilliseconds
        if ($record.finalized_elapsed_ms -gt $finalizationDeadline) { $record.exit_code = 1 }
        [IO.File]::WriteAllText($evidencePath, (ConvertTo-Json -InputObject $record -Depth 8), [Text.Encoding]::UTF8)
        if ($Probe -eq 'finalization-timeout') { Start-Sleep -Milliseconds 15 }
        $receiptCompletedMs = [int]$clock.ElapsedMilliseconds
        if ($receiptCompletedMs -gt $finalizationDeadline) {
            $record.exit_code = 1
            $record.finalized_elapsed_ms = $receiptCompletedMs
            Add-SafeError 'evidence-deadline-error'
            [IO.File]::WriteAllText($evidencePath, (ConvertTo-Json -InputObject $record -Depth 8), [Text.Encoding]::UTF8)
        }
        Write-Output "scenario=$Scenario probe=$Probe exit_code=$($record.exit_code) cleanup_verified=$($record.cleanup_verified) elapsed_ms=$receiptCompletedMs"
        Write-Output "evidence_path=$evidencePath"
    }
    catch {
        $record.exit_code = 1
        Write-Error 'P7-05 evidence finalization failed.'
    }
}

[Console]::Out.Flush()
[Console]::Error.Flush()
[Environment]::Exit([int]$record.exit_code)
