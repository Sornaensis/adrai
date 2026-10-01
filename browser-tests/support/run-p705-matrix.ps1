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
# Whole-case caps include fixture setup, browser work and per-case teardown.
# Keep the independent discovery/runner allowance and owned cleanup reserve.
$caseBudgetMilliseconds = 6660000
$overheadMilliseconds = 75000
$cleanupReserveMilliseconds = 60000
$deadlineMilliseconds = $caseBudgetMilliseconds + $overheadMilliseconds + $cleanupReserveMilliseconds
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

function Get-SafeFailureCategory([Exception] $exception) {
    for ($depth = 0; $null -ne $exception -and $depth -lt 4; $depth++) {
        if ($exception -is [UnauthorizedAccessException] -or $exception -is [Security.SecurityException]) { return 'access-denied' }
        if ($exception -is [IO.FileNotFoundException] -or $exception -is [IO.DirectoryNotFoundException] -or
            $exception -is [Management.Automation.ItemNotFoundException]) { return 'missing-input' }
        if ($exception -is [ArgumentException] -or $exception -is [NotSupportedException] -or
            $exception -is [FormatException]) { return 'invalid-input' }
        if ($exception -is [IO.IOException]) { return 'io-error' }
        $exception = $exception.InnerException
    }
    return 'other'
}

# Labels are fixed; no exception text, paths, commands, or credentials are retained.
function Set-RunnerPhase([string] $phase, [string] $inputLabel = 'none') {
    if ($phase -notin $supervisorPhases -or $inputLabel -notin $inputLabels -or $supervisorEvents.Count -ge 96) {
        throw 'Invalid supervisor phase.'
    }
    $script:runnerPhase = $phase
    $script:runnerInput = $inputLabel
    $supervisorEvents.Add([pscustomobject]@{ schema = 'adrai/p705-startup/v1'; source = 'supervisor';
        seq = $supervisorEvents.Count + 1; phase = $phase; input = $inputLabel; code = 'none';
        at_ms = [DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds(); exit_code = $null })
}

function Read-StartupJournal([string] $path, [string] $source, [string[]] $phases) {
    if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { throw 'Missing startup journal.' }
    if ((Get-Item -LiteralPath $path).Length -gt 16384) { throw 'Startup journal exceeds byte bound.' }
    $raw = [IO.File]::ReadAllText($path, [Text.Encoding]::UTF8)
    if (-not $raw.EndsWith("`n")) { throw 'Truncated startup journal.' }
    $events = [Collections.Generic.List[object]]::new()
    $previousTime = 0L
    $previousPhase = -1
    $globalErrorSeen = $false
    foreach ($line in @($raw -split "`n" | Where-Object { $_.Length -gt 0 })) {
        if ($line.Length -gt 512 -or $events.Count -ge 32) { throw 'Startup entry exceeds bound.' }
        $event = $line.TrimEnd("`r") | ConvertFrom-Json
        if ((($event.PSObject.Properties.Name | Sort-Object) -join ',') -ne 'at_ms,code,error_count,exit_code,input,phase,schema,seq,source' -or
            $event.schema -ne 'adrai/p705-startup/v1' -or $event.source -ne $source -or
            ($event.seq -isnot [long] -and $event.seq -isnot [int]) -or $event.seq -ne $events.Count + 1 -or
            $event.at_ms -isnot [long] -or $event.at_ms -lt 1577836800000 -or
            $event.at_ms -lt $previousTime -or $event.at_ms -gt ([DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds() + 60000) -or
            $event.input -ne 'none' -or $event.phase -notin $phases -or
            $event.code -notin @('none', 'access-denied', 'missing-input', 'invalid-input', 'io-error', 'other', 'nonzero', 'global-error')) {
            throw 'Invalid startup event.'
        }
        $phaseIndex = [Array]::IndexOf($phases, [string]$event.phase)
        if ($event.phase -eq 'global-error') {
            if ($globalErrorSeen -or $events.Count -eq 0 -or $previousPhase -ge [Array]::IndexOf($phases, 'end')) {
                throw 'Duplicate or post-end reporter error.'
            }
            $globalErrorSeen = $true
        } elseif ($phaseIndex -le $previousPhase -or ($events.Count -eq 0 -and $phaseIndex -ne 0)) {
            throw 'Invalid startup ordering.'
        }
        if ($source -eq 'reporter' -and $event.phase -eq 'end') {
            if (($event.error_count -isnot [int] -and $event.error_count -isnot [long]) -or
                $event.error_count -lt 0 -or $event.error_count -gt 1024) { throw 'Invalid reporter error count.' }
        } elseif ($null -ne $event.error_count) { throw 'Unexpected reporter error count.' }
        if ($null -ne $event.exit_code -and
            ($event.phase -notin @('discovery-exited', 'cli-exited') -or ($event.exit_code -isnot [long] -and $event.exit_code -isnot [int]) -or
             $event.exit_code -lt -2147483648 -or $event.exit_code -gt 4294967295)) { throw 'Invalid startup exit code.' }
        if ($event.phase -in @('discovery-exited', 'cli-exited') -and
            ($null -eq $event.exit_code -or $event.code -ne $(if ($event.exit_code -eq 0) { 'none' } else { 'nonzero' }))) {
            throw 'Missing startup exit code.'
        }
        if ($event.phase -eq 'wrapper-error') {
            if ($event.code -in @('none', 'nonzero', 'global-error')) { throw 'Invalid wrapper failure category.' }
        } elseif ($event.phase -eq 'global-error') {
            if ($event.code -ne 'global-error') { throw 'Invalid reporter error category.' }
        } elseif ($event.phase -notin @('discovery-exited', 'cli-exited') -and $event.code -ne 'none') {
            throw 'Unexpected startup category.'
        }
        $events.Add($event)
        $previousTime = $event.at_ms
        if ($event.phase -ne 'global-error') { $previousPhase = $phaseIndex }
    }
    if ($events.Count -eq 0) { throw 'Empty startup journal.' }
    return $events.ToArray()
}

$supervisorPhases = @('evidence-setup', 'inventory-read', 'inventory-parse', 'inventory-validate',
    'adrai-normalize', 'adrai-exists', 'window-derive', 'window-normalize', 'window-exists',
    'input-hash', 'window-hash', 'pins', 'browser-hash', 'assets', 'asset-gate',
    'job-prepare', 'wrapper-prepare', 'launch', 'root-wait', 'root-exit',
    'cleanup', 'startup-read', 'receipts-read', 'listener-check', 'temporary-remove', 'finalization')
$inputLabels = @('none', 'helper', 'supervisor', 'probe', 'reporter', 'inventory',
    'read_spec', 'mutation_spec', 'live_spec', 'server_fixture', 'playwright_config',
    'package_yaml', 'generated_cabal', 'window_fixture_main', 'window_documents',
    'app_bundle', 'asset_provenance', 'adrai_executable', 'window_fixture_executable', 'chromium')
$supervisorEvents = [Collections.Generic.List[object]]::new()
$runnerPhase = 'evidence-setup'
$runnerInput = 'none'
$record = [ordered]@{
    scenario = $Scenario
    probe = $Probe
    deadline_ms = $deadlineMilliseconds
    probe_effective_deadline_ms = $null
    case_budget_ms = $caseBudgetMilliseconds
    overhead_ms = $overheadMilliseconds
    work_budget_ms = $caseBudgetMilliseconds + $overheadMilliseconds
    cleanup_reserve_ms = $cleanupReserveMilliseconds
    inventory_budget_ms = $null
    budget_verified = $false
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
    progress_events = @()
    progress_completed_cases = @()
    progress_last_phase = $null
    progress_truncated = $false
    progress_verified = $false
    startup_events = @()
    startup_verified = $false
    startup_failure = $null
    execution_global_errors = $null
    safe_facts = @()
    browser_pin = $null
    asset_inputs_verified = $false
    asset_gate = $null
    owned_temporary = $ownedTemporary
}
$job = $null
$root = $null
$inventory = $null
$inventoryValidated = $false
$ownedTemporaryCreated = $false
$startupComplete = $false

try {
    Set-RunnerPhase 'evidence-setup'
    [IO.Directory]::CreateDirectory($evidenceDirectory) | Out-Null
    [IO.Directory]::CreateDirectory($ownedTemporary) | Out-Null
    $ownedTemporaryCreated = $true
    Set-RunnerPhase 'inventory-read'
    $inventoryRaw = Get-Content -LiteralPath $inventoryPath -Raw
    Set-RunnerPhase 'inventory-parse'
    $inventory = $inventoryRaw | ConvertFrom-Json
    Set-RunnerPhase 'inventory-validate'
    $candidateIds = @($inventory.browser | ForEach-Object { $_.id })
    if ($candidateIds.Count -ne 15 -or (@($candidateIds | Sort-Object -Unique)).Count -ne 15) { throw 'P7-05 scenario inventory must have exactly 15 distinct IDs.' }
    if ($inventory.schema -ne 'adrai/browser-scenarios/v1' -or
        (($candidateIds | Sort-Object) -join ',') -ne 'B01,B02,B03,B04,B05,B06,B07,B08,B09,B10,B11,B12,B13,B14,B15') {
        throw 'Invalid scenario inventory.'
    }
    $record.inventory_ids = $candidateIds
    [long]$inventoryBudget = 0
    foreach ($entry in $inventory.browser) {
        if (($entry.timeout_ms -isnot [long] -and $entry.timeout_ms -isnot [int]) -or
            $entry.timeout_ms -le 0 -or $entry.timeout_ms -gt $caseBudgetMilliseconds) {
            throw 'P7-05 case cap must be a finite positive integer within the case budget.'
        }
        $inventoryBudget += $entry.timeout_ms
    }
    $record.inventory_budget_ms = $inventoryBudget
    if ($inventoryBudget -ne $caseBudgetMilliseconds -or
        $deadlineMilliseconds -ne $inventoryBudget + $overheadMilliseconds + $cleanupReserveMilliseconds) {
        throw 'P7-05 case inventory and aggregate budget differ.'
    }
    $record.budget_verified = $true
    $inventoryValidated = $true
    Set-RunnerPhase 'adrai-normalize'
    if ([string]::IsNullOrWhiteSpace($AdraiExe)) { throw [ArgumentException]::new('Required executable input is absent.') }
    $adraiExe = [IO.Path]::GetFullPath($AdraiExe)
    Set-RunnerPhase 'adrai-exists'
    if (-not (Test-Path -LiteralPath $adraiExe -PathType Leaf)) { throw [IO.FileNotFoundException]::new('Required executable input is missing.') }
    if ([string]::IsNullOrWhiteSpace($WindowFixtureExe)) {
        Set-RunnerPhase 'window-derive'
        $buildDirectory = Split-Path -Parent (Split-Path -Parent $adraiExe)
        $WindowFixtureExe = Join-Path $buildDirectory 'adrai-window-fixture\adrai-window-fixture.exe'
    }
    Set-RunnerPhase 'window-normalize'
    $windowFixtureExe = [IO.Path]::GetFullPath($WindowFixtureExe)
    Set-RunnerPhase 'window-exists'
    if ($Scenario -in @('all', 'B03', 'B05', 'B12') -and -not (Test-Path -LiteralPath $windowFixtureExe -PathType Leaf)) { throw [IO.FileNotFoundException]::new('Required fixture input is missing.') }
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
        Set-RunnerPhase 'input-hash' ([string]$pair[0])
        $record.hashes[$pair[0]] = (Get-FileHash -LiteralPath $pair[1] -Algorithm SHA256).Hash.ToLowerInvariant()
    }
    Set-RunnerPhase 'window-hash' 'window_fixture_executable'
    if (Test-Path -LiteralPath $windowFixtureExe -PathType Leaf) {
        $record.hashes['window_fixture_executable'] = (Get-FileHash -LiteralPath $windowFixtureExe -Algorithm SHA256).Hash.ToLowerInvariant()
    }
    Set-RunnerPhase 'pins'
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
    Set-RunnerPhase 'browser-hash' 'chromium'
    $record.browser_pin = [ordered]@{
        playwright = [string]$installedPlaywright.version
        playwright_runtime = [string]$installedPlaywrightRuntime.version
        playwright_core = [string]$installedPlaywrightCore.version
        chromium_headless_shell_revision = [string]$chromium[0].revision
        chromium_version = [string]$chromium[0].browserVersion
        executable_sha256 = (Get-FileHash -LiteralPath $chromiumExe -Algorithm SHA256).Hash.ToLowerInvariant()
    }
    Set-RunnerPhase 'assets'
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
    Set-RunnerPhase 'asset-gate'
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
    Set-RunnerPhase 'job-prepare'
    Add-Type -Path $helperPath
    $job = [Adrai.RetainedTests.OwnedJob]::new()
    $node = (Get-Command node -ErrorAction Stop).Source
    Set-RunnerPhase 'wrapper-prepare'
    $ownedScript = @'
param([string] $NodePath, [string] $CliPath, [string] $InventoryPath, [string] $Scenario, [string] $EvidenceDirectory)
$ErrorActionPreference = 'Stop'
$startupSequence = 0
function Write-Startup([string] $phase, [string] $code = 'none', $exitCode = $null) {
    $script:startupSequence++
    if ($startupSequence -gt 8 -or $phase -notin @('wrapper-started', 'discovery-started', 'discovery-exited', 'discovery-verified', 'cli-started', 'cli-exited', 'wrapper-error') -or
        $code -notin @('none', 'nonzero', 'access-denied', 'missing-input', 'invalid-input', 'io-error', 'other')) { throw 'Invalid wrapper evidence.' }
    $entry = [ordered]@{ schema = 'adrai/p705-startup/v1'; source = 'wrapper'; seq = $startupSequence;
        phase = $phase; input = 'none'; code = $code; at_ms = [DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds(); exit_code = $exitCode; error_count = $null }
    [IO.File]::AppendAllText((Join-Path $EvidenceDirectory 'wrapper-startup.ndjson'),
        ((ConvertTo-Json -InputObject $entry -Compress) + "`n"), [Text.Encoding]::UTF8)
}
Write-Startup 'wrapper-started'
try {
$specs = @('tests/p705-read.spec.ts', 'tests/p705-mutations.spec.ts', 'tests/p705-live.spec.ts')
$inventory = Get-Content -LiteralPath $InventoryPath -Raw | ConvertFrom-Json
Write-Startup 'discovery-started'
$listed = & $NodePath $CliPath test $specs --list --config playwright.config.ts 2>&1
Write-Startup 'discovery-exited' $(if ($LASTEXITCODE -eq 0) { 'none' } else { 'nonzero' }) $LASTEXITCODE
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
Write-Startup 'discovery-verified'
$arguments = @('test') + $specs + @('--config', 'playwright.config.ts', '--workers=1', '--retries=0', '--reporter=./support/p705-reporter.cjs', '--output', (Join-Path $env:P705_OWNED_TEMP_ROOT 'playwright'))
if ($Scenario -ne 'all') { $arguments += @('--grep', "$Scenario ") }
Write-Startup 'cli-started'
& $NodePath $CliPath @arguments
$cliExitCode = $LASTEXITCODE
Write-Startup 'cli-exited' $(if ($cliExitCode -eq 0) { 'none' } else { 'nonzero' }) $cliExitCode
exit $cliExitCode
}
catch {
    $exception = $_.Exception
    $kind = 'other'
    for ($depth = 0; $null -ne $exception -and $depth -lt 4; $depth++) {
        if ($exception -is [UnauthorizedAccessException]) { $kind = 'access-denied'; break }
        if ($exception -is [IO.FileNotFoundException] -or $exception -is [Management.Automation.CommandNotFoundException]) { $kind = 'missing-input'; break }
        if ($exception -is [ArgumentException] -or $exception -is [FormatException]) { $kind = 'invalid-input'; break }
        if ($exception -is [IO.IOException]) { $kind = 'io-error'; break }
        $exception = $exception.InnerException
    }
    try { Write-Startup 'wrapper-error' $kind } catch { }
    exit 1
}
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
        Set-RunnerPhase 'launch'
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
    $rootBudget = if ($Probe -eq 'timeout') { [Math]::Min(1500, (Get-WaitBudget $cleanupReserveMilliseconds)) } else { Get-WaitBudget $cleanupReserveMilliseconds }
    if ($rootBudget -le 0) { throw 'No root wait budget remains.' }
    Set-RunnerPhase 'root-wait'
    $record.root_exited = $root.WaitForExit($rootBudget)
    if ($record.root_exited) {
        Set-RunnerPhase 'root-exit'
        $record.root_exit_code = $root.GetExitCode()
        $record.job_empty_before_cleanup = $job.WaitForEmpty([Math]::Min(3000, (Get-WaitBudget 5000)))
    } else {
        $record.timed_out = $true
        $record.job_empty_before_cleanup = $false
    }
}
catch {
    $record.startup_failure = [ordered]@{ source = 'supervisor'; phase = $runnerPhase;
        input = $runnerInput; code = (Get-SafeFailureCategory $_.Exception) }
    Add-SafeError 'runner-error'
}
finally {
    try {
    Set-RunnerPhase 'cleanup'
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
    Set-RunnerPhase 'startup-read'
    $startup = [Collections.Generic.List[object]]::new()
    if ($Probe -eq 'normal' -and $null -ne $root) {
        foreach ($journal in @(
            @('wrapper-startup.ndjson', 'wrapper', @('wrapper-started', 'discovery-started', 'discovery-exited', 'discovery-verified', 'cli-started', 'cli-exited', 'wrapper-error')),
            @('reporter-startup.ndjson', 'reporter', @('initialized', 'begin', 'first-test', 'global-error', 'end'))
        )) {
            try {
                foreach ($event in @(Read-StartupJournal (Join-Path $evidenceDirectory $journal[0]) $journal[1] $journal[2])) { $startup.Add($event) }
            } catch { Add-SafeError 'startup-read-error' }
        }
        $wrapper = @($startup | Where-Object { $_.source -eq 'wrapper' })
        $reporter = @($startup | Where-Object { $_.source -eq 'reporter' })
        $startupComplete = ((($wrapper | ForEach-Object { $_.phase }) -join ',') -eq 'wrapper-started,discovery-started,discovery-exited,discovery-verified,cli-started,cli-exited') -and
            (@($wrapper | Where-Object { $_.phase -eq 'discovery-exited' -and $_.exit_code -eq 0 }).Count -eq 1) -and
            (@($reporter | Where-Object { $_.phase -eq 'initialized' }).Count -eq 1) -and
            (@($reporter | Where-Object { $_.phase -eq 'begin' }).Count -eq 1) -and
            (@($reporter | Where-Object { $_.phase -eq 'first-test' }).Count -eq 1) -and
            (@($reporter | Where-Object { $_.phase -eq 'end' }).Count -eq 1) -and
            (@($wrapper | Where-Object { $_.phase -eq 'cli-exited' -and $_.exit_code -eq $record.root_exit_code }).Count -eq 1)
        if (-not $startupComplete) { Add-SafeError 'startup-incomplete' }
    }
    $record.startup_events = $startup.ToArray()
    Set-RunnerPhase 'receipts-read'
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
            if (($execution.global_errors -isnot [int] -and $execution.global_errors -isnot [long]) -or
                $execution.global_errors -lt 0 -or $execution.global_errors -gt 1024) {
                throw 'Invalid execution global error count.'
            }
            $record.execution_global_errors = [long]$execution.global_errors
            if ($execution.global_errors -ne 0) { throw 'P7-05 reporter recorded global errors.' }
        }
        catch { Add-SafeError 'execution-read-error' }
    }
    if ($Probe -eq 'normal' -and $null -ne $root) {
        $reporterEnd = @($startup | Where-Object { $_.source -eq 'reporter' -and $_.phase -eq 'end' })
        $reporterErrors = @($startup | Where-Object { $_.source -eq 'reporter' -and $_.phase -eq 'global-error' })
        # A single first-error marker may cover several errors; end carries their exact count.
        $record.startup_verified = $startupComplete -and $null -ne $record.execution_global_errors -and
            $reporterEnd.Count -eq 1 -and $reporterEnd[0].error_count -eq $record.execution_global_errors -and
            $reporterErrors.Count -eq $(if ($record.execution_global_errors -gt 0) { 1 } else { 0 })
        if ($startupComplete -and -not $record.startup_verified) { Add-SafeError 'startup-execution-mismatch' }
    }
    $progress = [Collections.Generic.List[object]]::new()
    foreach ($journal in @(@('reporter-progress.ndjson', 'reporter'), @('fixture-progress.ndjson', 'fixture'))) {
        $journalPath = Join-Path $evidenceDirectory $journal[0]
        if (-not (Test-Path -LiteralPath $journalPath)) { continue }
        try {
            $raw = [IO.File]::ReadAllText($journalPath, [Text.Encoding]::UTF8)
            if ($raw.Length -gt 65536) { throw 'Progress journal exceeds bound.' }
            $lines = $raw -split "`n"
            if (-not $raw.EndsWith("`n")) { $record.progress_truncated = $true }
            $completeCount = $lines.Count - 1
            for ($index = 0; $index -lt $completeCount; $index++) {
                if ($lines[$index].Length -gt 256) { throw 'Progress entry exceeds bound.' }
                $event = $lines[$index].TrimEnd("`r") | ConvertFrom-Json
                $status = $null
                if ($event.PSObject.Properties.Name -contains 'status') { $status = $event.status }
                if ($event.schema -ne 'adrai/p705-progress/v1' -or $event.source -ne $journal[1] -or
                    $event.id -notin $record.inventory_ids -or $event.at_ms -isnot [long] -or
                    ($event.seq -isnot [int] -and $event.seq -isnot [long]) -or
                    $event.seq -lt 1 -or $event.seq -gt 256 -or
                    $event.at_ms -lt 1577836800000 -or $event.at_ms -gt ([DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds() + 60000)) {
                    throw 'Invalid progress entry.'
                }
                $allowed = if ($journal[1] -eq 'reporter') { @('started', 'ended') } else {
                    @('fixture-start', 'git-seeded', 'adrai-initialized', 'decisions-seeded', 'bulk-seeded',
                      'sentinels-ready', 'web-started', 'web-ready', 'preflight-ready', 'action-started',
                      'action-ended', 'teardown-started', 'teardown-ended')
                }
                if ($event.phase -notin $allowed -or ($null -ne $status -and
                    ($event.phase -ne 'ended' -or $status -notin @('passed', 'failed', 'timedOut', 'skipped', 'interrupted')))) {
                    throw 'Invalid progress phase or status.'
                }
                $progress.Add([pscustomobject]@{ id = [string]$event.id; source = [string]$event.source;
                    phase = [string]$event.phase; at_ms = [long]$event.at_ms;
                    seq = [long]$event.seq; status = $status })
                if ($progress.Count -gt 256) { throw 'Progress event count exceeds bound.' }
            }
        }
        catch { Add-SafeError 'progress-read-error' }
    }
    if ($progress.Count -gt 0) {
        $ordered = @($progress | Sort-Object at_ms, source, seq)
        $first = $ordered[0].at_ms
        $previous = $first
        $record.progress_events = @($ordered | ForEach-Object {
            $event = [ordered]@{ id = $_.id; source = $_.source; phase = $_.phase;
                elapsed_ms = [Math]::Max(0, [long]($_.at_ms - $first));
                since_previous_ms = [Math]::Max(0, [long]($_.at_ms - $previous)); status = $_.status }
            $previous = $_.at_ms
            $event
        })
        $record.progress_completed_cases = @($record.progress_events | Where-Object { $_.source -eq 'reporter' -and $_.phase -eq 'ended' } |
            ForEach-Object { [ordered]@{ id = $_.id; status = $_.status } })
        $record.progress_last_phase = $record.progress_events[-1]
    }
    Set-RunnerPhase 'listener-check'
    $listenerMarker = Join-Path $ownedTemporary 'listener.json'
    if (Test-Path -LiteralPath $listenerMarker) {
        try { $record.listener_port = [int]((Get-Content -LiteralPath $listenerMarker -Raw | ConvertFrom-Json).port) }
        catch { Add-SafeError 'listener-read-error' }
    }
    if ($null -ne $record.listener_port -and $record.job_empty_after_cleanup) {
        try { $record.listener_closed = Test-ListenerClosed $record.listener_port }
        catch { Add-SafeError 'listener-check-error' }
    }
    $record.cleanup_verified = $record.job_empty_after_cleanup -and ($null -eq $root -or $record.root_exited) -and ($record.launch_failure_cleanup_verified -ne $false)
    if ($null -ne $root) { try { $root.Dispose() } catch { Add-SafeError 'root-dispose-error' } }
    if ($null -ne $job) { try { $job.Dispose() } catch { Add-SafeError 'job-dispose-error' } }
    Set-RunnerPhase 'temporary-remove'
    if ($record.cleanup_verified) {
        try {
            $tempRoot = [IO.Path]::GetFullPath([IO.Path]::GetTempPath()).TrimEnd('\')
            $target = [IO.Path]::GetFullPath($ownedTemporary)
            if (-not $target.StartsWith($tempRoot + '\', [StringComparison]::OrdinalIgnoreCase) -or -not ([IO.Path]::GetFileName($target)).StartsWith('adrai-p705-owned-', [StringComparison]::Ordinal)) {
                throw "Refusing to remove unexpected browser temporary root: $target"
            }
            if ($ownedTemporaryCreated) {
            if (([IO.File]::GetAttributes($target) -band [IO.FileAttributes]::ReparsePoint) -ne 0) { throw 'Owned browser temporary root is a reparse point.' }
            Assert-NoReparseEntries $target
            Remove-Item -LiteralPath $target -Recurse -Force
            }
            $record.owned_temporary_removed = -not (Test-Path -LiteralPath $target)
        }
        catch {
            Add-SafeError 'owned-temp-cleanup-error'
        }
    }
    [object[]]$expected = @()
    if ($inventoryValidated) {
        if ($Scenario -eq 'all') { $expected = [object[]]@($inventory.browser) }
        else { $expected = [object[]]@($inventory.browser | Where-Object { $_.id -eq $Scenario }) }
    }
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
    $progressPassed = -not $record.progress_truncated -and $record.progress_completed_cases.Count -eq $expected.Count
    if ($progressPassed) {
        foreach ($entry in $expected) {
            $id = $entry.id
            $fixtureRuns = if ($id -eq 'B04') { 2 } else { 1 }
            if (@($record.progress_events | Where-Object { $_.id -eq $id -and $_.source -eq 'reporter' -and $_.phase -eq 'started' }).Count -ne 1 -or
                @($record.progress_completed_cases | Where-Object { $_.id -eq $id -and $_.status -eq 'passed' }).Count -ne 1 -or
                @($record.progress_events | Where-Object { $_.id -eq $id -and $_.source -eq 'fixture' -and $_.phase -eq 'fixture-start' }).Count -ne $fixtureRuns -or
                @($record.progress_events | Where-Object { $_.id -eq $id -and $_.source -eq 'fixture' -and $_.phase -eq 'action-ended' }).Count -ne $fixtureRuns -or
                @($record.progress_events | Where-Object { $_.id -eq $id -and $_.source -eq 'fixture' -and $_.phase -eq 'teardown-ended' }).Count -ne $fixtureRuns) {
                $progressPassed = $false
                break
            }
        }
    }
    $record.progress_verified = $progressPassed
    $record.elapsed_ms = [int]$clock.ElapsedMilliseconds
    $normalPassed = $Probe -eq 'normal' -and $record.budget_verified -and $record.root_exit_code -eq 0 -and $record.job_empty_before_cleanup -eq $true -and $record.discovery_ids.Count -eq 15 -and $record.startup_verified -and $executionPassed -and $progressPassed -and -not $record.timed_out -and -not $record.error
    $timeoutPassed = $Probe -eq 'timeout' -and $record.timed_out -and $record.job_empty_before_cleanup -eq $false
    $earlyPassed = $Probe -in @('early-success', 'early-error', 'finalization-timeout') -and $record.root_exit_code -eq $(if ($Probe -eq 'early-error') { 17 } else { 0 }) -and $record.job_empty_before_cleanup -eq $false
    $spawnPassed = $Probe -eq 'spawn-failure' -and $record.launch_attempted -and $null -eq $root -and $record.launch_failure_type -eq 'Win32Exception' -and $record.launch_failure_native_error -eq 2 -and $record.job_active_before_cleanup -eq 0
    $needsListener = $Probe -ne 'spawn-failure'
    $record.exit_code = if ($record.budget_verified -and ($normalPassed -or $timeoutPassed -or $earlyPassed -or $spawnPassed) -and $record.cleanup_verified -and $record.owned_temporary_removed -and (-not $needsListener -or $record.listener_closed) -and $record.elapsed_ms -le $deadlineMilliseconds) { 0 } else { 1 }
    }
    catch {
        Add-SafeError 'finalization-error'
        $record.exit_code = 1
    }
    finally {
    try {
        Set-RunnerPhase 'finalization'
        $record.startup_events = @(@($supervisorEvents.ToArray()) + @($record.startup_events) | Sort-Object at_ms, source, seq)
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
        [Console]::Error.WriteLine('P7-05 evidence finalization failed.')
    }
    }
}

[Console]::Out.Flush()
[Console]::Error.Flush()
[Environment]::Exit([int]$record.exit_code)
