param(
    [ValidateSet('normal', 'spawn-failure', 'timeout', 'early-success', 'early-error')]
    [string] $Probe = 'normal'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$clock = [Diagnostics.Stopwatch]::StartNew()
$deadlineMilliseconds = 180000
$browserRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$repositoryRoot = [IO.Path]::GetFullPath((Join-Path $browserRoot '..'))
$helperPath = Join-Path $repositoryRoot 'tools\RetainedTests\OwnedJob.cs'
$cliPath = Join-Path $browserRoot 'node_modules\@playwright\test\cli.js'
$probePath = Join-Path $PSScriptRoot 'p704-owned-probe.mjs'
$evidenceDirectory = Join-Path ([IO.Path]::GetTempPath()) ('adrai-p704-browser-evidence-' + [Guid]::NewGuid().ToString('N'))
$ownedTemporary = Join-Path ([IO.Path]::GetTempPath()) ('adrai-p704-owned-' + [Guid]::NewGuid().ToString('N'))
$stdoutPath = Join-Path $evidenceDirectory 'stdout.log'
$stderrPath = Join-Path $evidenceDirectory 'stderr.log'
$evidencePath = Join-Path $evidenceDirectory 'result.json'

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

$record = [ordered]@{
    probe = $Probe
    deadline_ms = $deadlineMilliseconds
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
    owned_temporary = $ownedTemporary
    stdout_path = $stdoutPath
    stderr_path = $stderrPath
}
$job = $null
$root = $null

try {
    [IO.Directory]::CreateDirectory($evidenceDirectory) | Out-Null
    [IO.Directory]::CreateDirectory($ownedTemporary) | Out-Null
    foreach ($pair in @(
        @('helper', $helperPath), @('supervisor', $PSCommandPath),
        @('probe', $probePath), @('smoke', (Join-Path $browserRoot 'tests\p704-smoke.spec.ts')),
        @('server_fixture', (Join-Path $PSScriptRoot 'p704-server.ts')),
        @('playwright_config', (Join-Path $browserRoot 'playwright.config.ts')),
        @('app_bundle', (Join-Path $repositoryRoot 'web\dist\app.js')),
        @('asset_provenance', (Join-Path $repositoryRoot 'web\dist\provenance.json'))
    )) {
        $record.hashes[$pair[0]] = (Get-FileHash -LiteralPath $pair[1] -Algorithm SHA256).Hash.ToLowerInvariant()
    }
    Add-Type -Path $helperPath
    $job = [Adrai.RetainedTests.OwnedJob]::new()
    $node = (Get-Command node -ErrorAction Stop).Source
    $executable = if ($Probe -eq 'spawn-failure') { Join-Path $PSScriptRoot 'missing-p704-owned-probe.exe' } else { $node }
    $arguments = if ($Probe -eq 'normal') {
        [string[]]@($cliPath, 'test', 'tests/p704-smoke.spec.ts', '--config', 'playwright.config.ts', '--workers=1', '--reporter=list', '--output', (Join-Path $evidenceDirectory 'playwright'))
    } else {
        [string[]]@($probePath, $(if ($Probe -eq 'spawn-failure') { 'early-success' } else { $Probe }))
    }
    $environment = @{
        TMP = $ownedTemporary
        TEMP = $ownedTemporary
        P704_OWNED_JOB = '1'
        P704_OWNED_TEMP_ROOT = $ownedTemporary
        P704_EVIDENCE_DIR = $evidenceDirectory
    }
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
    $rootBudget = if ($Probe -eq 'timeout') { [Math]::Min(1500, (Get-WaitBudget 10000)) } else { Get-WaitBudget 10000 }
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
    $record.error = $_.Exception.Message
}
finally {
    if ($null -ne $job) {
        try {
            $record.job_active_before_cleanup = $job.ActiveProcessCount()
            if ($record.job_active_before_cleanup -gt 0) { $job.Terminate(125) }
            $record.job_empty_after_cleanup = $job.WaitForEmpty((Get-WaitBudget 2000))
        }
        catch {
            $record.error = (($record.error, $_.Exception.Message) | Where-Object { $_ }) -join '; '
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
            $record.error = (($record.error, $_.Exception.Message) | Where-Object { $_ }) -join '; '
        }
    }
    $listenerMarker = Join-Path $ownedTemporary 'listener.json'
    if (Test-Path -LiteralPath $listenerMarker) {
        try { $record.listener_port = [int]((Get-Content -LiteralPath $listenerMarker -Raw | ConvertFrom-Json).port) }
        catch { $record.error = (($record.error, $_.Exception.Message) | Where-Object { $_ }) -join '; ' }
    } elseif (Test-Path -LiteralPath $stdoutPath) {
        $output = [IO.File]::ReadAllText($stdoutPath)
        if ($output -match 'P704_ORIGIN=http://127\.0\.0\.1:(\d+)') { $record.listener_port = [int]$Matches[1] }
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
            if (-not $target.StartsWith($tempRoot + '\', [StringComparison]::OrdinalIgnoreCase) -or -not ([IO.Path]::GetFileName($target)).StartsWith('adrai-p704-owned-', [StringComparison]::Ordinal)) {
                throw "Refusing to remove unexpected browser temporary root: $target"
            }
            if (([IO.File]::GetAttributes($target) -band [IO.FileAttributes]::ReparsePoint) -ne 0) { throw 'Owned browser temporary root is a reparse point.' }
            Assert-NoReparseEntries $target
            Remove-Item -LiteralPath $target -Recurse -Force
            $record.owned_temporary_removed = -not (Test-Path -LiteralPath $target)
        }
        catch {
            $record.error = (($record.error, $_.Exception.Message) | Where-Object { $_ }) -join '; '
        }
    }
    $record.elapsed_ms = [int]$clock.ElapsedMilliseconds
    $normalPassed = $Probe -eq 'normal' -and $record.root_exit_code -eq 0 -and $record.job_empty_before_cleanup -eq $true -and -not $record.timed_out -and -not $record.error
    $timeoutPassed = $Probe -eq 'timeout' -and $record.timed_out -and $record.job_empty_before_cleanup -eq $false
    $earlyPassed = $Probe -in @('early-success', 'early-error') -and $record.root_exit_code -eq $(if ($Probe -eq 'early-success') { 0 } else { 17 }) -and $record.job_empty_before_cleanup -eq $false
    $spawnPassed = $Probe -eq 'spawn-failure' -and $record.launch_attempted -and $null -eq $root -and $record.launch_failure_type -eq 'Win32Exception' -and $record.launch_failure_native_error -eq 2 -and $record.job_active_before_cleanup -eq 0
    $needsListener = $Probe -ne 'spawn-failure'
    $record.exit_code = if (($normalPassed -or $timeoutPassed -or $earlyPassed -or $spawnPassed) -and $record.cleanup_verified -and $record.owned_temporary_removed -and (-not $needsListener -or $record.listener_closed) -and $record.elapsed_ms -le $deadlineMilliseconds) { 0 } else { 1 }
    try {
        [IO.File]::WriteAllText($evidencePath, (ConvertTo-Json -InputObject $record -Depth 8), [Text.Encoding]::UTF8)
        if (Test-Path -LiteralPath $stdoutPath) { Get-Content -LiteralPath $stdoutPath }
        if (Test-Path -LiteralPath $stderrPath) { Get-Content -LiteralPath $stderrPath }
        Write-Output ((ConvertTo-Json -InputObject $record -Depth 8) -replace '\s+', ' ')
        Write-Output "evidence_path=$evidencePath"
    }
    catch {
        $record.exit_code = 1
        Write-Error $_.Exception.Message
    }
}

exit $record.exit_code
