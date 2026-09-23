param(
    [ValidateSet('normal', 'timeout', 'spawn-failure', 'early-success', 'early-error')]
    [string] $Probe = 'normal'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$clock = [Diagnostics.Stopwatch]::StartNew()
$deadlineMilliseconds = 60000
$webRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$repositoryRoot = [IO.Path]::GetFullPath((Join-Path $webRoot '..'))
$helperPath = Join-Path $repositoryRoot 'tools\RetainedTests\OwnedJob.cs'
$workerPath = Join-Path $PSScriptRoot 'run-components.mjs'
$missingExecutable = Join-Path $PSScriptRoot 'missing-p704-probe.exe'
$testPath = Join-Path $PSScriptRoot 'ExplorerTest.elm'
$fixturePath = Join-Path $webRoot 'fixtures\api-v1.json'
$generatedPath = Join-Path $PSScriptRoot 'FixtureData.elm'
$evidenceDirectory = Join-Path ([IO.Path]::GetTempPath()) ('adrai-p704-components-' + [Guid]::NewGuid().ToString('N'))
$evidencePath = Join-Path $evidenceDirectory 'result.json'
$stdoutPath = Join-Path $evidenceDirectory 'stdout.log'
$stderrPath = Join-Path $evidenceDirectory 'stderr.log'
$utf8NoBom = New-Object System.Text.UTF8Encoding($false)

function Get-RemainingMilliseconds {
    return [Math]::Max(0, $deadlineMilliseconds - [int]$clock.ElapsedMilliseconds)
}

function Get-WaitBudget([int] $reserve) {
    return [Math]::Max(0, (Get-RemainingMilliseconds) - $reserve)
}

function Get-Sha256([string] $path) {
    return (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant()
}

$record = [ordered]@{
    command = 'powershell -NoProfile -ExecutionPolicy Bypass -File tests/run-components.ps1 -Probe ' + $Probe
    probe = $Probe
    deadline_ms = $deadlineMilliseconds
    root_pid = $null
    root_exited = $false
    root_exit_code = $null
    job_empty_before_cleanup = $null
    job_active_before_cleanup = $null
    job_empty_after_cleanup = $false
    launch_failure_cleanup_verified = $null
    launch_failure_stage = $null
    launch_failure_kind = $null
    launch_failure_native_error = $null
    timed_out = $false
    fixture_removed = $false
    cleanup_verified = $false
    error = $null
    hashes = [ordered]@{}
    stdout_path = $stdoutPath
    stderr_path = $stderrPath
}
$job = $null
$root = $null
$fixtureCreated = $false

try {
    [IO.Directory]::CreateDirectory($evidenceDirectory) | Out-Null
    foreach ($pair in @(
        @('helper', $helperPath), @('supervisor', $PSCommandPath),
        @('worker', $workerPath), @('tests', $testPath), @('fixture', $fixturePath)
    )) {
        $record.hashes[$pair[0]] = Get-Sha256 $pair[1]
    }
    if (Test-Path -LiteralPath $generatedPath) {
        throw 'Temporary FixtureData.elm already exists; refusing to replace it.'
    }
    $fixture = [IO.File]::ReadAllText($fixturePath)
    $literal = ConvertTo-Json -InputObject $fixture -Compress
    [IO.File]::WriteAllText($generatedPath, "module FixtureData exposing (document)`n`ndocument : String`ndocument =`n    $literal`n", $utf8NoBom)
    $fixtureCreated = $true

    if ((Get-WaitBudget 10000) -le 0) { throw 'Setup consumed the component deadline.' }
    Add-Type -Path $helperPath
    $job = [Adrai.RetainedTests.OwnedJob]::new()
    $node = (Get-Command node -ErrorAction Stop).Source
    $executable = if ($Probe -eq 'spawn-failure') { $missingExecutable } else { $node }
    $arguments = [string[]]@($workerPath, $(if ($Probe -eq 'spawn-failure') { 'normal' } else { $Probe }))
    $launchCleanup = [Math]::Min(5000, (Get-WaitBudget 1000))
    if ($launchCleanup -le 0) { throw 'No launch cleanup budget remains.' }
    try {
        $root = $job.Launch($executable, $arguments, $webRoot, $null, $stdoutPath, $stderrPath, $launchCleanup)
    }
    catch [Adrai.RetainedTests.OwnedLaunchException] {
        $record.launch_failure_stage = 'Launch'
        $record.launch_failure_kind = 'OwnedLaunchException'
        $record.launch_failure_native_error = $_.Exception.NativeErrorCode
        $record.launch_failure_cleanup_verified = $_.Exception.ProcessCleanupVerified
        throw
    }
    catch [System.ComponentModel.Win32Exception] {
        $record.launch_failure_stage = 'Launch'
        $record.launch_failure_kind = 'Win32Exception'
        $record.launch_failure_native_error = $_.Exception.NativeErrorCode
        if ($Probe -eq 'spawn-failure' -and $_.Exception.NativeErrorCode -eq 2 -and $_.Exception.Message.Contains('CreateProcessW failed for ' + $missingExecutable + '.')) {
            $record.launch_failure_cleanup_verified = 'not-applicable-before-create'
        }
        throw
    }
    $record.root_pid = $root.ProcessId
    $rootBudget = if ($Probe -eq 'timeout') { [Math]::Min(750, (Get-WaitBudget 10000)) } else { Get-WaitBudget 10000 }
    if ($rootBudget -le 0) { throw 'No root wait budget remains.' }
    $record.root_exited = $root.WaitForExit($rootBudget)
    if ($record.root_exited) {
        $record.root_exit_code = $root.GetExitCode()
        $drainBudget = [Math]::Min(750, (Get-WaitBudget 5000))
        $record.job_empty_before_cleanup = $job.WaitForEmpty($drainBudget)
    }
    else {
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
            $record.job_empty_after_cleanup = $job.WaitForEmpty((Get-WaitBudget 1000))
        }
        catch {
            $record.error = (($record.error, $_.Exception.Message) | Where-Object { $_ }) -join '; '
        }
    }
    else {
        $record.job_empty_after_cleanup = $true
    }
    if ($null -ne $root) {
        try {
            $record.root_exited = $root.WaitForExit((Get-WaitBudget 1000))
            if ($record.root_exited -and $null -eq $record.root_exit_code) { $record.root_exit_code = $root.GetExitCode() }
        }
        catch {
            $record.error = (($record.error, $_.Exception.Message) | Where-Object { $_ }) -join '; '
        }
    }
    $record.cleanup_verified = $record.job_empty_after_cleanup -and ($null -eq $root -or $record.root_exited) -and ($record.launch_failure_cleanup_verified -ne $false)
    if ($null -ne $root) { $root.Dispose() }
    if ($null -ne $job) { $job.Dispose() }
    if ($record.cleanup_verified -and $fixtureCreated) {
        try {
            Remove-Item -LiteralPath $generatedPath -Force
            $record.fixture_removed = -not (Test-Path -LiteralPath $generatedPath)
        }
        catch {
            $record.error = (($record.error, $_.Exception.Message) | Where-Object { $_ }) -join '; '
        }
    }
    $record.elapsed_ms = [int]$clock.ElapsedMilliseconds
    $normalPassed = $Probe -eq 'normal' -and $record.root_exit_code -eq 0 -and $record.job_empty_before_cleanup -eq $true -and -not $record.timed_out -and -not $record.error
    $timeoutPassed = $Probe -eq 'timeout' -and $record.timed_out -and $record.job_empty_before_cleanup -eq $false
    $earlyPassed = $Probe -in @('early-success', 'early-error') -and $record.root_exit_code -eq $(if ($Probe -eq 'early-success') { 0 } else { 17 }) -and $record.job_empty_before_cleanup -eq $false
    $spawnPassed = $Probe -eq 'spawn-failure' -and $null -eq $root -and $record.launch_failure_stage -eq 'Launch' -and $record.launch_failure_kind -eq 'Win32Exception' -and $record.launch_failure_native_error -eq 2 -and $record.launch_failure_cleanup_verified -eq 'not-applicable-before-create' -and $record.error
    $record.exit_code = if (($normalPassed -or $timeoutPassed -or $earlyPassed -or $spawnPassed) -and $record.cleanup_verified -and $record.fixture_removed -and $record.elapsed_ms -le $deadlineMilliseconds) { 0 } else { 1 }
    try {
        if (Test-Path -LiteralPath $stdoutPath) {
            $record.owned_descendant_pid = if (([IO.File]::ReadAllText($stdoutPath)) -match 'owned_descendant_pid=(\d+)') { [int]$Matches[1] } else { $null }
        }
        $json = ConvertTo-Json -InputObject $record -Depth 8
        [IO.File]::WriteAllText($evidencePath, $json, $utf8NoBom)
        if (Test-Path -LiteralPath $stdoutPath) { Get-Content -LiteralPath $stdoutPath }
        if (Test-Path -LiteralPath $stderrPath) { Get-Content -LiteralPath $stderrPath }
        Write-Output ($json -replace '\s+', ' ')
        Write-Output "evidence_path=$evidencePath"
        if ($clock.ElapsedMilliseconds -gt $deadlineMilliseconds) {
            $record.exit_code = 1
            $record.elapsed_ms = [int]$clock.ElapsedMilliseconds
            [IO.File]::WriteAllText($evidencePath, (ConvertTo-Json -InputObject $record -Depth 8), $utf8NoBom)
        }
    }
    catch {
        $record.exit_code = 1
        Write-Error $_.Exception.Message
    }
}

exit $record.exit_code
