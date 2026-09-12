[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('Build', 'List', 'Focused', 'Complete', 'SelfCheck')]
    [string] $Mode,

    [Parameter()]
    [string] $RepositoryRoot,

    [Parameter()]
    [string] $LedgerPath,

    [Parameter()]
    [string] $OrdinaryPartitionsPath,

    [Parameter()]
    [string] $BuildManifestPath,

    [Parameter()]
    [string] $AdraiExe,

    [Parameter()]
    [string] $OrdinaryTestExe,

    [Parameter()]
    [string] $CacheSelectionTestExe,

    [Parameter()]
    [string] $StressTestExe,

    [Parameter()]
    [string] $BenchmarkRegistrationTestExe,

    [Parameter()]
    [string] $StackExe,

    [Parameter()]
    [ValidateSet('adrai-test', 'adrai-cache-selection-test', 'adrai-stress-test', 'adrai-benchmark-registration-test')]
    [string] $Component,

    [Parameter()]
    [string] $TestName,

    [Parameter()]
    [ValidateRange(1, 7200)]
    [int] $DeadlineSeconds,

    [Parameter()]
    [ValidateRange(1, 60)]
    [int] $CleanupReserveSeconds = 10,

    [Parameter()]
    [string] $EvidenceDirectory
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$script:Evidence = [ordered] @{
    schemaVersion = 1
    mode = $Mode
    repositoryRoot = $null
    startedUtc = [DateTime]::UtcNow.ToString('o')
    deadlineSeconds = $null
    result = 'incomplete'
    error = $null
    elapsedSeconds = $null
    invocations = [Collections.Generic.List[object]]::new()
    artifacts = [Collections.Generic.List[object]]::new()
    schedulerRuns = [Collections.Generic.List[object]]::new()
    cleanupVerified = $false
}
$script:RunTimer = $null
$script:DeadlineMilliseconds = 0L
$script:EvidenceFile = $null

function Resolve-AbsoluteDirectory {
    param([Parameter(Mandatory = $true)][string] $Path)
    $full = [IO.Path]::GetFullPath($Path)
    if (-not (Test-Path -LiteralPath $full -PathType Container)) {
        throw "Directory does not exist: $full"
    }
    return $full.TrimEnd([IO.Path]::DirectorySeparatorChar, [IO.Path]::AltDirectorySeparatorChar)
}

function Resolve-AbsoluteFile {
    param(
        [Parameter(Mandatory = $true)][string] $Path,
        [Parameter(Mandatory = $true)][string] $Label
    )
    if ([string]::IsNullOrWhiteSpace($Path)) {
        throw "$Label is required."
    }
    $full = [IO.Path]::GetFullPath($Path)
    if (-not [IO.Path]::IsPathRooted($full) -or -not (Test-Path -LiteralPath $full -PathType Leaf)) {
        throw "$Label must name an existing absolute file: $full"
    }
    return $full
}

function Test-PathInsideRoot {
    param(
        [Parameter(Mandatory = $true)][string] $Path,
        [Parameter(Mandatory = $true)][string] $Root
    )
    $fullPath = [IO.Path]::GetFullPath($Path).TrimEnd([IO.Path]::DirectorySeparatorChar, [IO.Path]::AltDirectorySeparatorChar)
    $fullRoot = [IO.Path]::GetFullPath($Root).TrimEnd([IO.Path]::DirectorySeparatorChar, [IO.Path]::AltDirectorySeparatorChar)
    return $fullPath.Equals($fullRoot, [StringComparison]::OrdinalIgnoreCase) -or
        $fullPath.StartsWith($fullRoot + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)
}

function Get-Sha256 {
    param([Parameter(Mandatory = $true)][string] $Path)
    return (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
}

function Get-BuildInputInventory {
    param([Parameter(Mandatory = $true)][string] $Root)

    $paths = [Collections.Generic.List[string]]::new()
    foreach ($relative in @('package.yaml', 'stack.yaml', 'stack.yaml.lock')) {
        $candidate = Join-Path $Root $relative
        if (Test-Path -LiteralPath $candidate -PathType Leaf) { $paths.Add($candidate) }
    }
    foreach ($directoryName in @('src', 'app', 'bench', 'test')) {
        $directory = Join-Path $Root $directoryName
        if (-not (Test-Path -LiteralPath $directory -PathType Container)) {
            throw "Required build-input directory is missing: $directory"
        }
        foreach ($file in Get-ChildItem -LiteralPath $directory -Recurse -Force -File) {
            if (($file.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {
                throw "Build-input inventory refuses a reparse-point file: $($file.FullName)"
            }
            $relative = $file.FullName.Substring($Root.Length).TrimStart(
                [IO.Path]::DirectorySeparatorChar,
                [IO.Path]::AltDirectorySeparatorChar
            ).Replace([IO.Path]::DirectorySeparatorChar, '/')
            if ($relative -notmatch '(^|/)\.stack-work(/|$)') { $paths.Add($file.FullName) }
        }
    }

    [string[]]$uniquePaths = @($paths | Select-Object -Unique)
    [Array]::Sort($uniquePaths, [StringComparer]::OrdinalIgnoreCase)
    $records = [Collections.Generic.List[object]]::new()
    foreach ($path in $uniquePaths) {
        $relative = $path.Substring($Root.Length).TrimStart(
            [IO.Path]::DirectorySeparatorChar,
            [IO.Path]::AltDirectorySeparatorChar
        ).Replace([IO.Path]::DirectorySeparatorChar, '/')
        $records.Add([ordered]@{ path = $relative; sha256 = Get-Sha256 $path })
    }
    if ($records.Count -eq 0) { throw 'Build-input inventory is unexpectedly empty.' }
    return $records.ToArray()
}

function Assert-MatchingBuildInputs {
    param(
        [Parameter(Mandatory = $true)][object[]] $Expected,
        [Parameter(Mandatory = $true)][object[]] $Actual,
        [Parameter(Mandatory = $true)][string] $Context
    )
    if ($Expected.Count -ne $Actual.Count) {
        throw "$Context build-input count changed: expected $($Expected.Count), observed $($Actual.Count)."
    }
    for ($index = 0; $index -lt $Expected.Count; $index++) {
        if ([string]$Expected[$index].path -cne [string]$Actual[$index].path -or
            [string]$Expected[$index].sha256 -cne [string]$Actual[$index].sha256) {
            throw "$Context build input changed at ordinal index $index."
        }
    }
}

function Get-GitDirectory {
    param([Parameter(Mandatory = $true)][string] $Root)
    $marker = Join-Path $Root '.git'
    if (Test-Path -LiteralPath $marker -PathType Container) {
        return [IO.Path]::GetFullPath($marker)
    }
    if (-not (Test-Path -LiteralPath $marker -PathType Leaf)) {
        throw "Git marker is missing: $marker"
    }
    $line = [IO.File]::ReadAllText($marker).Trim()
    if ($line -cnotmatch '^gitdir: (.+)$') {
        throw "Git marker is malformed: $marker"
    }
    $candidate = $Matches[1]
    if (-not [IO.Path]::IsPathRooted($candidate)) {
        $candidate = Join-Path $Root $candidate
    }
    return Resolve-AbsoluteDirectory -Path $candidate
}

function Get-GitHeadFromFiles {
    param([Parameter(Mandatory = $true)][string] $Root)
    $gitDirectory = Get-GitDirectory -Root $Root
    $head = [IO.File]::ReadAllText((Join-Path $gitDirectory 'HEAD')).Trim()
    if ($head -cmatch '^[0-9a-f]{40}$') {
        return $head
    }
    if ($head -cnotmatch '^ref: (refs/.+)$') {
        throw 'Git HEAD is neither a lowercase commit ID nor a symbolic ref.'
    }
    $refName = $Matches[1]
    $looseRef = Join-Path $gitDirectory ($refName.Replace('/', [IO.Path]::DirectorySeparatorChar))
    if (Test-Path -LiteralPath $looseRef -PathType Leaf) {
        $value = [IO.File]::ReadAllText($looseRef).Trim()
        if ($value -cnotmatch '^[0-9a-f]{40}$') { throw "Malformed loose ref: $refName" }
        return $value
    }
    $packedRefs = Join-Path $gitDirectory 'packed-refs'
    if (Test-Path -LiteralPath $packedRefs -PathType Leaf) {
        foreach ($line in [IO.File]::ReadAllLines($packedRefs)) {
            if ($line -cmatch "^([0-9a-f]{40}) $([regex]::Escape($refName))$") {
                return $Matches[1]
            }
        }
    }
    throw "Unable to resolve symbolic HEAD ref from Git files: $refName"
}

function New-EvidenceDirectory {
    param(
        [Parameter(Mandatory = $true)][string] $Root,
        [string] $Requested
    )
    if ([string]::IsNullOrWhiteSpace($Requested)) {
        $Requested = Join-Path ([IO.Path]::GetTempPath()) ("adrai-retained-tests-{0}" -f [Guid]::NewGuid().ToString('N'))
    }
    $full = [IO.Path]::GetFullPath($Requested)
    if (Test-PathInsideRoot -Path $full -Root $Root) {
        throw "Evidence directory must be outside the repository: $full"
    }
    [IO.Directory]::CreateDirectory($full) | Out-Null
    return $full
}

function Write-Evidence {
    if ($null -eq $script:EvidenceFile) { return }
    if ($null -ne $script:RunTimer) {
        $script:Evidence.elapsedSeconds = [Math]::Round($script:RunTimer.Elapsed.TotalSeconds, 6)
    }
    $json = $script:Evidence | ConvertTo-Json -Depth 12
    [IO.File]::WriteAllText($script:EvidenceFile, $json + [Environment]::NewLine, [Text.UTF8Encoding]::new($false))
}

function Get-RemainingMilliseconds {
    if ($null -eq $script:RunTimer) { throw 'The monotonic run timer has not started.' }
    return $script:DeadlineMilliseconds - $script:RunTimer.ElapsedMilliseconds
}

function Get-ProcessEnvironment {
    param(
        [string] $ResolvedAdraiExe,
        [bool] $ConstrainRuntime = $true
    )
    $environment = @{}
    foreach ($entry in Get-ChildItem Env:) {
        if ($entry.Name.StartsWith('TASTY_', [StringComparison]::OrdinalIgnoreCase)) {
            $environment[$entry.Name] = $null
        }
    }
    $environment.ADRAI_TEST_LARGE_VALIDATION_EVIDENCE = $null
    $environment.STACK_YAML = $null
    if ($ConstrainRuntime) {
        $environment.TASTY_NUM_THREADS = '1'
        $environment.GHCRTS = '-N1'
    }
    if (-not [string]::IsNullOrWhiteSpace($ResolvedAdraiExe)) {
        $environment.ADRAI_EXE = $ResolvedAdraiExe
    }
    return $environment
}

function Test-AggregateCleanupVerified {
    param(
        [AllowNull()][object] $LaunchFailureCleanupVerified,
        [bool] $JobCleanupVerified
    )
    return $JobCleanupVerified -and
        ($null -eq $LaunchFailureCleanupVerified -or [bool]$LaunchFailureCleanupVerified)
}

function Invoke-OwnedProcess {
    param(
        [Parameter(Mandatory = $true)][string] $Id,
        [Parameter(Mandatory = $true)][string] $Executable,
        [Parameter(Mandatory = $true)][AllowEmptyCollection()][string[]] $Arguments,
        [Parameter(Mandatory = $true)][string] $WorkingDirectory,
        [Parameter(Mandatory = $true)][hashtable] $Environment,
        [int] $InvocationLimitSeconds = 0
    )

    $safeId = $Id -replace '[^A-Za-z0-9_.-]', '_'
    $stdoutPath = Join-Path $script:EvidenceDirectory ("{0}.stdout.txt" -f $safeId)
    $stderrPath = Join-Path $script:EvidenceDirectory ("{0}.stderr.txt" -f $safeId)
    $record = [ordered] @{
        id = $Id
        executable = $Executable
        arguments = @($Arguments)
        startedUtc = [DateTime]::UtcNow.ToString('o')
        launched = $false
        launchFailureCleanupVerified = $null
        pid = $null
        exitCode = $null
        timedOut = $false
        orphanedDescendants = $false
        orphanFailureSnapshot = $null
        cleanupVerified = $false
        durationSeconds = $null
        stdout = $stdoutPath
        stderr = $stderrPath
    }
    $invocationTimer = [Diagnostics.Stopwatch]::StartNew()
    $job = $null
    $process = $null
    $cleanupProblem = $null
    try {
        $remaining = Get-RemainingMilliseconds
        $cleanupReserveMilliseconds = [int64]$CleanupReserveSeconds * 1000L
        $usable = $remaining - $cleanupReserveMilliseconds
        if ($usable -le 0) {
            throw "The global deadline has no time left before the cleanup reserve for '$Id'."
        }
        if ($InvocationLimitSeconds -gt 0) {
            $usable = [Math]::Min($usable, [int64]$InvocationLimitSeconds * 1000L)
        }
        $invocationBudget = $usable

        $job = [Adrai.RetainedTests.OwnedJob]::new()
        $launchCleanup = [int][Math]::Min($cleanupReserveMilliseconds, [Math]::Max(0L, (Get-RemainingMilliseconds)))
        try {
            $process = $job.Launch($Executable, $Arguments, $WorkingDirectory, $Environment, $stdoutPath, $stderrPath, $launchCleanup)
        }
        catch [Adrai.RetainedTests.OwnedLaunchException] {
            $record.launchFailureCleanupVerified = $_.Exception.ProcessCleanupVerified
            throw
        }
        $record.launched = $true
        $record.pid = $process.ProcessId
        $globalUsableAfterLaunch = (Get-RemainingMilliseconds) - $cleanupReserveMilliseconds
        $invocationUsableAfterLaunch = $invocationBudget - $invocationTimer.ElapsedMilliseconds
        $waitMilliseconds = [Math]::Min($globalUsableAfterLaunch, $invocationUsableAfterLaunch)
        $rootExited = $false
        $rootExitObservedAtMilliseconds = $null
        if ($waitMilliseconds -gt 0) {
            $rootExited = $process.WaitForExit([int][Math]::Min($waitMilliseconds, [int]::MaxValue))
        }
        if (-not $rootExited) {
            $record.timedOut = $true
            $job.Terminate(124)
        }
        else {
            $rootExitObservedAtMilliseconds = $script:RunTimer.ElapsedMilliseconds
            $record.exitCode = $process.GetExitCode()
        }
        $process.Dispose()
        $process = $null

        if (-not $record.timedOut -and $job.ActiveProcessCount() -gt 0) {
            $grace = [int][Math]::Min(250L, [Math]::Max(0L, (Get-RemainingMilliseconds) - $cleanupReserveMilliseconds))
            if ($grace -gt 0) { [void]$job.WaitForEmpty($grace) }
            $orphanActiveProcessCount = $job.ActiveProcessCount()
            if ($orphanActiveProcessCount -gt 0) {
                $record.orphanedDescendants = $true
                $orphanTriggerMilliseconds = $script:RunTimer.ElapsedMilliseconds
                try {
                    $record.orphanFailureSnapshot = [ordered] @{
                        triggerMonotonicMilliseconds = $orphanTriggerMilliseconds
                        rootExitObservedMonotonicMilliseconds = $rootExitObservedAtMilliseconds
                        rootPid = $record.pid
                        activeProcessCount = $orphanActiveProcessCount
                        captureError = $null
                        job = $job.CaptureFailureSnapshot()
                    }
                }
                catch {
                    $record.orphanFailureSnapshot = [ordered] @{
                        triggerMonotonicMilliseconds = $orphanTriggerMilliseconds
                        rootExitObservedMonotonicMilliseconds = $rootExitObservedAtMilliseconds
                        rootPid = $record.pid
                        activeProcessCount = $orphanActiveProcessCount
                        captureError = $_.Exception.Message
                        job = $null
                    }
                }
                $job.Terminate(125)
            }
        }

        $cleanupRemaining = [int][Math]::Min(
            [int64]$CleanupReserveSeconds * 1000L,
            [Math]::Max(0L, (Get-RemainingMilliseconds))
        )
        if ($cleanupRemaining -gt 0) {
            $record.cleanupVerified = $job.WaitForEmpty($cleanupRemaining)
        }
        else {
            $record.cleanupVerified = ($job.ActiveProcessCount() -eq 0)
        }
        if (-not $record.cleanupVerified) {
            throw "Owned process tree '$Id' did not become empty before the global deadline."
        }
        return [pscustomobject]$record
    }
    finally {
        if ($null -eq $job) {
            $record.cleanupVerified = $true
        }
        elseif (-not $record.cleanupVerified) {
            try {
                if ($job.ActiveProcessCount() -gt 0) { $job.Terminate(125) }
                $finalCleanupMilliseconds = [int][Math]::Min(
                    [int64]$CleanupReserveSeconds * 1000L,
                    [Math]::Max(0L, (Get-RemainingMilliseconds))
                )
                if ($finalCleanupMilliseconds -gt 0) {
                    $record.cleanupVerified = $job.WaitForEmpty($finalCleanupMilliseconds)
                }
                else {
                    $record.cleanupVerified = ($job.ActiveProcessCount() -eq 0)
                }
                if (-not $record.cleanupVerified) {
                    $cleanupProblem = "Owned process tree '$Id' did not become empty before the global deadline."
                }
            }
            catch {
                $cleanupProblem = "Owned process tree '$Id' cleanup failed: $($_.Exception.Message)"
            }
        }
        $record.cleanupVerified = Test-AggregateCleanupVerified `
            -LaunchFailureCleanupVerified $record.launchFailureCleanupVerified `
            -JobCleanupVerified $record.cleanupVerified
        if (-not $record.cleanupVerified -and $record.launchFailureCleanupVerified -eq $false) {
            $cleanupProblem = "Unassigned process cleanup for '$Id' was not verified."
        }
        if ($null -ne $process) { $process.Dispose() }
        if ($null -ne $job) { $job.Dispose() }
        $invocationTimer.Stop()
        $record.durationSeconds = [Math]::Round($invocationTimer.Elapsed.TotalSeconds, 6)
        $script:Evidence.invocations.Add([pscustomobject]$record)
        if ($null -ne $cleanupProblem) { throw $cleanupProblem }
    }
}

function New-OwnedProcessState {
    param(
        [Parameter(Mandatory = $true)][string] $Id,
        [Parameter(Mandatory = $true)][AllowEmptyCollection()][string[]] $Arguments,
        [Parameter(Mandatory = $true)][ValidateSet('normal', 'exclusive')][string] $Classification,
        [Parameter(Mandatory = $true)][string] $Executable,
        [Parameter(Mandatory = $true)][int] $ExpectedCount,
        [int] $InvocationLimitSeconds = 0
    )

    $safeId = $Id -replace '[^A-Za-z0-9_.-]', '_'
    $stdoutPath = Join-Path $script:EvidenceDirectory ("{0}.stdout.txt" -f $safeId)
    $stderrPath = Join-Path $script:EvidenceDirectory ("{0}.stderr.txt" -f $safeId)
    $record = [ordered] @{
        id = $Id
        executable = $Executable
        arguments = @($Arguments)
        schedulerClass = $Classification
        startedUtc = [DateTime]::UtcNow.ToString('o')
        launched = $false
        launchFailureCleanupVerified = $null
        pid = $null
        exitCode = $null
        timedOut = $false
        orphanedDescendants = $false
        orphanFailureSnapshot = $null
        cancelledByCoordinator = $false
        cleanupVerified = $false
        durationSeconds = $null
        stdout = $stdoutPath
        stderr = $stderrPath
    }
    return [pscustomobject]@{
        Id = $Id
        Classification = $Classification
        Record = $record
        Timer = [Diagnostics.Stopwatch]::StartNew()
        Job = $null
        Process = $null
        RootExitedAtMilliseconds = $null
        InvocationLimitMilliseconds = $(if ($InvocationLimitSeconds -gt 0) { [int64]$InvocationLimitSeconds * 1000L } else { 0L })
        ExpectedCount = $ExpectedCount
        Finalized = $false
    }
}

function Start-OwnedProcessState {
    param(
        [Parameter(Mandatory = $true)][pscustomobject] $State,
        [Parameter(Mandatory = $true)][string] $Executable,
        [Parameter(Mandatory = $true)][AllowEmptyCollection()][string[]] $Arguments,
        [Parameter(Mandatory = $true)][string] $WorkingDirectory,
        [Parameter(Mandatory = $true)][hashtable] $Environment
    )

    $cleanupReserveMilliseconds = [int64]$CleanupReserveSeconds * 1000L
    if ((Get-RemainingMilliseconds) - $cleanupReserveMilliseconds -le 0) {
        throw "The global deadline has no time left before launching '$($State.Id)'."
    }
    $State.Job = [Adrai.RetainedTests.OwnedJob]::new()
    $launchCleanup = [int][Math]::Min($cleanupReserveMilliseconds, [Math]::Max(0L, (Get-RemainingMilliseconds)))
    try {
        $State.Process = $State.Job.Launch(
            $Executable,
            $Arguments,
            $WorkingDirectory,
            $Environment,
            $State.Record.stdout,
            $State.Record.stderr,
            $launchCleanup
        )
    }
    catch [Adrai.RetainedTests.OwnedLaunchException] {
        $State.Record.launchFailureCleanupVerified = $_.Exception.ProcessCleanupVerified
        throw
    }
    $State.Record.launched = $true
    $State.Record.pid = $State.Process.ProcessId
    return $State
}

function Get-OwnedProcessStateDisposition {
    param([Parameter(Mandatory = $true)][pscustomobject] $State)

    if ($null -ne $State.Process -and $State.Process.WaitForExit(0)) {
        $State.Record.exitCode = $State.Process.GetExitCode()
        $State.Process.Dispose()
        $State.Process = $null
        $State.RootExitedAtMilliseconds = $script:RunTimer.ElapsedMilliseconds
    }
    if ($null -ne $State.Process) {
        if ($State.InvocationLimitMilliseconds -gt 0 -and $State.Timer.ElapsedMilliseconds -ge $State.InvocationLimitMilliseconds) {
            $State.Record.timedOut = $true
            return 'failed'
        }
        return 'running'
    }
    if ($State.Record.exitCode -ne 0) { return 'failed' }
    $activeProcessCount = $State.Job.ActiveProcessCount()
    if ($activeProcessCount -eq 0) { return 'succeeded' }
    if ($null -ne $State.RootExitedAtMilliseconds -and
        $script:RunTimer.ElapsedMilliseconds - $State.RootExitedAtMilliseconds -ge 250L) {
        $State.Record.orphanedDescendants = $true
        $orphanTriggerMilliseconds = $script:RunTimer.ElapsedMilliseconds
        try {
            $State.Record.orphanFailureSnapshot = [ordered] @{
                triggerMonotonicMilliseconds = $orphanTriggerMilliseconds
                rootExitObservedMonotonicMilliseconds = $State.RootExitedAtMilliseconds
                rootPid = $State.Record.pid
                activeProcessCount = $activeProcessCount
                captureError = $null
                job = $State.Job.CaptureFailureSnapshot()
            }
        }
        catch {
            $State.Record.orphanFailureSnapshot = [ordered] @{
                triggerMonotonicMilliseconds = $orphanTriggerMilliseconds
                rootExitObservedMonotonicMilliseconds = $State.RootExitedAtMilliseconds
                rootPid = $State.Record.pid
                activeProcessCount = $activeProcessCount
                captureError = $_.Exception.Message
                job = $null
            }
        }
        return 'failed'
    }
    return 'running'
}

function Complete-OwnedProcessState {
    param(
        [Parameter(Mandatory = $true)][pscustomobject] $State,
        [bool] $AllowUnverifiedCleanup = $false
    )

    if ($State.Finalized) { return [pscustomobject]$State.Record }
    if ($null -ne $State.Process) {
        if ($State.Process.WaitForExit(0)) { $State.Record.exitCode = $State.Process.GetExitCode() }
        $State.Process.Dispose()
        $State.Process = $null
    }
    $jobCleanupVerified = $null -eq $State.Job
    if ($null -ne $State.Job) {
        try { $jobCleanupVerified = $State.Job.ActiveProcessCount() -eq 0 }
        catch { $jobCleanupVerified = $false }
    }
    $State.Record.cleanupVerified = Test-AggregateCleanupVerified `
        -LaunchFailureCleanupVerified $State.Record.launchFailureCleanupVerified `
        -JobCleanupVerified $jobCleanupVerified
    if ($null -ne $State.Job) { $State.Job.Dispose() }
    $State.Job = $null
    $State.Timer.Stop()
    $State.Record.durationSeconds = [Math]::Round($State.Timer.Elapsed.TotalSeconds, 6)
    $State.Finalized = $true
    $result = [pscustomobject]$State.Record
    $script:Evidence.invocations.Add($result)
    if (-not $result.cleanupVerified -and -not $AllowUnverifiedCleanup) {
        throw "Owned process tree '$($State.Id)' was finalized before cleanup was verified."
    }
    return $result
}

function Stop-AllOwnedProcessStates {
    param(
        [Parameter(Mandatory = $true)][AllowEmptyCollection()][Collections.Generic.List[object]] $States,
        [Parameter(Mandatory = $true)][string] $Reason
    )

    $problems = [Collections.Generic.List[string]]::new()
    # Terminate every active Job first. Waiting for any one tree before issuing
    # the other termination requests could consume the shared cleanup budget.
    foreach ($state in @($States.ToArray())) {
        $state.Record.cancelledByCoordinator = $true
        try {
            if ($null -ne $state.Job -and $state.Job.ActiveProcessCount() -gt 0) { $state.Job.Terminate(125) }
        }
        catch { $problems.Add("$($state.Id): terminate failed: $($_.Exception.Message)") }
    }

    do {
        $remainingStates = [Collections.Generic.List[object]]::new()
        foreach ($state in @($States.ToArray())) {
            if ($null -eq $state.Job) { continue }
            try {
                if ($state.Job.ActiveProcessCount() -gt 0) { $remainingStates.Add($state) }
            }
            catch { $problems.Add("$($state.Id): cleanup poll failed: $($_.Exception.Message)") }
        }
        if ($remainingStates.Count -eq 0) { break }
        $remainingMilliseconds = Get-RemainingMilliseconds
        if ($remainingMilliseconds -le 0) { break }
        foreach ($state in @($remainingStates.ToArray())) {
            $slice = [int][Math]::Min(25L, [Math]::Max(0L, (Get-RemainingMilliseconds)))
            if ($slice -le 0) { break }
            try { [void]$state.Job.WaitForEmpty($slice) }
            catch { $problems.Add("$($state.Id): cleanup wait failed: $($_.Exception.Message)") }
        }
    } while ($true)

    foreach ($state in @($States.ToArray())) {
        try {
            $result = Complete-OwnedProcessState -State $state -AllowUnverifiedCleanup $true
            if (-not $result.cleanupVerified) { $problems.Add("$($state.Id): owned descendants remained after '$Reason'.") }
        }
        catch { $problems.Add("$($state.Id): finalization failed: $($_.Exception.Message)") }
    }
    $States.Clear()
    return $problems.ToArray()
}

function Invoke-OwnedProcessQueue {
    param(
        [Parameter(Mandatory = $true)][object[]] $Jobs,
        [Parameter(Mandatory = $true)][ValidateRange(1, 3)][int] $MaximumActiveJobs,
        [Parameter(Mandatory = $true)][string] $RunId
    )

    $runEvidence = [pscustomobject][ordered]@{
        id = $RunId
        maximumActiveJobs = $MaximumActiveJobs
        queue = @($Jobs | ForEach-Object { [string]$_.id })
        peakActiveJobs = 0
        completed = [Collections.Generic.List[string]]::new()
        result = 'incomplete'
        cleanupVerified = $false
    }
    $script:Evidence.schedulerRuns.Add($runEvidence)
    $active = [Collections.Generic.List[object]]::new()
    $nextIndex = 0
    $pollSignal = [Threading.ManualResetEvent]::new($false)
    try {
        while ($nextIndex -lt $Jobs.Count -or $active.Count -gt 0) {
            $cleanupReserveMilliseconds = [int64]$CleanupReserveSeconds * 1000L
            if ((Get-RemainingMilliseconds) - $cleanupReserveMilliseconds -le 0) {
                foreach ($state in @($active.ToArray())) { $state.Record.timedOut = $true }
                throw "Scheduler '$RunId' reached the shared deadline before its cleanup reserve."
            }

            $exclusiveActive = @($active.ToArray() | Where-Object { $_.Classification -ceq 'exclusive' }).Count -gt 0
            while (-not $exclusiveActive -and $nextIndex -lt $Jobs.Count -and $active.Count -lt $MaximumActiveJobs) {
                $definition = $Jobs[$nextIndex]
                if ([string]$definition.classification -ceq 'exclusive' -and $active.Count -gt 0) { break }
                $state = New-OwnedProcessState `
                    -Id ([string]$definition.id) `
                    -Executable ([string]$definition.executable) `
                    -Arguments @($definition.arguments | ForEach-Object { [string]$_ }) `
                    -Classification ([string]$definition.classification) `
                    -ExpectedCount ([int]$definition.expectedCount) `
                    -InvocationLimitSeconds ([int]$definition.invocationLimitSeconds)
                $active.Add($state)
                [void](Start-OwnedProcessState `
                    -State $state `
                    -Executable ([string]$definition.executable) `
                    -Arguments @($definition.arguments | ForEach-Object { [string]$_ }) `
                    -WorkingDirectory ([string]$definition.workingDirectory) `
                    -Environment $definition.environment)
                $nextIndex++
                if ($active.Count -gt $runEvidence.peakActiveJobs) { $runEvidence.peakActiveJobs = $active.Count }
                if ($state.Classification -ceq 'exclusive') { $exclusiveActive = $true; break }
            }

            $madeProgress = $false
            foreach ($state in @($active.ToArray())) {
                $disposition = Get-OwnedProcessStateDisposition -State $state
                if ($disposition -ceq 'failed') {
                    if ($state.Record.timedOut) { throw "Invocation '$($state.Id)' timed out." }
                    if ($state.Record.orphanedDescendants) { throw "Invocation '$($state.Id)' left a descendant running after its root exited." }
                    throw "Invocation '$($state.Id)' exited with code $($state.Record.exitCode)."
                }
                if ($disposition -ceq 'succeeded') {
                    $result = Complete-OwnedProcessState -State $state
                    [void]$active.Remove($state)
                    Assert-SuccessfulInvocation $result
                    if ($state.ExpectedCount -ge 0) { Assert-TastyExecutionCount $result $state.ExpectedCount }
                    $runEvidence.completed.Add($state.Id)
                    $madeProgress = $true
                }
            }
            if (-not $madeProgress -and $active.Count -gt 0) { [void]$pollSignal.WaitOne(10) }
        }
        $runEvidence.result = 'passed'
        $runEvidence.cleanupVerified = $true
    }
    catch {
        $failure = $_
        [string[]]$cleanupProblems = @(Stop-AllOwnedProcessStates -States $active -Reason $failure.Exception.Message)
        $runEvidence.result = 'failed'
        $runEvidence.cleanupVerified = ($cleanupProblems.Count -eq 0)
        if ($cleanupProblems.Count -gt 0) {
            throw "$($failure.Exception.Message) Cleanup failures: $($cleanupProblems -join ' | ')"
        }
        throw $failure
    }
    finally {
        $pollSignal.Dispose()
    }
}

function Assert-SuccessfulInvocation {
    param([Parameter(Mandatory = $true)][pscustomobject] $Result)
    if ($Result.timedOut) { throw "Invocation '$($Result.id)' timed out." }
    if ($Result.orphanedDescendants) { throw "Invocation '$($Result.id)' left a descendant running after its root exited." }
    if (-not $Result.cleanupVerified) { throw "Invocation '$($Result.id)' cleanup was not verified." }
    if ($Result.exitCode -ne 0) { throw "Invocation '$($Result.id)' exited with code $($Result.exitCode)." }
}

function Assert-TastyExecutionCount {
    param(
        [Parameter(Mandatory = $true)][pscustomobject] $Result,
        [Parameter(Mandatory = $true)][int] $ExpectedCount
    )
    $text = [IO.File]::ReadAllText($Result.stdout)
    $summaries = [Regex]::Matches($text, '(?m)^All ([0-9]+) tests passed(?:\s|$)')
    if ($summaries.Count -ne 1) {
        throw "Invocation '$($Result.id)' did not emit exactly one successful Tasty execution summary."
    }
    $observed = [int]::Parse($summaries[0].Groups[1].Value, [Globalization.CultureInfo]::InvariantCulture)
    if ($observed -ne $ExpectedCount) {
        throw "Invocation '$($Result.id)' executed $observed tests; expected $ExpectedCount."
    }
    $Result | Add-Member -NotePropertyName observedTestCount -NotePropertyValue $observed -Force
    $evidenceRecord = @($script:Evidence.invocations | Where-Object { $_.id -ceq $Result.id })[-1]
    $evidenceRecord | Add-Member -NotePropertyName observedTestCount -NotePropertyValue $observed -Force
}

function Get-ArtifactPaths {
    return [ordered] @{
        adrai = Resolve-AbsoluteFile -Path $AdraiExe -Label 'AdraiExe'
        ordinary = Resolve-AbsoluteFile -Path $OrdinaryTestExe -Label 'OrdinaryTestExe'
        cacheSelection = Resolve-AbsoluteFile -Path $CacheSelectionTestExe -Label 'CacheSelectionTestExe'
        stress = Resolve-AbsoluteFile -Path $StressTestExe -Label 'StressTestExe'
        benchmarkRegistration = Resolve-AbsoluteFile -Path $BenchmarkRegistrationTestExe -Label 'BenchmarkRegistrationTestExe'
    }
}

function Get-ExpectedStackArtifacts {
    param(
        [Parameter(Mandatory = $true)][string] $Root,
        [Parameter(Mandatory = $true)][string] $DistDirectory
    )
    $fullDist = [IO.Path]::GetFullPath($(if ([IO.Path]::IsPathRooted($DistDirectory)) { $DistDirectory } else { Join-Path $Root $DistDirectory }))
    $requiredPrefix = [IO.Path]::GetFullPath((Join-Path $Root '.stack-work\dist')).TrimEnd('\') + '\'
    if (-not $fullDist.StartsWith($requiredPrefix, [StringComparison]::OrdinalIgnoreCase)) {
        throw "Stack dist directory is outside the repository's .stack-work/dist tree: $fullDist"
    }
    return [ordered]@{
        adrai = [IO.Path]::GetFullPath((Join-Path $fullDist 'build\adrai\adrai.exe'))
        ordinary = [IO.Path]::GetFullPath((Join-Path $fullDist 'build\adrai-test\adrai-test.exe'))
        cacheSelection = [IO.Path]::GetFullPath((Join-Path $fullDist 'build\adrai-cache-selection-test\adrai-cache-selection-test.exe'))
        stress = [IO.Path]::GetFullPath((Join-Path $fullDist 'build\adrai-stress-test\adrai-stress-test.exe'))
        benchmarkRegistration = [IO.Path]::GetFullPath((Join-Path $fullDist 'build\adrai-benchmark-registration-test\adrai-benchmark-registration-test.exe'))
    }
}

function Assert-StackArtifactPaths {
    param(
        [Parameter(Mandatory = $true)][Collections.IDictionary] $Explicit,
        [Parameter(Mandatory = $true)][Collections.IDictionary] $Expected
    )
    foreach ($role in @('adrai', 'ordinary', 'cacheSelection', 'stress', 'benchmarkRegistration')) {
        if (-not ([string]$Explicit[$role]).Equals([string]$Expected[$role], [StringComparison]::OrdinalIgnoreCase)) {
            throw "Explicit '$role' executable is not the corresponding Stack dist artifact. Expected '$($Expected[$role])'."
        }
    }
}

function Get-StackDistDirectory {
    param([Parameter(Mandatory = $true)][string] $ResolvedStack)
    $stackYaml = Join-Path $script:RepositoryRoot 'stack.yaml'
    $result = Invoke-OwnedProcess -Id 'stack-path-dist-dir' -Executable $ResolvedStack -Arguments @('--stack-yaml', $stackYaml, 'path', '--dist-dir') -WorkingDirectory $script:RepositoryRoot -Environment (Get-ProcessEnvironment $null $false)
    Assert-SuccessfulInvocation $result
    [string[]]$lines = @([IO.File]::ReadAllLines($result.stdout) | Where-Object { -not [string]::IsNullOrWhiteSpace($_) })
    if ($lines.Count -ne 1) { throw 'stack path --dist-dir did not emit exactly one path.' }
    return [IO.Path]::GetFullPath($(if ([IO.Path]::IsPathRooted($lines[0])) { $lines[0] } else { Join-Path $script:RepositoryRoot $lines[0] }))
}

function New-BuildManifest {
    param(
        [Parameter(Mandatory = $true)][string] $Root,
        [Parameter(Mandatory = $true)][Collections.IDictionary] $Artifacts,
        [Parameter(Mandatory = $true)][object[]] $BuildInputs,
        [Parameter(Mandatory = $true)][string] $StackDistDirectory
    )
    $artifactRecords = [Collections.Generic.List[object]]::new()
    foreach ($role in $Artifacts.Keys) {
        $path = [string]$Artifacts[$role]
        $artifactRecords.Add([ordered]@{ role = $role; path = $path; sha256 = Get-Sha256 $path })
    }
    return [ordered] @{
        schemaVersion = 1
        artifactOrigin = 'stack-dist'
        stackDistDirectory = $StackDistDirectory
        repositoryRoot = $Root
        gitHead = Get-GitHeadFromFiles -Root $Root
        packageSha256 = Get-Sha256 (Join-Path $Root 'package.yaml')
        stackSha256 = Get-Sha256 (Join-Path $Root 'stack.yaml')
        buildInputs = $BuildInputs
        artifacts = $artifactRecords
    }
}

function Read-And-VerifyBuildManifest {
    param(
        [Parameter(Mandatory = $true)][string] $Root,
        [Parameter(Mandatory = $true)][Collections.IDictionary] $Artifacts
    )
    $path = Resolve-AbsoluteFile -Path $BuildManifestPath -Label 'BuildManifestPath'
    $manifest = [IO.File]::ReadAllText($path) | ConvertFrom-Json
    if ($manifest.schemaVersion -ne 1) { throw 'Unsupported build manifest schemaVersion.' }
    if ([string]$manifest.artifactOrigin -cne 'stack-dist') { throw 'Build manifest artifact origin is not Stack dist.' }
    if (-not ([string]$manifest.repositoryRoot).Equals($Root, [StringComparison]::OrdinalIgnoreCase)) {
        throw 'Build manifest repositoryRoot does not match the requested repository.'
    }
    if ([string]$manifest.gitHead -cne (Get-GitHeadFromFiles -Root $Root)) { throw 'Build manifest Git HEAD is stale.' }
    if ([string]$manifest.packageSha256 -cne (Get-Sha256 (Join-Path $Root 'package.yaml'))) { throw 'Build manifest package.yaml hash is stale.' }
    if ([string]$manifest.stackSha256 -cne (Get-Sha256 (Join-Path $Root 'stack.yaml'))) { throw 'Build manifest stack.yaml hash is stale.' }
    [object[]]$manifestInputs = @($manifest.buildInputs)
    if ($manifestInputs.Count -eq 0) { throw 'Build manifest buildInputs are missing.' }
    [object[]]$currentInputs = @(Get-BuildInputInventory -Root $Root)
    Assert-MatchingBuildInputs -Expected $manifestInputs -Actual $currentInputs -Context 'Pre-execution'
    $expectedStackArtifacts = Get-ExpectedStackArtifacts -Root $Root -DistDirectory ([string]$manifest.stackDistDirectory)
    Assert-StackArtifactPaths -Explicit $Artifacts -Expected $expectedStackArtifacts

    $expectedRoles = @('adrai', 'ordinary', 'cacheSelection', 'stress', 'benchmarkRegistration')
    if (@($manifest.artifacts).Count -ne $expectedRoles.Count) { throw 'Build manifest must contain exactly five artifacts.' }
    foreach ($role in $expectedRoles) {
        $entries = @($manifest.artifacts | Where-Object { [string]$_.role -ceq $role })
        if ($entries.Count -ne 1) { throw "Build manifest must contain exactly one '$role' artifact." }
        $actualPath = [string]$Artifacts[$role]
        if (-not ([IO.Path]::GetFullPath([string]$entries[0].path)).Equals($actualPath, [StringComparison]::OrdinalIgnoreCase)) {
            throw "Explicit '$role' path does not match the frozen build manifest."
        }
        $actualHash = Get-Sha256 $actualPath
        if ([string]$entries[0].sha256 -cne $actualHash) { throw "Artifact hash mismatch for '$role'." }
        $script:Evidence.artifacts.Add([ordered]@{ role = $role; path = $actualPath; sha256 = $actualHash })
    }
    return $manifest
}

function ConvertTo-OrdinalUniqueStrings {
    param(
        [Parameter(Mandatory = $true)][object[]] $Values,
        [Parameter(Mandatory = $true)][string] $Label
    )
    $set = [Collections.Generic.HashSet[string]]::new([StringComparer]::Ordinal)
    $result = [Collections.Generic.List[string]]::new()
    foreach ($raw in $Values) {
        $value = [string]$raw
        if ([string]::IsNullOrWhiteSpace($value) -or $value.Contains("`r") -or $value.Contains("`n")) {
            throw "$Label contains an empty or multiline name."
        }
        if (-not $set.Add($value)) { throw "$Label contains duplicate '$value'." }
        $result.Add($value)
    }
    if ($result.Count -eq 0) { throw "$Label must not be empty." }
    return $result.ToArray()
}

function Read-And-VerifyLedger {
    $path = Resolve-AbsoluteFile -Path $LedgerPath -Label 'LedgerPath'
    $ledger = [IO.File]::ReadAllText($path) | ConvertFrom-Json
    if ($ledger.schemaVersion -ne 1) { throw 'Unsupported retained-suite ledger schemaVersion.' }
    $components = @($ledger.components)
    $required = [ordered]@{
        'adrai-test' = @{ role = 'ordinary'; args = @() }
        'adrai-cache-selection-test' = @{ role = 'cacheSelection'; args = @() }
        'adrai-stress-test' = @{ role = 'stress'; args = @('--run-stress') }
        'adrai-benchmark-registration-test' = @{ role = 'benchmarkRegistration'; args = @() }
    }
    if ($components.Count -ne $required.Count) { throw 'Ledger must contain exactly the four retained test components.' }
    foreach ($name in $required.Keys) {
        $matches = @($components | Where-Object { [string]$_.name -ceq $name })
        if ($matches.Count -ne 1) { throw "Ledger must contain exactly one '$name' component." }
        $item = $matches[0]
        if ([string]$item.executableRole -cne [string]$required[$name].role) {
            throw "Ledger executableRole is invalid for '$name'."
        }
        $actualArgs = @($item.args | ForEach-Object { [string]$_ })
        $expectedArgs = @($required[$name].args)
        if ($actualArgs.Count -ne $expectedArgs.Count) { throw "Ledger args are invalid for '$name'." }
        for ($index = 0; $index -lt $actualArgs.Count; $index++) {
            if ($actualArgs[$index] -cne $expectedArgs[$index]) { throw "Ledger args are invalid for '$name'." }
        }
        $item | Add-Member -NotePropertyName verifiedTests -NotePropertyValue (
            ConvertTo-OrdinalUniqueStrings -Values @($item.tests) -Label "Ledger tests for '$name'"
        ) -Force
    }
    $repeats = @($ledger.repeats)
    if ($repeats.Count -eq 0) { throw 'Ledger must contain at least one named targeted repeat.' }
    foreach ($repeat in $repeats) {
        $componentMatches = @($components | Where-Object { [string]$_.name -ceq [string]$repeat.component })
        if ($componentMatches.Count -ne 1) { throw "Repeat references unknown component '$($repeat.component)'." }
        $name = [string]$repeat.test
        if (-not ($componentMatches[0].verifiedTests -ccontains $name)) {
            throw "Repeat test '$name' is not retained by component '$($repeat.component)'."
        }
        $count = 0
        if (-not [int]::TryParse([string]$repeat.count, [ref]$count) -or $count -lt 1 -or $count -gt 100) {
            throw "Repeat count must be from 1 through 100 for '$name'."
        }
    }
    return $ledger
}

function Get-ComponentExecutable {
    param(
        [Parameter(Mandatory = $true)][string] $Name,
        [Parameter(Mandatory = $true)][Collections.IDictionary] $Artifacts
    )
    switch -CaseSensitive ($Name) {
        'adrai-test' { return [string]$Artifacts.ordinary }
        'adrai-cache-selection-test' { return [string]$Artifacts.cacheSelection }
        'adrai-stress-test' { return [string]$Artifacts.stress }
        'adrai-benchmark-registration-test' { return [string]$Artifacts.benchmarkRegistration }
        default { throw "Unknown retained component: $Name" }
    }
}

function Assert-ExactRegistration {
    param(
        [Parameter(Mandatory = $true)][object] $ComponentRecord,
        [Parameter(Mandatory = $true)][string] $Executable,
        [Parameter(Mandatory = $true)][string] $ResolvedAdraiExe,
        [Parameter(Mandatory = $true)][string] $Id
    )
    $args = @($ComponentRecord.args | ForEach-Object { [string]$_ }) + @('--list-tests')
    $result = Invoke-OwnedProcess -Id $Id -Executable $Executable -Arguments $args -WorkingDirectory $script:RepositoryRoot -Environment (Get-ProcessEnvironment $ResolvedAdraiExe)
    Assert-SuccessfulInvocation $result
    [string[]]$actual = @(ConvertTo-OrdinalUniqueStrings -Values @([IO.File]::ReadAllLines($result.stdout)) -Label "Actual registration for '$($ComponentRecord.name)'")
    [string[]]$expected = @($ComponentRecord.verifiedTests)
    [Array]::Sort($actual, [StringComparer]::Ordinal)
    [Array]::Sort($expected, [StringComparer]::Ordinal)
    if ($actual.Count -ne $expected.Count) {
        throw "Registration count mismatch for '$($ComponentRecord.name)': expected $($expected.Count), observed $($actual.Count)."
    }
    for ($index = 0; $index -lt $actual.Count; $index++) {
        if ($actual[$index] -cne $expected[$index]) {
            throw "Registration mismatch for '$($ComponentRecord.name)' at ordinal index $index."
        }
    }
}

function ConvertTo-ExactTastyPattern {
    param([Parameter(Mandatory = $true)][string] $Name)
    $escaped = $Name.Replace('\', '\\').Replace('"', '\"')
    return ('$0 == "' + $escaped + '"')
}

function ConvertTo-WindowsCommandLineArgument {
    param([Parameter(Mandatory = $true)][AllowEmptyString()][string] $Value)

    if ($Value.Length -gt 0 -and $Value.IndexOfAny([char[]]@(' ', "`t", "`n", [char]11, '"')) -lt 0) {
        return $Value
    }
    $builder = [Text.StringBuilder]::new()
    [void]$builder.Append('"')
    $backslashes = 0
    foreach ($character in $Value.ToCharArray()) {
        if ($character -eq '\') {
            $backslashes++
        }
        elseif ($character -eq '"') {
            [void]$builder.Append('\', ($backslashes * 2) + 1)
            [void]$builder.Append('"')
            $backslashes = 0
        }
        else {
            if ($backslashes -gt 0) { [void]$builder.Append('\', $backslashes) }
            [void]$builder.Append($character)
            $backslashes = 0
        }
    }
    if ($backslashes -gt 0) { [void]$builder.Append('\', $backslashes * 2) }
    [void]$builder.Append('"')
    return $builder.ToString()
}

function Assert-WindowsCommandLineLength {
    param(
        [Parameter(Mandatory = $true)][string] $Executable,
        [Parameter(Mandatory = $true)][AllowEmptyCollection()][string[]] $Arguments,
        [Parameter(Mandatory = $true)][string] $Id
    )

    $parts = @((ConvertTo-WindowsCommandLineArgument $Executable)) + @(
        $Arguments | ForEach-Object { ConvertTo-WindowsCommandLineArgument ([string]$_) }
    )
    $lengthIncludingTerminator = (($parts -join ' ').Length) + 1
    if ($lengthIncludingTerminator -gt 32767) {
        throw "Invocation '$Id' requires a $lengthIncludingTerminator-character Windows command line including its terminator."
    }
    return $lengthIncludingTerminator
}

function Assert-OrdinalSetEquality {
    param(
        [Parameter(Mandatory = $true)][string[]] $Expected,
        [Parameter(Mandatory = $true)][string[]] $Actual,
        [Parameter(Mandatory = $true)][string] $Label
    )

    [string[]]$expectedSorted = @($Expected)
    [string[]]$actualSorted = @($Actual)
    [Array]::Sort($expectedSorted, [StringComparer]::Ordinal)
    [Array]::Sort($actualSorted, [StringComparer]::Ordinal)
    if ($expectedSorted.Count -ne $actualSorted.Count) {
        throw "$Label count mismatch: expected $($expectedSorted.Count), observed $($actualSorted.Count)."
    }
    for ($index = 0; $index -lt $expectedSorted.Count; $index++) {
        if ($expectedSorted[$index] -cne $actualSorted[$index]) {
            throw "$Label differs at ordinal index $index."
        }
    }
}

function Assert-PartitionRegistration {
    param(
        [Parameter(Mandatory = $true)][object] $Partition,
        [Parameter(Mandatory = $true)][string] $Executable,
        [Parameter(Mandatory = $true)][string] $ResolvedAdraiExe
    )

    $id = "list-partition-$([string]$Partition.id)"
    [string[]]$arguments = @('-p', [string]$Partition.selector, '--list-tests')
    [void](Assert-WindowsCommandLineLength -Executable $Executable -Arguments $arguments -Id $id)
    $result = Invoke-OwnedProcess -Id $id -Executable $Executable -Arguments $arguments -WorkingDirectory $script:RepositoryRoot -Environment (Get-ProcessEnvironment $ResolvedAdraiExe)
    Assert-SuccessfulInvocation $result
    [string[]]$actual = @(ConvertTo-OrdinalUniqueStrings -Values @([IO.File]::ReadAllLines($result.stdout)) -Label "Actual registration for partition '$($Partition.id)'")
    [string[]]$expected = @($Partition.verifiedTests)
    Assert-OrdinalSetEquality -Expected $expected -Actual $actual -Label "Registration for ordinary partition '$($Partition.id)'"
    $result | Add-Member -NotePropertyName observedRegistrationCount -NotePropertyValue $actual.Count -Force
    $evidenceRecord = @($script:Evidence.invocations | Where-Object { $_.id -ceq $result.id })[-1]
    $evidenceRecord | Add-Member -NotePropertyName observedRegistrationCount -NotePropertyValue $actual.Count -Force
}

function Read-And-VerifyOrdinaryPartitions {
    param(
        [Parameter(Mandatory = $true)][object] $Ledger,
        [Parameter(Mandatory = $true)][Collections.IDictionary] $Artifacts
    )

    if ([string]::IsNullOrWhiteSpace($OrdinaryPartitionsPath)) {
        $script:OrdinaryPartitionsPath = Resolve-AbsoluteFile `
            -Path (Join-Path $script:RepositoryRoot 'test\coverage\ordinary-partitions.json') `
            -Label 'OrdinaryPartitionsPath'
    }
    else {
        $script:OrdinaryPartitionsPath = Resolve-AbsoluteFile -Path $OrdinaryPartitionsPath -Label 'OrdinaryPartitionsPath'
        if (-not (Test-PathInsideRoot -Path $script:OrdinaryPartitionsPath -Root $script:RepositoryRoot)) {
            throw 'OrdinaryPartitionsPath must be inside the repository build-input tree.'
        }
    }
    $overlay = [IO.File]::ReadAllText($script:OrdinaryPartitionsPath) | ConvertFrom-Json
    if ($overlay.schemaVersion -ne 1) { throw 'Unsupported ordinary partition schemaVersion.' }
    if ([int]$overlay.maximumActiveJobs -ne 3) { throw 'The retained scheduler requires exactly three active owned process roots.' }
    if ([string]$overlay.component -cne 'adrai-test') { throw 'Ordinary partitions must target adrai-test.' }

    $ordinaryComponent = @($Ledger.components | Where-Object { [string]$_.name -ceq 'adrai-test' })[0]
    $raceTestName = 'ADRAI.P4-07.Cache integration (P4-07).competing tree-identical targets retain their own provenance projections'
    $requiredPartitionSelectors = @{
        K = '$3 == "Cache integration (P4-07)" && !($0 == "ADRAI.P4-07.Cache integration (P4-07).competing tree-identical targets retain their own provenance projections")'
        Krace = '$0 == "ADRAI.P4-07.Cache integration (P4-07).competing tree-identical targets retain their own provenance projections"'
    }
    $requiredPartitionIds = @(
        'A', 'C', 'D', 'E', 'T', 'N', 'K', 'Krace', 'O', 'R', 'Rest',
        'Q', 'Env', 'MutationE2E', 'CompilerSearch'
    )
    $partitions = @($overlay.partitions)
    if ($partitions.Count -ne $requiredPartitionIds.Count) { throw 'Ordinary partition overlay must contain exactly fifteen partitions.' }
    $partitionIds = ConvertTo-OrdinalUniqueStrings -Values @($partitions | ForEach-Object { [string]$_.id }) -Label 'Ordinary partition IDs'
    Assert-OrdinalSetEquality -Expected $requiredPartitionIds -Actual $partitionIds -Label 'Ordinary partition IDs'

    $union = [Collections.Generic.HashSet[string]]::new([StringComparer]::Ordinal)
    foreach ($partition in $partitions) {
        $expectedPartitionClass = if ([string]$partition.id -ceq 'Krace') { 'exclusive' } else { 'normal' }
        if ([string]$partition.classification -cne $expectedPartitionClass) {
            throw "Ordinary partition '$($partition.id)' must be a $expectedPartitionClass queued job."
        }
        if ([string]::IsNullOrWhiteSpace([string]$partition.selector)) { throw "Ordinary partition '$($partition.id)' has no selector." }
        if ($requiredPartitionSelectors.ContainsKey([string]$partition.id) -and
            [string]$partition.selector -cne [string]$requiredPartitionSelectors[[string]$partition.id]) {
            throw "Ordinary partition '$($partition.id)' does not have its required exact selector."
        }
        $partition | Add-Member -NotePropertyName verifiedTests -NotePropertyValue (
            ConvertTo-OrdinalUniqueStrings -Values @($partition.tests) -Label "Tests for ordinary partition '$($partition.id)'"
        ) -Force
        if ([string]$partition.id -ceq 'Krace' -and
            (@($partition.verifiedTests).Count -ne 1 -or [string](@($partition.verifiedTests)[0]) -cne $raceTestName)) {
            throw "Ordinary partition 'Krace' must contain only the competing tree-identical provenance test."
        }
        foreach ($name in @($partition.verifiedTests)) {
            if (-not $union.Add($name)) { throw "Ordinary partitions overlap at '$name'." }
        }
    }
    [string[]]$ordinaryNames = @($ordinaryComponent.verifiedTests)
    [string[]]$unionNames = @($union)
    Assert-OrdinalSetEquality -Expected $ordinaryNames -Actual $unionNames -Label 'Ordinary partition union'

    [string[]]$requiredQueueOrder = @(
        'Q', 'N', 'R', 'T', 'Env', 'A', 'CompilerSearch', 'K', 'O',
        'MutationE2E', 'D', 'cache', 'C', 'Rest', 'stress', 'E', 'registration',
        'Krace', 'repeat-001-001'
    )
    $queue = @($overlay.queue)
    if ($queue.Count -ne $requiredQueueOrder.Count) { throw 'Scheduler queue length does not match the required job multiset.' }
    $queueIds = ConvertTo-OrdinalUniqueStrings -Values @($queue | ForEach-Object { [string]$_.id }) -Label 'Scheduler queue IDs'
    for ($index = 0; $index -lt $requiredQueueOrder.Count; $index++) {
        if ([string]$queue[$index].id -cne $requiredQueueOrder[$index]) {
            throw "Scheduler queue order differs at index $index."
        }
        $expectedClass = if ($requiredQueueOrder[$index] -in @('Krace', 'repeat-001-001')) { 'exclusive' } else { 'normal' }
        if ([string]$queue[$index].classification -cne $expectedClass) {
            throw "Scheduler queue classification is invalid for '$($queue[$index].id)'."
        }
    }

    $nonOrdinaryComponents = [ordered]@{
        cache = 'adrai-cache-selection-test'
        stress = 'adrai-stress-test'
        registration = 'adrai-benchmark-registration-test'
    }
    $definitions = @{}
    foreach ($partition in $partitions) {
        $definitions[[string]$partition.id] = [pscustomobject]@{
            id = [string]$partition.id
            classification = [string]$partition.classification
            executable = [string]$Artifacts.ordinary
            arguments = @('-p', [string]$partition.selector)
            workingDirectory = $script:RepositoryRoot
            environment = Get-ProcessEnvironment $Artifacts.adrai
            invocationLimitSeconds = 0
            expectedCount = @($partition.verifiedTests).Count
        }
    }
    foreach ($jobId in $nonOrdinaryComponents.Keys) {
        $componentName = [string]$nonOrdinaryComponents[$jobId]
        $componentRecord = @($Ledger.components | Where-Object { [string]$_.name -ceq $componentName })[0]
        $definitions[$jobId] = [pscustomobject]@{
            id = $jobId
            classification = 'normal'
            executable = Get-ComponentExecutable -Name $componentName -Artifacts $Artifacts
            arguments = @($componentRecord.args | ForEach-Object { [string]$_ })
            workingDirectory = $script:RepositoryRoot
            environment = Get-ProcessEnvironment $Artifacts.adrai
            invocationLimitSeconds = 0
            expectedCount = @($componentRecord.verifiedTests).Count
        }
    }

    $repeatIndex = 0
    foreach ($repeat in @($Ledger.repeats)) {
        $componentRecord = @($Ledger.components | Where-Object { [string]$_.name -ceq [string]$repeat.component })[0]
        for ($iteration = 1; $iteration -le [int]$repeat.count; $iteration++) {
            $repeatIndex++
            $jobId = "repeat-{0:D3}-{1:D3}" -f $repeatIndex, $iteration
            $definitions[$jobId] = [pscustomobject]@{
                id = $jobId
                classification = 'exclusive'
                executable = Get-ComponentExecutable -Name ([string]$componentRecord.name) -Artifacts $Artifacts
                arguments = @($componentRecord.args | ForEach-Object { [string]$_ }) + @('-p', (ConvertTo-ExactTastyPattern -Name ([string]$repeat.test)))
                workingDirectory = $script:RepositoryRoot
                environment = Get-ProcessEnvironment $Artifacts.adrai
                invocationLimitSeconds = 0
                expectedCount = 1
            }
        }
    }

    $requiredIds = @($partitions | ForEach-Object { [string]$_.id }) + @($nonOrdinaryComponents.Keys) + @(
        $definitions.Keys | Where-Object { $_.StartsWith('repeat-', [StringComparison]::Ordinal) }
    )
    Assert-OrdinalSetEquality -Expected @($requiredIds) -Actual @($queueIds) -Label 'Required and queued scheduler job IDs'
    $ordered = [Collections.Generic.List[object]]::new()
    foreach ($queueItem in $queue) {
        $id = [string]$queueItem.id
        if (-not $definitions.ContainsKey($id)) { throw "Scheduler queue references unknown job '$id'." }
        $definition = $definitions[$id]
        if ([string]$definition.classification -cne [string]$queueItem.classification) {
            throw "Scheduler classification mismatch for '$id'."
        }
        $commandLength = Assert-WindowsCommandLineLength -Executable $definition.executable -Arguments @($definition.arguments) -Id $id
        $definition | Add-Member -NotePropertyName windowsCommandLineLength -NotePropertyValue $commandLength
        $ordered.Add($definition)
    }
    return [pscustomobject]@{
        maximumActiveJobs = 3
        partitions = $partitions
        jobs = $ordered.ToArray()
    }
}

function Invoke-SelfCheck {
    $powershell = Resolve-AbsoluteFile -Path ([Diagnostics.Process]::GetCurrentProcess().MainModule.FileName) -Label 'PowerShell executable'
    $fixtureDirectory = Join-Path $script:EvidenceDirectory 'fixture path with spaces'
    [IO.Directory]::CreateDirectory($fixtureDirectory) | Out-Null
    $argumentFixture = Join-Path $fixtureDirectory 'argument check.ps1'
    $fixtureText = "param([string]`$Value)`r`nif (`$Value -cne 'space `"quote`" trailing\') { exit 91 }`r`nexit 0`r`n"
    [IO.File]::WriteAllText($argumentFixture, $fixtureText, [Text.UTF8Encoding]::new($false))
    $environment = Get-ProcessEnvironment $null

    if (-not (Test-AggregateCleanupVerified -LaunchFailureCleanupVerified $null -JobCleanupVerified $true) -or
        -not (Test-AggregateCleanupVerified -LaunchFailureCleanupVerified $true -JobCleanupVerified $true) -or
        (Test-AggregateCleanupVerified -LaunchFailureCleanupVerified $false -JobCleanupVerified $true) -or
        (Test-AggregateCleanupVerified -LaunchFailureCleanupVerified $true -JobCleanupVerified $false)) {
        throw 'Combined launch and Job cleanup verification self-check failed.'
    }
    $script:Evidence.cleanupAggregationVerified = $true

    $success = Invoke-OwnedProcess -Id 'selfcheck-success-quoting' -Executable $powershell -Arguments @(
        '-NoLogo', '-NoProfile', '-NonInteractive', '-File', $argumentFixture, 'space "quote" trailing\'
    ) -WorkingDirectory $script:EvidenceDirectory -Environment $environment -InvocationLimitSeconds 5
    Assert-SuccessfulInvocation $success

    $selectorName = 'group.test\path "quoted"'
    $selectorExpected = '$0 == "group.test\\path \"quoted\""'
    if ((ConvertTo-ExactTastyPattern -Name $selectorName) -cne $selectorExpected) {
        throw 'Exact Tasty selector self-check failed.'
    }

    $tastyFixture = Join-Path $fixtureDirectory 'tasty summary.ps1'
    [IO.File]::WriteAllText(
        $tastyFixture,
        "param([string]`$Pattern)`r`nif (`$Pattern -cne '$($selectorExpected.Replace("'", "''"))') { exit 95 }`r`n[Console]::Out.WriteLine('All 1 tests passed')`r`nexit 0`r`n",
        [Text.UTF8Encoding]::new($false)
    )
    $tasty = Invoke-OwnedProcess -Id 'selfcheck-tasty-execution' -Executable $powershell -Arguments @(
        '-NoLogo', '-NoProfile', '-NonInteractive', '-File', $tastyFixture, $selectorExpected
    ) -WorkingDirectory $script:EvidenceDirectory -Environment $environment -InvocationLimitSeconds 5
    Assert-SuccessfulInvocation $tasty
    Assert-TastyExecutionCount $tasty 1

    $fakeDist = Join-Path $script:RepositoryRoot '.stack-work\dist\controlled-selfcheck'
    $expectedArtifacts = Get-ExpectedStackArtifacts -Root $script:RepositoryRoot -DistDirectory $fakeDist
    Assert-StackArtifactPaths -Explicit $expectedArtifacts -Expected $expectedArtifacts
    $wrongArtifacts = [ordered]@{}
    foreach ($role in $expectedArtifacts.Keys) { $wrongArtifacts[$role] = $expectedArtifacts[$role] }
    $wrongArtifacts.ordinary = Join-Path $script:EvidenceDirectory 'caller-selected.exe'
    $rejectedCallerArtifact = $false
    try {
        Assert-StackArtifactPaths -Explicit $wrongArtifacts -Expected $expectedArtifacts
    }
    catch {
        $rejectedCallerArtifact = $true
    }
    if (-not $rejectedCallerArtifact) { throw 'Caller-selected artifact rejection self-check failed.' }
    $script:Evidence.stackArtifactPathsPinned = $true

    $savedTastyListTests = [Environment]::GetEnvironmentVariable('TASTY_LIST_TESTS', 'Process')
    $savedTastyPattern = [Environment]::GetEnvironmentVariable('TASTY_PATTERN', 'Process')
    $savedStackYaml = [Environment]::GetEnvironmentVariable('STACK_YAML', 'Process')
    try {
        [Environment]::SetEnvironmentVariable('TASTY_LIST_TESTS', 'true', 'Process')
        [Environment]::SetEnvironmentVariable('TASTY_PATTERN', 'untrusted inherited selector', 'Process')
        [Environment]::SetEnvironmentVariable('STACK_YAML', 'C:\untrusted\stack.yaml', 'Process')
        $scrubbed = Get-ProcessEnvironment $null
        if ($scrubbed.TASTY_LIST_TESTS -ne $null -or $scrubbed.TASTY_PATTERN -ne $null -or
            [string]$scrubbed.TASTY_NUM_THREADS -cne '1' -or $scrubbed.STACK_YAML -ne $null) {
            throw 'Inherited runner option scrubbing self-check failed.'
        }
        $environmentFixture = Join-Path $fixtureDirectory 'environment check.ps1'
        $environmentFixtureText = @"
if (`$null -ne [Environment]::GetEnvironmentVariable('TASTY_LIST_TESTS')) { exit 92 }
if (`$null -ne [Environment]::GetEnvironmentVariable('TASTY_PATTERN')) { exit 93 }
if ([Environment]::GetEnvironmentVariable('TASTY_NUM_THREADS') -cne '1') { exit 94 }
if (`$null -ne [Environment]::GetEnvironmentVariable('STACK_YAML')) { exit 96 }
exit 0
"@
        [IO.File]::WriteAllText($environmentFixture, $environmentFixtureText, [Text.UTF8Encoding]::new($false))
        $environmentCheck = Invoke-OwnedProcess -Id 'selfcheck-tasty-environment' -Executable $powershell -Arguments @(
            '-NoLogo', '-NoProfile', '-NonInteractive', '-File', $environmentFixture
        ) -WorkingDirectory $script:EvidenceDirectory -Environment $scrubbed -InvocationLimitSeconds 5
        Assert-SuccessfulInvocation $environmentCheck
        $script:Evidence.inheritedTastyOptionsScrubbed = $true
        $script:Evidence.exactSelector = $selectorExpected
    }
    finally {
        [Environment]::SetEnvironmentVariable('TASTY_LIST_TESTS', $savedTastyListTests, 'Process')
        [Environment]::SetEnvironmentVariable('TASTY_PATTERN', $savedTastyPattern, 'Process')
        [Environment]::SetEnvironmentVariable('STACK_YAML', $savedStackYaml, 'Process')
    }

    $nonzero = Invoke-OwnedProcess -Id 'selfcheck-nonzero' -Executable $powershell -Arguments @(
        '-NoLogo', '-NoProfile', '-NonInteractive', '-Command', 'exit 23'
    ) -WorkingDirectory $script:EvidenceDirectory -Environment $environment -InvocationLimitSeconds 5
    if ($nonzero.timedOut -or $nonzero.orphanedDescendants -or -not $nonzero.cleanupVerified -or $nonzero.exitCode -ne 23) {
        throw 'Nonzero self-check did not preserve exit code 23 and clean its owned tree.'
    }

    $blockingCommand = '[Threading.ManualResetEvent]::new($false).WaitOne()'
    $timeout = Invoke-OwnedProcess -Id 'selfcheck-timeout' -Executable $powershell -Arguments @(
        '-NoLogo', '-NoProfile', '-NonInteractive', '-Command', $blockingCommand
    ) -WorkingDirectory $script:EvidenceDirectory -Environment $environment -InvocationLimitSeconds 1
    if (-not $timeout.timedOut -or -not $timeout.cleanupVerified) {
        throw 'Timeout self-check did not time out and verify owned-tree cleanup.'
    }

    $encodedBlocking = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes($blockingCommand))
    $parentCommand = "`$child = Start-Process -FilePath '$($powershell.Replace("'", "''"))' -ArgumentList '-NoLogo','-NoProfile','-NonInteractive','-EncodedCommand','$encodedBlocking' -WindowStyle Hidden -PassThru; [Console]::Out.WriteLine(`$child.Id); exit 0"
    $grandchild = Invoke-OwnedProcess -Id 'selfcheck-grandchild' -Executable $powershell -Arguments @(
        '-NoLogo', '-NoProfile', '-NonInteractive', '-Command', $parentCommand
    ) -WorkingDirectory $script:EvidenceDirectory -Environment $environment -InvocationLimitSeconds 5
    if ($grandchild.timedOut -or -not $grandchild.orphanedDescendants -or -not $grandchild.cleanupVerified -or $grandchild.exitCode -ne 0) {
        throw 'Grandchild self-check did not detect and clean a descendant after root exit.'
    }
    $grandchildPidText = [IO.File]::ReadAllText($grandchild.stdout).Trim()
    $grandchildPid = 0
    if (-not [int]::TryParse($grandchildPidText, [ref]$grandchildPid)) {
        throw 'Grandchild self-check did not report its controlled child PID.'
    }
    $grandchildSnapshot = $grandchild.orphanFailureSnapshot
    if ($null -eq $grandchildSnapshot -or $null -ne $grandchildSnapshot.captureError -or $null -eq $grandchildSnapshot.job -or
        $grandchildSnapshot.rootPid -ne $grandchild.pid -or $grandchildSnapshot.activeProcessCount -lt 1 -or
        $grandchildSnapshot.triggerMonotonicMilliseconds -lt $grandchildSnapshot.rootExitObservedMonotonicMilliseconds -or
        -not $grandchildSnapshot.job.complete -or $grandchildSnapshot.job.returnedProcessIdCount -lt 1) {
        throw 'Grandchild self-check did not capture complete pre-termination orphan evidence.'
    }
    $grandchildIdentity = @($grandchildSnapshot.job.processes | Where-Object {
        $_.processId -eq $grandchildPid -and $_.confirmedJobMembership -eq $true -and
        $_.identityState -ceq 'confirmed' -and $_.zeroWaitState -ceq 'running' -and
        -not [string]::IsNullOrWhiteSpace($_.imagePath) -and
        -not [string]::IsNullOrWhiteSpace($_.imageName) -and
        $null -ne $_.creationTimeFileTime -and
        -not [string]::IsNullOrWhiteSpace($_.creationTimeUtc)
    })
    if ($grandchildIdentity.Count -ne 1) {
        throw 'Grandchild self-check snapshot did not identify its running same-Job child with durable identity.'
    }
    try {
        $grandchildProcess = [Diagnostics.Process]::GetProcessById($grandchildPid)
        try {
            $grandchildProcess.Refresh()
            if (-not $grandchildProcess.HasExited) {
                throw "Controlled grandchild PID $grandchildPid survived Job Object cleanup."
            }
            $script:Evidence.grandchildPid = $grandchildPid
            $script:Evidence.grandchildProcessExited = $true
        }
        finally {
            $grandchildProcess.Dispose()
        }
    }
    catch [ArgumentException] {
        $script:Evidence.grandchildPid = $grandchildPid
        $script:Evidence.grandchildProcessAbsent = $true
    }

    $unrelatedStart = [Diagnostics.ProcessStartInfo]::new()
    $unrelatedStart.FileName = $powershell
    $unrelatedStart.Arguments = "-NoLogo -NoProfile -NonInteractive -EncodedCommand $encodedBlocking"
    $unrelatedStart.UseShellExecute = $false
    $unrelatedStart.CreateNoWindow = $true
    $unrelated = [Diagnostics.Process]::Start($unrelatedStart)
    try {
        $ownedTimeout = Invoke-OwnedProcess -Id 'selfcheck-unrelated-preservation' -Executable $powershell -Arguments @(
            '-NoLogo', '-NoProfile', '-NonInteractive', '-Command', $blockingCommand
        ) -WorkingDirectory $script:EvidenceDirectory -Environment $environment -InvocationLimitSeconds 1
        if (-not $ownedTimeout.timedOut -or -not $ownedTimeout.cleanupVerified) {
            throw 'Owned timeout in unrelated-process self-check did not clean up.'
        }
        $unrelated.Refresh()
        if ($unrelated.HasExited) { throw 'Terminating the owned Job Object also terminated the unrelated control process.' }
        $script:Evidence.unrelatedProcessPreserved = $true
    }
    finally {
        $unrelated.Refresh()
        if (-not $unrelated.HasExited) {
            $unrelated.Kill()
            if (-not $unrelated.WaitForExit(5000)) {
                throw "Controlled unrelated PID $($unrelated.Id) did not exit after exact-PID cleanup."
            }
        }
        $script:Evidence.unrelatedProcessCleanupVerified = $unrelated.HasExited
        $unrelated.Dispose()
    }

    $schedulerFixture = Join-Path $fixtureDirectory 'scheduler fixture.ps1'
    $schedulerFixtureText = @'
param(
    [Parameter(Mandatory = $true)][string] $Mode,
    [Parameter(Mandatory = $true)][string] $PowerShellPath
)
switch -CaseSensitive ($Mode) {
    'success' {
        [void]([Threading.ManualResetEvent]::new($false).WaitOne(150))
        [Console]::Out.WriteLine('All 1 tests passed')
        exit 0
    }
    'nonzero' { exit 23 }
    'block' { [void]([Threading.ManualResetEvent]::new($false).WaitOne()); exit 0 }
    'malformed-summary' { [Console]::Out.WriteLine('All 2 tests passed'); exit 0 }
    'grandchild' {
        $blocking = '[void]([Threading.ManualResetEvent]::new($false).WaitOne())'
        $encoded = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes($blocking))
        $child = Start-Process -FilePath $PowerShellPath -ArgumentList '-NoLogo','-NoProfile','-NonInteractive','-EncodedCommand',$encoded -WindowStyle Hidden -PassThru
        [Console]::Out.WriteLine($child.Id)
        exit 0
    }
    default { exit 97 }
}
'@
    [IO.File]::WriteAllText($schedulerFixture, $schedulerFixtureText, [Text.UTF8Encoding]::new($false))

    function New-ControlledSchedulerJob {
        param(
            [string] $Id,
            [string] $FixtureMode,
            [int] $ExpectedCount,
            [int] $LimitSeconds = 5,
            [string] $Executable = $powershell
        )
        return [pscustomobject]@{
            id = $Id
            classification = 'normal'
            executable = $Executable
            arguments = @('-NoLogo', '-NoProfile', '-NonInteractive', '-File', $schedulerFixture, $FixtureMode, $powershell)
            workingDirectory = $script:EvidenceDirectory
            environment = $environment
            invocationLimitSeconds = $LimitSeconds
            expectedCount = $ExpectedCount
        }
    }
    function Assert-ControlledSchedulerCleanup {
        param([string] $Prefix, [int] $ExpectedRecords)
        $records = @($script:Evidence.invocations | Where-Object { ([string]$_.id).StartsWith($Prefix, [StringComparison]::Ordinal) })
        if ($records.Count -ne $ExpectedRecords) {
            throw "Scheduler self-check '$Prefix' recorded $($records.Count) invocations; expected $ExpectedRecords."
        }
        if (@($records | Where-Object { -not $_.cleanupVerified }).Count -gt 0) {
            throw "Scheduler self-check '$Prefix' left cleanup unverified."
        }
    }
    function Invoke-ExpectedSchedulerFailure {
        param([object[]] $Jobs, [string] $RunId)
        $failed = $false
        try { Invoke-OwnedProcessQueue -Jobs $Jobs -MaximumActiveJobs 3 -RunId $RunId }
        catch { $failed = $true }
        if (-not $failed) { throw "Scheduler self-check '$RunId' unexpectedly passed." }
    }

    $schedulerSuccessJobs = @(
        (New-ControlledSchedulerJob 'scheduler-success-1' 'success' 1),
        (New-ControlledSchedulerJob 'scheduler-success-2' 'success' 1),
        (New-ControlledSchedulerJob 'scheduler-success-3' 'success' 1)
    )
    Invoke-OwnedProcessQueue -Jobs $schedulerSuccessJobs -MaximumActiveJobs 3 -RunId 'selfcheck-scheduler-success'
    Assert-ControlledSchedulerCleanup 'scheduler-success-' 3
    $successRun = @($script:Evidence.schedulerRuns | Where-Object { $_.id -ceq 'selfcheck-scheduler-success' })[-1]
    if ($successRun.peakActiveJobs -ne 3 -or $successRun.completed.Count -ne 3) {
        throw 'Scheduler success self-check did not exercise three simultaneously owned process roots.'
    }

    $schedulerNonzeroJobs = @(
        (New-ControlledSchedulerJob 'scheduler-nonzero-fail' 'nonzero' -1),
        (New-ControlledSchedulerJob 'scheduler-nonzero-peer-1' 'block' -1),
        (New-ControlledSchedulerJob 'scheduler-nonzero-peer-2' 'block' -1)
    )
    Invoke-ExpectedSchedulerFailure $schedulerNonzeroJobs 'selfcheck-scheduler-nonzero'
    Assert-ControlledSchedulerCleanup 'scheduler-nonzero-' 3
    $nonzeroRecord = @($script:Evidence.invocations | Where-Object { $_.id -ceq 'scheduler-nonzero-fail' })[-1]
    if ($nonzeroRecord.exitCode -ne 23) { throw 'Scheduler nonzero self-check did not preserve exit code 23.' }

    $unrelatedSchedulerStart = [Diagnostics.ProcessStartInfo]::new()
    $unrelatedSchedulerStart.FileName = $powershell
    $unrelatedSchedulerStart.Arguments = "-NoLogo -NoProfile -NonInteractive -EncodedCommand $encodedBlocking"
    $unrelatedSchedulerStart.UseShellExecute = $false
    $unrelatedSchedulerStart.CreateNoWindow = $true
    $unrelatedScheduler = [Diagnostics.Process]::Start($unrelatedSchedulerStart)
    try {
        $schedulerTimeoutJobs = @(
            (New-ControlledSchedulerJob 'scheduler-timeout-1' 'block' -1 1),
            (New-ControlledSchedulerJob 'scheduler-timeout-2' 'block' -1 1),
            (New-ControlledSchedulerJob 'scheduler-timeout-3' 'block' -1 1)
        )
        Invoke-ExpectedSchedulerFailure $schedulerTimeoutJobs 'selfcheck-scheduler-timeout'
        Assert-ControlledSchedulerCleanup 'scheduler-timeout-' 3
        if (@($script:Evidence.invocations | Where-Object { ([string]$_.id).StartsWith('scheduler-timeout-', [StringComparison]::Ordinal) -and $_.timedOut }).Count -eq 0) {
            throw 'Scheduler timeout self-check did not record a timed-out owned root.'
        }
        $unrelatedScheduler.Refresh()
        if ($unrelatedScheduler.HasExited) { throw 'Scheduler all-active cleanup terminated an unrelated process.' }
        $script:Evidence.schedulerUnrelatedProcessPreserved = $true
    }
    finally {
        $unrelatedScheduler.Refresh()
        if (-not $unrelatedScheduler.HasExited) {
            $unrelatedScheduler.Kill()
            if (-not $unrelatedScheduler.WaitForExit(5000)) { throw 'Scheduler unrelated control process did not exit.' }
        }
        $unrelatedScheduler.Dispose()
    }

    $schedulerLaunchJobs = @(
        (New-ControlledSchedulerJob 'scheduler-launch-peer-1' 'block' -1),
        (New-ControlledSchedulerJob 'scheduler-launch-peer-2' 'block' -1),
        (New-ControlledSchedulerJob 'scheduler-launch-failure' 'success' -1 5 'Z:\missing\scheduler-fixture.exe')
    )
    Invoke-ExpectedSchedulerFailure $schedulerLaunchJobs 'selfcheck-scheduler-launch-failure'
    Assert-ControlledSchedulerCleanup 'scheduler-launch-' 3

    $schedulerCoordinatorJobs = @(
        (New-ControlledSchedulerJob 'scheduler-coordinator-invalid-summary' 'malformed-summary' 1),
        (New-ControlledSchedulerJob 'scheduler-coordinator-peer-1' 'block' -1),
        (New-ControlledSchedulerJob 'scheduler-coordinator-peer-2' 'block' -1)
    )
    Invoke-ExpectedSchedulerFailure $schedulerCoordinatorJobs 'selfcheck-scheduler-coordinator-exception'
    Assert-ControlledSchedulerCleanup 'scheduler-coordinator-' 3

    $schedulerEmptyCleanupJobs = @(
        (New-ControlledSchedulerJob 'scheduler-empty-cleanup-invalid-summary' 'malformed-summary' 1)
    )
    Invoke-ExpectedSchedulerFailure $schedulerEmptyCleanupJobs 'selfcheck-scheduler-empty-cleanup'
    Assert-ControlledSchedulerCleanup 'scheduler-empty-cleanup-' 1
    $emptyCleanupRun = @($script:Evidence.schedulerRuns | Where-Object { $_.id -ceq 'selfcheck-scheduler-empty-cleanup' })[-1]
    if (-not $emptyCleanupRun.cleanupVerified) {
        throw 'Scheduler empty-active-set cleanup did not preserve verified cleanup.'
    }

    $schedulerOrphanJobs = @(
        (New-ControlledSchedulerJob 'scheduler-orphan-root' 'grandchild' -1),
        (New-ControlledSchedulerJob 'scheduler-orphan-peer-1' 'block' -1),
        (New-ControlledSchedulerJob 'scheduler-orphan-peer-2' 'block' -1)
    )
    Invoke-ExpectedSchedulerFailure $schedulerOrphanJobs 'selfcheck-scheduler-root-exit'
    Assert-ControlledSchedulerCleanup 'scheduler-orphan-' 3
    $orphanRecord = @($script:Evidence.invocations | Where-Object { $_.id -ceq 'scheduler-orphan-root' })[-1]
    if (-not $orphanRecord.orphanedDescendants) { throw 'Scheduler root-exit self-check did not classify the surviving descendant.' }
    $schedulerGrandchildPid = 0
    if (-not [int]::TryParse([IO.File]::ReadAllText($orphanRecord.stdout).Trim(), [ref]$schedulerGrandchildPid)) {
        throw 'Scheduler root-exit self-check did not report its controlled grandchild PID.'
    }
    $schedulerOrphanSnapshot = $orphanRecord.orphanFailureSnapshot
    if ($null -eq $schedulerOrphanSnapshot -or $null -ne $schedulerOrphanSnapshot.captureError -or $null -eq $schedulerOrphanSnapshot.job -or
        $schedulerOrphanSnapshot.rootPid -ne $orphanRecord.pid -or $schedulerOrphanSnapshot.activeProcessCount -lt 1 -or
        $schedulerOrphanSnapshot.triggerMonotonicMilliseconds -lt $schedulerOrphanSnapshot.rootExitObservedMonotonicMilliseconds -or
        -not $schedulerOrphanSnapshot.job.complete -or $schedulerOrphanSnapshot.job.returnedProcessIdCount -lt 1) {
        throw 'Scheduler root-exit self-check did not capture complete pre-termination orphan evidence.'
    }
    $schedulerGrandchildIdentity = @($schedulerOrphanSnapshot.job.processes | Where-Object {
        $_.processId -eq $schedulerGrandchildPid -and $_.confirmedJobMembership -eq $true -and
        $_.identityState -ceq 'confirmed' -and $_.zeroWaitState -ceq 'running' -and
        -not [string]::IsNullOrWhiteSpace($_.imagePath) -and
        -not [string]::IsNullOrWhiteSpace($_.imageName) -and
        $null -ne $_.creationTimeFileTime -and
        -not [string]::IsNullOrWhiteSpace($_.creationTimeUtc)
    })
    if ($schedulerGrandchildIdentity.Count -ne 1) {
        throw 'Scheduler root-exit self-check snapshot did not identify its running same-Job child with durable identity.'
    }
    try {
        $schedulerGrandchild = [Diagnostics.Process]::GetProcessById($schedulerGrandchildPid)
        try { $schedulerGrandchild.Refresh(); if (-not $schedulerGrandchild.HasExited) { throw 'Scheduler-owned grandchild survived cleanup.' } }
        finally { $schedulerGrandchild.Dispose() }
    }
    catch [ArgumentException] { }
    $script:Evidence.schedulerMultiJobSelfCheckVerified = $true
}

try {
    if ($DeadlineSeconds -eq 0) {
        $DeadlineSeconds = switch ($Mode) {
            'Complete' { 600 }
            'List' { 60 }
            'Focused' { 60 }
            'SelfCheck' { 45 }
            'Build' { 1200 }
        }
    }
    if (($Mode -eq 'Complete' -and $DeadlineSeconds -gt 600) -or
        (($Mode -eq 'List' -or $Mode -eq 'Focused') -and $DeadlineSeconds -gt 60)) {
        throw "DeadlineSeconds exceeds the maximum for mode '$Mode'."
    }
    if ($CleanupReserveSeconds -ge $DeadlineSeconds) { throw 'CleanupReserveSeconds must be smaller than DeadlineSeconds.' }
    $script:DeadlineMilliseconds = [int64]$DeadlineSeconds * 1000L
    $script:Evidence.deadlineSeconds = $DeadlineSeconds
    # The single monotonic clock includes type compilation, artifact and ledger
    # verification, evidence setup, every child, and final cleanup verification.
    $script:RunTimer = [Diagnostics.Stopwatch]::StartNew()

    if ([string]::IsNullOrWhiteSpace($RepositoryRoot)) {
        $RepositoryRoot = Split-Path -Parent $PSScriptRoot
    }
    $script:RepositoryRoot = Resolve-AbsoluteDirectory -Path $RepositoryRoot
    $script:Evidence.repositoryRoot = $script:RepositoryRoot
    $script:EvidenceDirectory = New-EvidenceDirectory -Root $script:RepositoryRoot -Requested $EvidenceDirectory
    $script:EvidenceFile = Join-Path $script:EvidenceDirectory 'result.json'

    $helperPath = Join-Path $PSScriptRoot 'RetainedTests\OwnedJob.cs'
    Add-Type -Path $helperPath

    if ($Mode -eq 'SelfCheck') {
        Invoke-SelfCheck
    }
    elseif ($Mode -eq 'Build') {
        $resolvedStack = Resolve-AbsoluteFile -Path $StackExe -Label 'StackExe'
        if ([string]::IsNullOrWhiteSpace($BuildManifestPath)) { throw 'BuildManifestPath is required for Build mode.' }
        [object[]]$inputsBeforeBuild = @(Get-BuildInputInventory -Root $script:RepositoryRoot)
        $build = Invoke-OwnedProcess -Id 'build' -Executable $resolvedStack -Arguments @(
            '--stack-yaml', (Join-Path $script:RepositoryRoot 'stack.yaml'),
            'build', '--pedantic', '--test', '--bench', '--no-run-tests', '--no-run-benchmarks'
        ) -WorkingDirectory $script:RepositoryRoot -Environment (Get-ProcessEnvironment $null $false)
        Assert-SuccessfulInvocation $build
        [object[]]$inputsAfterBuild = @(Get-BuildInputInventory -Root $script:RepositoryRoot)
        Assert-MatchingBuildInputs -Expected $inputsBeforeBuild -Actual $inputsAfterBuild -Context 'During build'
        $stackDistDirectory = Get-StackDistDirectory -ResolvedStack $resolvedStack
        $artifacts = Get-ArtifactPaths
        $expectedStackArtifacts = Get-ExpectedStackArtifacts -Root $script:RepositoryRoot -DistDirectory $stackDistDirectory
        Assert-StackArtifactPaths -Explicit $artifacts -Expected $expectedStackArtifacts
        $manifest = New-BuildManifest -Root $script:RepositoryRoot -Artifacts $artifacts -BuildInputs $inputsAfterBuild -StackDistDirectory $stackDistDirectory
        $manifestOutput = [IO.Path]::GetFullPath($BuildManifestPath)
        if (Test-PathInsideRoot -Path $manifestOutput -Root $script:RepositoryRoot) {
            throw 'BuildManifestPath must be outside the repository.'
        }
        [IO.Directory]::CreateDirectory([IO.Path]::GetDirectoryName($manifestOutput)) | Out-Null
        [IO.File]::WriteAllText($manifestOutput, (($manifest | ConvertTo-Json -Depth 8) + [Environment]::NewLine), [Text.UTF8Encoding]::new($false))
    }
    else {
        $artifacts = Get-ArtifactPaths
        [void](Read-And-VerifyBuildManifest -Root $script:RepositoryRoot -Artifacts $artifacts)
        $ledger = Read-And-VerifyLedger

        if ($Mode -eq 'Complete') {
            foreach ($componentRecord in @($ledger.components)) {
                $name = [string]$componentRecord.name
                $executable = Get-ComponentExecutable -Name $name -Artifacts $artifacts
                Assert-ExactRegistration -ComponentRecord $componentRecord -Executable $executable -ResolvedAdraiExe $artifacts.adrai -Id ("list-{0}" -f $name)
            }
            $schedule = Read-And-VerifyOrdinaryPartitions -Ledger $ledger -Artifacts $artifacts
            foreach ($partition in @($schedule.partitions)) {
                Assert-PartitionRegistration -Partition $partition -Executable $artifacts.ordinary -ResolvedAdraiExe $artifacts.adrai
            }
            Invoke-OwnedProcessQueue -Jobs @($schedule.jobs) -MaximumActiveJobs $schedule.maximumActiveJobs -RunId 'complete-retained-tests'
        }
        else {
            if ([string]::IsNullOrWhiteSpace($Component)) { throw "Component is required for mode '$Mode'." }
            $componentRecord = @($ledger.components | Where-Object { [string]$_.name -ceq $Component })[0]
            $executable = Get-ComponentExecutable -Name $Component -Artifacts $artifacts
            Assert-ExactRegistration -ComponentRecord $componentRecord -Executable $executable -ResolvedAdraiExe $artifacts.adrai -Id ("list-{0}" -f $Component)
            if ($Mode -eq 'Focused') {
                if ([string]::IsNullOrWhiteSpace($TestName) -or -not ($componentRecord.verifiedTests -ccontains $TestName)) {
                    throw 'TestName must exactly match a retained test in the selected component.'
                }
                $arguments = @($componentRecord.args | ForEach-Object { [string]$_ }) + @('-p', (ConvertTo-ExactTastyPattern -Name $TestName))
                $focused = Invoke-OwnedProcess -Id 'focused' -Executable $executable -Arguments $arguments -WorkingDirectory $script:RepositoryRoot -Environment (Get-ProcessEnvironment $artifacts.adrai)
                Assert-SuccessfulInvocation $focused
                Assert-TastyExecutionCount $focused 1
            }
        }
    }

    if ($script:RunTimer.ElapsedMilliseconds -gt $script:DeadlineMilliseconds) {
        throw "Mode '$Mode' exceeded its monotonic deadline during setup or cleanup."
    }
    $script:Evidence.cleanupVerified = $true
    $script:Evidence.result = 'passed'
    Write-Evidence
    if ($script:RunTimer.ElapsedMilliseconds -gt $script:DeadlineMilliseconds) {
        throw "Mode '$Mode' exceeded its monotonic deadline while finalizing evidence."
    }
    Write-Output "Retained-test runner passed mode '$Mode'. Evidence: $script:EvidenceFile"
    exit 0
}
catch {
    $failure = $_
    $script:Evidence.result = 'failed'
    $script:Evidence.error = $failure.Exception.Message
    try { Write-Evidence } catch { }
    [Console]::Error.WriteLine("Retained-test runner failed: {0}", ($failure.Exception.Message -replace '[\r\n]+', ' | '))
    if ($null -ne $script:EvidenceFile) { [Console]::Error.WriteLine("Evidence: {0}", $script:EvidenceFile) }
    exit 1
}
