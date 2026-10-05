param(
    [Parameter(Mandatory=$true)][string] $NodeExe,
    [Parameter(Mandatory=$true)][ValidateSet('win32','linux')][string] $NodePlatform,
    [Parameter(Mandatory=$true)][ValidateSet('x64','arm64')][string] $NodeArchitecture,
    [ValidateRange(1,3600000)][int] $RemainingMilliseconds = 3600000,
    [string] $ExpiresUtc,
    [ValidateSet('normal','timeout','spawn-failure','early-success','early-error')][string] $Probe = 'normal'
)
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$clock = [Diagnostics.Stopwatch]::StartNew()
$expiry = if($ExpiresUtc){[DateTimeOffset]::Parse($ExpiresUtc,[Globalization.CultureInfo]::InvariantCulture)}else{[DateTimeOffset]::UtcNow.AddMilliseconds($RemainingMilliseconds)}
$webRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$repositoryRoot = [IO.Path]::GetFullPath((Join-Path $webRoot '..'))
$windows = [Environment]::OSVersion.Platform -eq [PlatformID]::Win32NT
$workerPath = Join-Path $PSScriptRoot 'run-components.mjs'
$testPath = Join-Path $PSScriptRoot 'ExplorerTest.elm'
$fixturePath = Join-Path $webRoot 'fixtures/api-v1.json'
$evidenceDirectory = Join-Path ([IO.Path]::GetTempPath()) ('adrai-p704-components-' + [Guid]::NewGuid().ToString('N'))
$project = Join-Path $evidenceDirectory 'elm-project'
$externalTests = Join-Path $project 'tests'
$generatedPath = Join-Path $externalTests 'FixtureData.elm'
$externalTest = Join-Path $externalTests 'ExplorerTest.elm'
$stdoutPath = Join-Path $evidenceDirectory 'stdout.log'
$stderrPath = Join-Path $evidenceDirectory 'stderr.log'
$utf8 = New-Object System.Text.UTF8Encoding($false)
$job = $null
$root = $null
$record = [ordered]@{
    probe=$Probe; deadline_ms=$RemainingMilliseconds; expires_utc=$expiry.ToString('o'); root_pid=$null; controller_pid=$null
    member_count_available=$windows; job_active_before_cleanup=$null; namespace_active_before_cleanup=$null
    root_exited=$false; root_exit_code=$null; job_empty_before_cleanup=$false; job_empty_after_cleanup=$false
    timed_out=$false; cancellation_requested=$false; fixture_removed=$false; cleanup_verified=$false
    launch_failure_cleanup_verified=$null; launch_failure_native_error=$null; error=$null
    cleanup_authority=$(if($windows){'job-empty-and-root-exit'}else{'namespace-tree-reaped-and-readers-joined'})
    hashes=[ordered]@{}; stdout_path=$stdoutPath; stderr_path=$stderrPath; project_path=$project
}
function Remaining { return [Math]::Max(0,[Math]::Min($RemainingMilliseconds-[int]$clock.ElapsedMilliseconds,[int64]($expiry-[DateTimeOffset]::UtcNow).TotalMilliseconds)) }
function WorkBudget { return [Math]::Max(0,(Remaining)-10000) }
function Hash([string]$path) { return (Get-FileHash -LiteralPath $path).Hash.ToLowerInvariant() }
function Failure([string]$message) { $record.error = (@($record.error,$message) | Where-Object { $_ }) -join '; ' }
try {
    [IO.Directory]::CreateDirectory($externalTests) | Out-Null
    if ($evidenceDirectory.StartsWith($repositoryRoot+[IO.Path]::DirectorySeparatorChar,[StringComparison]::OrdinalIgnoreCase)) { throw 'Component project must be external.' }
    if (-not [IO.Path]::IsPathRooted($NodeExe) -or -not (Test-Path -LiteralPath $NodeExe -PathType Leaf)) { throw 'NodeExe must be the selected absolute Node executable.' }
    if (($NodePlatform -eq 'win32') -ne $windows) { throw 'Node platform does not match the ownership provider.' }
    $compiler = Join-Path $webRoot ('node_modules/@elm_binaries/'+$NodePlatform+'_'+$NodeArchitecture+'/'+$(if($windows){'elm.exe'}else{'elm'}))
    if (-not (Test-Path -LiteralPath $compiler -PathType Leaf)) { throw 'The locked native Elm compiler is missing.' }
    $provider = Join-Path $repositoryRoot ('tools/RetainedTests/'+$(if($windows){'OwnedJob.cs'}else{'LinuxOwnedJob.cs'}))
    foreach($pair in @(@('provider',$provider),@('supervisor',$PSCommandPath),@('worker',$workerPath),@('tests',$testPath),@('fixture',$fixturePath),@('elm_manifest',(Join-Path $webRoot 'elm.json')),@('node',$NodeExe),@('compiler',$compiler))) { $record.hashes[$pair[0]]=Hash $pair[1] }
    if (-not $windows) {
        $owner=$env:ADRAI_RETAINED_OWNER_EXE
        if (-not $owner -or -not [IO.Path]::IsPathRooted($owner)) { throw 'Select the provenance-bound native Linux owner.' }
        $ownerRecord=Get-Content -LiteralPath ($owner+'.json') -Raw | ConvertFrom-Json
        if ($ownerRecord.schemaVersion-ne1 -or $ownerRecord.binarySha256-cne(Hash $owner) -or $ownerRecord.sourceSha256-cne(Hash (Join-Path $repositoryRoot 'tools/RetainedTests/linux_owner.c'))) { throw 'Linux owner source/binary provenance changed.' }
        $record.hashes.owner=Hash $owner
        $record.hashes.owner_build_record=Hash ($owner+'.json')
    }
    Add-Type -Path $provider
    if (-not $windows) { [Adrai.RetainedTests.OwnedJob]::HelperPath=$owner }
    $manifest=Get-Content -LiteralPath (Join-Path $webRoot 'elm.json') -Raw | ConvertFrom-Json
    $manifest.'source-directories'=@((Join-Path $webRoot 'src'))
    [IO.File]::WriteAllText((Join-Path $project 'elm.json'),($manifest|ConvertTo-Json -Depth 8),$utf8)
    [IO.File]::Copy($testPath,$externalTest)
    $literal=ConvertTo-Json -InputObject ([IO.File]::ReadAllText($fixturePath)) -Compress
    [IO.File]::WriteAllText($generatedPath,"module FixtureData exposing (document)`n`ndocument : String`ndocument =`n    $literal`n",$utf8)
    if ((WorkBudget)-le0) { throw 'No component execution and cleanup allocation remains.' }
    $job=[Adrai.RetainedTests.OwnedJob]::new()
    $executable=if($Probe-eq'spawn-failure'){Join-Path $evidenceDirectory 'missing-p704-probe'}else{$NodeExe}
    $arguments=[string[]]@($workerPath,$(if($Probe-eq'spawn-failure'){'normal'}else{$Probe}),$project,$externalTest,$compiler)
    try { $root=$job.Launch($executable,$arguments,$webRoot,$null,$stdoutPath,$stderrPath,[Math]::Min(5000,(WorkBudget))) }
    catch [Adrai.RetainedTests.OwnedLaunchException] { $record.launch_failure_cleanup_verified=$_.Exception.ProcessCleanupVerified; $record.launch_failure_native_error=$_.Exception.NativeErrorCode; throw }
    catch [System.ComponentModel.Win32Exception] {
        $record.launch_failure_native_error=$_.Exception.NativeErrorCode
        if($windows-and$Probe-eq'spawn-failure'-and$_.Exception.NativeErrorCode-eq2){$record.launch_failure_cleanup_verified='not-applicable-before-create'}
        throw
    }
    if($windows){$record.root_pid=$root.ProcessId}else{$record.controller_pid=$root.ProcessId}
    if($Probe-eq'timeout') {
        # Keep the existing probe name; cancel on the actual descendant signal.
        while((WorkBudget)-gt0) {
            if(Test-Path -LiteralPath $stdoutPath) {
                $stream=[IO.File]::Open($stdoutPath,[IO.FileMode]::Open,[IO.FileAccess]::Read,[IO.FileShare]::ReadWrite)
                try {
                    $reader=[IO.StreamReader]::new($stream)
                    try { $ready=$reader.ReadToEnd()-match'owned_descendant_pid=\d+' }
                    finally { $reader.Dispose() }
                } finally { $stream.Dispose() }
                if($ready){$record.cancellation_requested=$true;break}
            }
            if($root.WaitForExit(0)){throw 'Cancellation fixture ended before descendant readiness.'}
            [Threading.Thread]::Sleep(10)
        }
        if(-not$record.cancellation_requested){$record.timed_out=$true;throw 'Component readiness exhausted the caller allocation.'}
    } else {
        $record.root_exited=$root.WaitForExit((WorkBudget))
        if($record.root_exited){$record.root_exit_code=$root.GetExitCode();$record.job_empty_before_cleanup=$job.WaitForEmpty($(if($Probe-eq'normal'){WorkBudget}else{0}))}
        else{$record.timed_out=$true}
    }
} catch { Failure $_.Exception.Message }
finally {
    if($null-ne$job) {
        try {
            $active=$job.ActiveProcessCount()
            if($windows){$record.job_active_before_cleanup=$active}else{$record.namespace_active_before_cleanup=($active-ne0)}
            if($active-ne0){$job.Terminate(125)}
            $record.job_empty_after_cleanup=$job.WaitForEmpty([Math]::Max(0,(Remaining)-1000))
        } catch { Failure $_.Exception.Message }
    } else { $record.job_empty_after_cleanup=($record.launch_failure_cleanup_verified-ne$false) }
    # Linux cancellation can reap the namespace without observing a payload
    # ROOT frame. TREE and joined readers prove absence; no exit is invented.
    if($windows-and$null-ne$root){try{$record.root_exited=$root.WaitForExit([Math]::Max(0,(Remaining)-1000));if($record.root_exited){$record.root_exit_code=$root.GetExitCode()}}catch{Failure $_.Exception.Message}}
    $record.cleanup_verified=$record.job_empty_after_cleanup-and(-not$windows-or$null-eq$root-or$record.root_exited)-and($record.launch_failure_cleanup_verified-ne$false)
    if($null-ne$root){try{$root.Dispose()}catch{Failure $_.Exception.Message;$record.cleanup_verified=$false}}
    if($null-ne$job){try{$job.Dispose()}catch{Failure $_.Exception.Message;$record.cleanup_verified=$false}}
    if($record.cleanup_verified){
        try { if(Test-Path -LiteralPath $generatedPath){Remove-Item -LiteralPath $generatedPath -Force};$record.fixture_removed=-not(Test-Path -LiteralPath $generatedPath) } catch { Failure $_.Exception.Message }
    }
    $normal=$Probe-eq'normal'-and$record.root_exit_code-eq0-and$record.job_empty_before_cleanup-and-not$record.timed_out-and-not$record.error
    $cancel=$Probe-eq'timeout'-and$record.cancellation_requested-and-not$record.timed_out-and-not$record.job_empty_before_cleanup-and-not$record.error
    $early=$Probe-in@('early-success','early-error')-and$record.root_exit_code-eq$(if($Probe-eq'early-success'){0}else{17})-and-not$record.job_empty_before_cleanup-and-not$record.error
    $missing=$Probe-eq'spawn-failure'-and$null-eq$root-and$record.launch_failure_native_error-eq2-and$record.launch_failure_cleanup_verified-and$record.error
    $record.elapsed_ms=[int]$clock.ElapsedMilliseconds
    $record.exit_code=if(($normal-or$cancel-or$early-or$missing)-and$record.cleanup_verified-and$record.fixture_removed-and$record.elapsed_ms-le$RemainingMilliseconds){0}else{1}
    try {
        [IO.File]::WriteAllText((Join-Path $evidenceDirectory 'result.json'),($record|ConvertTo-Json -Depth 8),$utf8)
        if(Test-Path -LiteralPath $stdoutPath){Get-Content -LiteralPath $stdoutPath}
        if(Test-Path -LiteralPath $stderrPath){Get-Content -LiteralPath $stderrPath}
        Write-Output ('evidence_path='+ (Join-Path $evidenceDirectory 'result.json'))
        if((Remaining)-le0){$record.exit_code=1;$record.elapsed_ms=[int]$clock.ElapsedMilliseconds;[IO.File]::WriteAllText((Join-Path $evidenceDirectory 'result.json'),($record|ConvertTo-Json -Depth 8),$utf8)}
    } catch { $record.exit_code=1;Write-Error $_.Exception.Message }
}
exit $record.exit_code
