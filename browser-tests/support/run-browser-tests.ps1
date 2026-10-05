param(
    [Parameter(Mandatory=$true)][string]$NodeExe,
    [Parameter(Mandatory=$true)][ValidateSet('win32','linux')][string]$NodePlatform,
    [Parameter(Mandatory=$true)][ValidateSet('x64','arm64')][string]$NodeArchitecture,
    [ValidateRange(1,3600000)][int]$RemainingMilliseconds=3600000,
    [string]$ExpiresUtc,
    [string]$TestArgumentsBase64,
    [Parameter(ValueFromRemainingArguments=$true)][string[]]$TestArguments
)
Set-StrictMode -Version Latest
$ErrorActionPreference='Stop'
$clock=[Diagnostics.Stopwatch]::StartNew()
$expiry=if($ExpiresUtc){[DateTimeOffset]::Parse($ExpiresUtc,[Globalization.CultureInfo]::InvariantCulture)}else{[DateTimeOffset]::UtcNow.AddMilliseconds($RemainingMilliseconds)}
$root=[IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$repo=[IO.Path]::GetFullPath((Join-Path $root '..'))
$windows=[Environment]::OSVersion.Platform-eq[PlatformID]::Win32NT
[string]$output=Join-Path ([IO.Path]::GetTempPath()) ('adrai-browser-'+[Guid]::NewGuid().ToString('N'))
$stdout=Join-Path $output 'stdout.log'
$stderr=Join-Path $output 'stderr.log'
$worker=Join-Path $PSScriptRoot 'run-browser-tests.mjs'
$job=$null
$process=$null
$code=1
$record=[ordered]@{
    remaining_ms=$RemainingMilliseconds; expires_utc=$expiry.ToString('o'); root_pid=$null; controller_pid=$null; member_count_available=$windows
    root_exited=$false; root_exit_code=$null; job_empty_before_cleanup=$false; job_empty_after_cleanup=$false
    active_member_count=$null; namespace_active=$null; cleanup_verified=$false; timed_out=$false; error=$null
    launch_failure_cleanup_verified=$null; hashes=[ordered]@{}; output=$output
    cleanup_authority=$(if($windows){'job-empty-and-root-exit'}else{'namespace-tree-reaped-and-readers-joined'})
}
function Remaining { return [Math]::Max(0,[Math]::Min($RemainingMilliseconds-[int]$clock.ElapsedMilliseconds,[int64]($expiry-[DateTimeOffset]::UtcNow).TotalMilliseconds)) }
function WorkBudget { return [Math]::Max(0,(Remaining)-10000) }
function Hash([string]$path) { return (Get-FileHash -LiteralPath $path).Hash.ToLowerInvariant() }
function Failure([string]$message) { $record.error=(@($record.error,$message)|Where-Object{$_})-join'; ';$script:code=1 }
try {
    if($TestArgumentsBase64){
        $decodedArguments=ConvertFrom-Json -InputObject ([Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($TestArgumentsBase64)))
        $TestArguments=[string[]]@()
        if($null-ne$decodedArguments){$TestArguments=[string[]]$decodedArguments}
    }
    if($output.StartsWith($repo+[IO.Path]::DirectorySeparatorChar,[StringComparison]::OrdinalIgnoreCase)){throw 'Browser output must be external.'}
    [IO.Directory]::CreateDirectory($output)|Out-Null
    Write-Output ('Browser output: '+$output)
    if(-not[IO.Path]::IsPathRooted($NodeExe)-or-not(Test-Path -LiteralPath $NodeExe -PathType Leaf)){throw 'NodeExe must be the selected absolute Node executable.'}
    if(($NodePlatform-eq'win32')-ne$windows){throw 'Node platform does not match the ownership provider.'}
    $provider=Join-Path $repo ('tools/RetainedTests/'+$(if($windows){'OwnedJob.cs'}else{'LinuxOwnedJob.cs'}))
    foreach($pair in @(@('node',$NodeExe),@('supervisor',$PSCommandPath),@('worker',$worker),@('provider',$provider),@('browser_lock',(Join-Path $root 'package-lock.json')),@('web_lock',(Join-Path $repo 'web/package-lock.json')),@('elm_manifest',(Join-Path $repo 'web/elm.json')),@('fixture',(Join-Path $root 'fixtures/BrowserFixture.elm')),@('playwright_config',(Join-Path $root 'playwright.config.ts')))){$record.hashes[$pair[0]]=Hash $pair[1]}
    if(-not$windows){
        $owner=$env:ADRAI_RETAINED_OWNER_EXE
        if(-not$owner-or-not[IO.Path]::IsPathRooted($owner)){throw 'Select the provenance-bound native Linux owner.'}
        $ownerRecord=Get-Content -LiteralPath ($owner+'.json') -Raw|ConvertFrom-Json
        if($ownerRecord.schemaVersion-ne1-or$ownerRecord.binarySha256-cne(Hash $owner)-or$ownerRecord.sourceSha256-cne(Hash (Join-Path $repo 'tools/RetainedTests/linux_owner.c'))){throw 'Linux owner source/binary provenance changed.'}
        $record.hashes.owner=Hash $owner
        $record.hashes.owner_build_record=Hash ($owner+'.json')
    }
    Add-Type -Path $provider
    if(-not$windows){[Adrai.RetainedTests.OwnedJob]::HelperPath=$owner}
    if((WorkBudget)-le0){throw 'No browser execution and cleanup allocation remains.'}
    $job=[Adrai.RetainedTests.OwnedJob]::new()
    $environment=@{ADRAI_BROWSER_OUTPUT=$output;TEMP=$output;TMP=$output;TMPDIR=$output}
    $arguments=[string[]](@($worker)+$TestArguments)
    try{$process=$job.Launch($NodeExe,$arguments,$root,$environment,$stdout,$stderr,[Math]::Min(5000,(WorkBudget)))}
    catch [Adrai.RetainedTests.OwnedLaunchException]{$record.launch_failure_cleanup_verified=$_.Exception.ProcessCleanupVerified;throw}
    if($windows){$record.root_pid=$process.ProcessId}else{$record.controller_pid=$process.ProcessId}
    $record.root_exited=$process.WaitForExit((WorkBudget))
    if($record.root_exited){
        $record.root_exit_code=$process.GetExitCode()
        $record.job_empty_before_cleanup=$job.WaitForEmpty((WorkBudget))
        $code=$record.root_exit_code
        if(-not$record.job_empty_before_cleanup){$code=1;$record.timed_out=$true}
    }else{$record.timed_out=$true;$code=1}
}catch{Failure $_.Exception.Message}
finally{
    if($null-ne$job){
        try{
            $active=$job.ActiveProcessCount()
            if($windows){$record.active_member_count=$active}else{$record.namespace_active=($active-ne0)}
            if($active-ne0){$job.Terminate(1);$code=1}
            $record.job_empty_after_cleanup=$job.WaitForEmpty([Math]::Max(0,(Remaining)-1000))
        }catch{Failure $_.Exception.Message}
    }else{$record.job_empty_after_cleanup=($record.launch_failure_cleanup_verified-ne$false)}
    # A cancelled Linux namespace may have no ROOT frame; TREE plus joined
    # readers verifies cleanup while its payload exit code stays unknown.
    if($windows-and$null-ne$process){try{$record.root_exited=$process.WaitForExit([Math]::Max(0,(Remaining)-1000));if($record.root_exited-and$null-eq$record.root_exit_code){$record.root_exit_code=$process.GetExitCode()}}catch{Failure $_.Exception.Message}}
    $record.cleanup_verified=$record.job_empty_after_cleanup-and(-not$windows-or$null-eq$process-or$record.root_exited)-and($record.launch_failure_cleanup_verified-ne$false)
    if($null-ne$process){try{$process.Dispose()}catch{Failure $_.Exception.Message;$record.cleanup_verified=$false}}
    if($null-ne$job){try{$job.Dispose()}catch{Failure $_.Exception.Message;$record.cleanup_verified=$false}}
    if(-not$record.cleanup_verified-or$record.error-or(Remaining)-le0){$code=1}
    $record.elapsed_ms=[int]$clock.ElapsedMilliseconds
    $record.exit_code=$code
    try{
        [IO.File]::WriteAllText((Join-Path $output 'result.json'),($record|ConvertTo-Json -Depth 8))
        if(Test-Path -LiteralPath $stdout){Get-Content -LiteralPath $stdout}
        if(Test-Path -LiteralPath $stderr){Get-Content -LiteralPath $stderr}
        Write-Output ('Owned browser processes cleaned up: '+$record.cleanup_verified)
        Write-Output ('evidence_path='+ (Join-Path $output 'result.json'))
        if((Remaining)-le0){$code=1;$record.exit_code=1;$record.elapsed_ms=[int]$clock.ElapsedMilliseconds;[IO.File]::WriteAllText((Join-Path $output 'result.json'),($record|ConvertTo-Json -Depth 8))}
    }catch{$code=1;Write-Error $_.Exception.Message}
}
exit $code
