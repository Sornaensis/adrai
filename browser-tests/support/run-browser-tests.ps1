param([Parameter(ValueFromRemainingArguments=$true)][string[]]$TestArguments)
$ErrorActionPreference = 'Stop'
$root = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$repo = [IO.Path]::GetFullPath((Join-Path $root '..'))
$output = Join-Path ([IO.Path]::GetTempPath()) ('adrai-browser-' + [Guid]::NewGuid().ToString('N'))
if ($output.StartsWith($repo + '\', [StringComparison]::OrdinalIgnoreCase)) { throw 'Browser output must be external.' }
[IO.Directory]::CreateDirectory($output) | Out-Null
Write-Output "Browser output: $output"
$node = (Get-Command node -ErrorAction Stop).Source
Add-Type -Path (Join-Path $repo 'tools/RetainedTests/OwnedJob.cs')
$job = [Adrai.RetainedTests.OwnedJob]::new()
$process = $null
$code = 1
try {
    $environment = @{ ADRAI_BROWSER_OUTPUT=$output; TEMP=$output; TMP=$output }
    $arguments = [string[]](@((Join-Path $PSScriptRoot 'run-browser-tests.mjs')) + $TestArguments)
    $process = $job.Launch($node, $arguments, $root, $environment, (Join-Path $output 'stdout.log'), (Join-Path $output 'stderr.log'), 5000)
    while (-not $process.WaitForExit(250)) { }
    $code = $process.GetExitCode()
} finally {
    try {
        # This wait only drains owned descendants; it is not a test-performance cap.
        if (-not $job.WaitForEmpty(5000)) { $job.Terminate(1); $code=1 }
        $empty = $job.WaitForEmpty(5000)
        if (-not $empty) { $code=1 }
        Write-Output "Owned browser processes cleaned up: $empty"
    } finally {
        try { if ($null -ne $process) { $process.Dispose() } } finally { $job.Dispose() }
    }
    Get-Content -LiteralPath (Join-Path $output 'stdout.log') -ErrorAction SilentlyContinue
    Get-Content -LiteralPath (Join-Path $output 'stderr.log') -ErrorAction SilentlyContinue
}
exit $code
