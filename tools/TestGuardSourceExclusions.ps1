[CmdletBinding()]
param(
    [Parameter()]
    [string] $GuardPath
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

if ([string]::IsNullOrWhiteSpace($GuardPath)) {
    $GuardPath = Join-Path -Path $PSScriptRoot -ChildPath 'GuardSourceExclusions.ps1'
}

$ignoreContents = @"
/ADRAI_1_Source/
/ADRAI_1_Search_Enhanced_Haskell_Reimplementation_Plan.md
"@
$fixtureBytes = [Text.Encoding]::UTF8.GetBytes("dummy protected prototype fixture`n")
$tempBase = [IO.Path]::GetFullPath([IO.Path]::GetTempPath()).TrimEnd([IO.Path]::DirectorySeparatorChar, [IO.Path]::AltDirectorySeparatorChar)
$testParent = Join-Path -Path $tempBase -ChildPath ("adrai-source-guard-tests-{0}" -f [Guid]::NewGuid().ToString('N'))
$powerShellExecutable = [Diagnostics.Process]::GetCurrentProcess().MainModule.FileName

function Invoke-TestGit {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root,

        [Parameter(Mandatory = $true)]
        [string[]] $Arguments
    )

    $allArguments = @('--no-replace-objects', '-C', $Root) + $Arguments
    $previousErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    try {
        $output = @(& git @allArguments 2>&1)
        $exitCode = $LASTEXITCODE
    }
    finally {
        $ErrorActionPreference = $previousErrorActionPreference
    }
    if ($exitCode -ne 0) {
        throw "Test setup Git command failed with exit code $exitCode (git $($allArguments -join ' ')): $(($output | ForEach-Object { $_.ToString() }) -join [Environment]::NewLine)"
    }
    return $output
}

function New-TestRoot {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Name,

        [switch] $InitializeGit
    )

    $root = Join-Path -Path $testParent -ChildPath $Name
    [void] [IO.Directory]::CreateDirectory((Join-Path -Path $root -ChildPath 'ADRAI_1_Source'))
    [void] [IO.Directory]::CreateDirectory((Join-Path -Path $root -ChildPath 'tools'))
    [IO.File]::WriteAllText((Join-Path -Path $root -ChildPath '.gitignore'), $ignoreContents, [Text.UTF8Encoding]::new($false))
    [IO.File]::WriteAllBytes((Join-Path -Path $root -ChildPath 'ADRAI_1_Source/fixture.bin'), $fixtureBytes)

    $hashOutput = @(Invoke-TestGit -Root $root -Arguments @('hash-object', '--no-filters', '--', 'ADRAI_1_Source/fixture.bin'))
    if ($hashOutput.Count -ne 1) {
        throw 'Test fixture git hash-object returned an unexpected result.'
    }
    $fixtureBlobId = $hashOutput[0].ToString().Trim()
    $objectFormat = if ($fixtureBlobId -match '^[0-9a-f]{40}$') {
        'sha1'
    }
    elseif ($fixtureBlobId -match '^[0-9a-f]{64}$') {
        'sha256'
    }
    else {
        throw "Test fixture git hash-object returned malformed ID '$fixtureBlobId'."
    }
    $manifestContents = "object-format=$objectFormat`nfile-count=1`n$fixtureBlobId`n"
    [IO.File]::WriteAllText((Join-Path -Path $root -ChildPath 'tools/ProtectedSourceBlobIds.txt'), $manifestContents, [Text.UTF8Encoding]::new($false))

    if ($InitializeGit) {
        [void] (Invoke-TestGit -Root $root -Arguments @('init', '--quiet', "--object-format=$objectFormat"))
        [void] (Invoke-TestGit -Root $root -Arguments @('config', 'user.name', 'Source Guard Test'))
        [void] (Invoke-TestGit -Root $root -Arguments @('config', 'user.email', 'source-guard@example.invalid'))
    }

    return $root
}

function Invoke-GuardProcess {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root
    )

    $arguments = @(
        '-NoLogo', '-NoProfile', '-NonInteractive',
        '-ExecutionPolicy', 'Bypass',
        '-File', $GuardPath,
        '-RepositoryRoot', $Root
    )
    $previousErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    try {
        $output = @(& $powerShellExecutable @arguments 2>&1)
        $exitCode = $LASTEXITCODE
    }
    finally {
        $ErrorActionPreference = $previousErrorActionPreference
    }
    return [pscustomobject] @{
        ExitCode = $exitCode
        Output = ($output | ForEach-Object { $_.ToString() }) -join [Environment]::NewLine
    }
}

function Assert-GuardResult {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject] $Result,

        [Parameter(Mandatory = $true)]
        [bool] $ShouldPass,

        [Parameter(Mandatory = $true)]
        [string] $ExpectedText
    )

    if ($ShouldPass -and $Result.ExitCode -ne 0) {
        throw "Expected guard success, got exit code $($Result.ExitCode): $($Result.Output)"
    }
    if (-not $ShouldPass -and $Result.ExitCode -eq 0) {
        throw "Expected guard rejection, but it passed: $($Result.Output)"
    }
    $normalizedOutput = [Text.RegularExpressions.Regex]::Replace($Result.Output, '\s+', ' ')
    $normalizedExpectedText = [Text.RegularExpressions.Regex]::Replace($ExpectedText, '\s+', ' ')
    if ($normalizedOutput.IndexOf($normalizedExpectedText, [StringComparison]::Ordinal) -lt 0) {
        throw "Guard output did not contain '$ExpectedText': $($Result.Output)"
    }
}

function Add-AndCommit {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root,

        [Parameter(Mandatory = $true)]
        [string] $Path,

        [Parameter(Mandatory = $true)]
        [string] $Message,

        [switch] $Force
    )

    $addArguments = if ($Force) { @('add', '-f', '--', $Path) } else { @('add', '--', $Path) }
    [void] (Invoke-TestGit -Root $Root -Arguments $addArguments)
    [void] (Invoke-TestGit -Root $Root -Arguments @('commit', '--quiet', '-m', $Message))
}

$tests = @(
    [pscustomobject] @{
        Name = 'pre-init root passes after denylist verification'
        Action = {
            $root = New-TestRoot -Name 'pre-init'
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $true -ExpectedText 'pre-Git-init'
        }
    },
    [pscustomobject] @{
        Name = 'clean repository passes'
        Action = {
            $root = New-TestRoot -Name 'clean' -InitializeGit
            [void] [IO.Directory]::CreateDirectory((Join-Path -Path $root -ChildPath 'src'))
            [IO.File]::WriteAllText((Join-Path -Path $root -ChildPath 'src/safe.txt'), "distinct safe bytes`n", [Text.UTF8Encoding]::new($false))
            Add-AndCommit -Root $root -Path 'src/safe.txt' -Message 'safe commit'
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $true -ExpectedText 'guard passed'
        }
    },
    [pscustomobject] @{
        Name = 'clean repository passes after prototype deletion'
        Action = {
            $root = New-TestRoot -Name 'post-prototype-deletion' -InitializeGit
            [void] [IO.Directory]::CreateDirectory((Join-Path -Path $root -ChildPath 'src'))
            [IO.File]::WriteAllText((Join-Path -Path $root -ChildPath 'src/safe.txt'), "distinct post-deletion bytes`n", [Text.UTF8Encoding]::new($false))
            Add-AndCommit -Root $root -Path 'src/safe.txt' -Message 'safe commit before prototype deletion'
            $prototypePath = [IO.Path]::GetFullPath((Join-Path -Path $root -ChildPath 'ADRAI_1_Source'))
            $expectedPrototypePath = [IO.Path]::GetFullPath($root).TrimEnd([IO.Path]::DirectorySeparatorChar, [IO.Path]::AltDirectorySeparatorChar) + [IO.Path]::DirectorySeparatorChar + 'ADRAI_1_Source'
            if (-not $prototypePath.Equals($expectedPrototypePath, [StringComparison]::OrdinalIgnoreCase)) {
                throw "Refusing test fixture deletion outside literal prototype path: $prototypePath"
            }
            Remove-Item -LiteralPath $prototypePath -Recurse -Force
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $true -ExpectedText 'immutable blob manifest'
        }
    },
    [pscustomobject] @{
        Name = 'staged protected path is rejected'
        Action = {
            $root = New-TestRoot -Name 'staged-path' -InitializeGit
            [void] (Invoke-TestGit -Root $root -Arguments @('add', '-f', '--', 'ADRAI_1_Source/fixture.bin'))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'protected path is staged/tracked'
        }
    },
    [pscustomobject] @{
        Name = 'tracked protected path is rejected'
        Action = {
            $root = New-TestRoot -Name 'tracked-path' -InitializeGit
            Add-AndCommit -Root $root -Path 'ADRAI_1_Source/fixture.bin' -Message 'protected path commit' -Force
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'protected path is staged/tracked'
        }
    },
    [pscustomobject] @{
        Name = 'historical protected path is rejected after index removal'
        Action = {
            $root = New-TestRoot -Name 'historical-path' -InitializeGit
            Add-AndCommit -Root $root -Path 'ADRAI_1_Source/fixture.bin' -Message 'protected path commit' -Force
            [void] (Invoke-TestGit -Root $root -Arguments @('rm', '--quiet', '--cached', '--', 'ADRAI_1_Source/fixture.bin'))
            [void] (Invoke-TestGit -Root $root -Arguments @('commit', '--quiet', '-m', 'remove protected path from index'))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'protected path exists in reachable history'
        }
    },
    [pscustomobject] @{
        Name = 'staged renamed prototype blob is rejected'
        Action = {
            $root = New-TestRoot -Name 'staged-blob' -InitializeGit
            [void] [IO.Directory]::CreateDirectory((Join-Path -Path $root -ChildPath 'src'))
            [IO.File]::WriteAllBytes((Join-Path -Path $root -ChildPath 'src/renamed.bin'), $fixtureBytes)
            [void] (Invoke-TestGit -Root $root -Arguments @('add', '--', 'src/renamed.bin'))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'protected prototype blob is staged/tracked'
        }
    },
    [pscustomobject] @{
        Name = 'committed renamed prototype blob is rejected from history'
        Action = {
            $root = New-TestRoot -Name 'historical-blob' -InitializeGit
            [void] [IO.Directory]::CreateDirectory((Join-Path -Path $root -ChildPath 'src'))
            [IO.File]::WriteAllBytes((Join-Path -Path $root -ChildPath 'src/renamed.bin'), $fixtureBytes)
            Add-AndCommit -Root $root -Path 'src/renamed.bin' -Message 'renamed protected blob'
            [void] (Invoke-TestGit -Root $root -Arguments @('rm', '--quiet', '--cached', '--', 'src/renamed.bin'))
            [void] (Invoke-TestGit -Root $root -Arguments @('commit', '--quiet', '-m', 'remove renamed blob from index'))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'protected prototype blob exists in reachable history'
        }
    },
    [pscustomobject] @{
        Name = 'reflog-only protected commit is rejected'
        Action = {
            $root = New-TestRoot -Name 'reflog-only' -InitializeGit
            [void] [IO.Directory]::CreateDirectory((Join-Path -Path $root -ChildPath 'src'))
            [IO.File]::WriteAllText((Join-Path -Path $root -ChildPath 'src/base.txt'), "reflog base bytes`n", [Text.UTF8Encoding]::new($false))
            Add-AndCommit -Root $root -Path 'src/base.txt' -Message 'reflog base'
            Add-AndCommit -Root $root -Path 'ADRAI_1_Source/fixture.bin' -Message 'reflog-only protected path' -Force
            [void] (Invoke-TestGit -Root $root -Arguments @('reset', '--mixed', '--quiet', 'HEAD^'))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'protected path exists in reachable history'
        }
    },
    [pscustomobject] @{
        Name = 'standalone plan path is rejected explicitly'
        Action = {
            $root = New-TestRoot -Name 'standalone-plan-path' -InitializeGit
            [IO.File]::WriteAllText((Join-Path -Path $root -ChildPath 'ADRAI_1_Search_Enhanced_Haskell_Reimplementation_Plan.md'), "dummy standalone plan fixture`n", [Text.UTF8Encoding]::new($false))
            [void] (Invoke-TestGit -Root $root -Arguments @('add', '-f', '--', 'ADRAI_1_Search_Enhanced_Haskell_Reimplementation_Plan.md'))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'ADRAI_1_Search_Enhanced_Haskell_Reimplementation_Plan.md'
        }
    },
    [pscustomobject] @{
        Name = 'hostile GIT_INDEX_FILE bypass attempt is rejected'
        Action = {
            $root = New-TestRoot -Name 'hostile-index-environment' -InitializeGit
            [void] (Invoke-TestGit -Root $root -Arguments @('add', '-f', '--', 'ADRAI_1_Source/fixture.bin'))
            $previousIndexFile = [Environment]::GetEnvironmentVariable('GIT_INDEX_FILE', [EnvironmentVariableTarget]::Process)
            [Environment]::SetEnvironmentVariable('GIT_INDEX_FILE', (Join-Path -Path $root -ChildPath 'alternate-clean.index'), [EnvironmentVariableTarget]::Process)
            try {
                $result = Invoke-GuardProcess -Root $root
            }
            finally {
                [Environment]::SetEnvironmentVariable('GIT_INDEX_FILE', $previousIndexFile, [EnvironmentVariableTarget]::Process)
            }
            Assert-GuardResult -Result $result -ShouldPass $false -ExpectedText 'GIT_INDEX_FILE'
        }
    },
    [pscustomobject] @{
        Name = 'live prototype manifest mismatch is rejected'
        Action = {
            $root = New-TestRoot -Name 'manifest-mismatch' -InitializeGit
            [IO.File]::WriteAllBytes((Join-Path -Path $root -ChildPath 'ADRAI_1_Source/fixture.bin'), [Text.Encoding]::UTF8.GetBytes("changed dummy fixture bytes`n"))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'do not match immutable manifest'
        }
    },
    [pscustomobject] @{
        Name = 'malformed blob manifest is rejected'
        Action = {
            $root = New-TestRoot -Name 'malformed-manifest' -InitializeGit
            [IO.File]::WriteAllText((Join-Path -Path $root -ChildPath 'tools/ProtectedSourceBlobIds.txt'), "object-format=sha1`nfile-count=1`nnot-a-blob-id`n", [Text.UTF8Encoding]::new($false))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'malformed sha1 blob ID'
        }
    },
    [pscustomobject] @{
        Name = 'missing anchored denylist rule is rejected'
        Action = {
            $root = New-TestRoot -Name 'missing-rule'
            [IO.File]::WriteAllText((Join-Path -Path $root -ChildPath '.gitignore'), "/ADRAI_1_Source/`n", [Text.UTF8Encoding]::new($false))
            Assert-GuardResult -Result (Invoke-GuardProcess -Root $root) -ShouldPass $false -ExpectedText 'must contain exactly one literal'
        }
    }
)

$failures = [Collections.Generic.List[string]]::new()
try {
    [void] [IO.Directory]::CreateDirectory($testParent)
    foreach ($test in $tests) {
        try {
            & $test.Action
            Write-Output "[PASS] $($test.Name)"
        }
        catch {
            $failures.Add("$($test.Name): $($_.Exception.Message)")
            Write-Output "[FAIL] $($test.Name)"
        }
    }
}
finally {
    $resolvedTestParent = [IO.Path]::GetFullPath($testParent)
    $expectedPrefix = $tempBase + [IO.Path]::DirectorySeparatorChar + 'adrai-source-guard-tests-'
    if ($resolvedTestParent.StartsWith($expectedPrefix, [StringComparison]::OrdinalIgnoreCase) -and (Test-Path -LiteralPath $resolvedTestParent)) {
        Remove-Item -LiteralPath $resolvedTestParent -Recurse -Force
    }
}

if ($failures.Count -gt 0) {
    throw "Source exclusion guard tests failed:$([Environment]::NewLine)- $($failures -join "$([Environment]::NewLine)- ")"
}

Write-Output "All $($tests.Count) source exclusion guard tests passed."
