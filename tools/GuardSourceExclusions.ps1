[CmdletBinding()]
param(
    [Parameter()]
    [string] $RepositoryRoot
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

if ([string]::IsNullOrWhiteSpace($RepositoryRoot)) {
    $RepositoryRoot = Split-Path -Parent $PSScriptRoot
}

$protectedSourceDirectory = 'ADRAI_1_Source'
$protectedPlanPath = 'ADRAI_1_Search_Enhanced_Haskell_Reimplementation_Plan.md'
$protectedBlobManifestPath = 'tools/ProtectedSourceBlobIds.txt'
$requiredIgnoreRules = @(
    '/ADRAI_1_Source/'
    '/ADRAI_1_Search_Enhanced_Haskell_Reimplementation_Plan.md'
)
$forbiddenGitEnvironmentVariables = @(
    'GIT_ALTERNATE_OBJECT_DIRECTORIES'
    'GIT_CEILING_DIRECTORIES'
    'GIT_COMMON_DIR'
    'GIT_DEFAULT_HASH'
    'GIT_DIR'
    'GIT_DISCOVERY_ACROSS_FILESYSTEM'
    'GIT_GRAFT_FILE'
    'GIT_INDEX_FILE'
    'GIT_NAMESPACE'
    'GIT_NO_REPLACE_OBJECTS'
    'GIT_OBJECT_DIRECTORY'
    'GIT_OBJECT_FORMAT'
    'GIT_QUARANTINE_PATH'
    'GIT_REPLACE_REF_BASE'
    'GIT_SHALLOW_FILE'
    'GIT_WORK_TREE'
)

function Invoke-GitReadOnly {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root,

        [Parameter(Mandatory = $true)]
        [string[]] $Arguments
    )

    $boundArguments = @('--no-replace-objects', '-C', $Root) + $Arguments
    $previousErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    try {
        $output = @(& git @boundArguments 2>&1)
        $exitCode = $LASTEXITCODE
    }
    finally {
        $ErrorActionPreference = $previousErrorActionPreference
    }
    if ($exitCode -ne 0) {
        $details = ($output | ForEach-Object { $_.ToString() }) -join [Environment]::NewLine
        throw "Git command failed with exit code $exitCode (git $($boundArguments -join ' ')). $details"
    }

    return $output
}

function Assert-CleanGitEnvironment {
    $nonemptyVariables = [Collections.Generic.List[string]]::new()
    foreach ($name in $forbiddenGitEnvironmentVariables) {
        $value = [Environment]::GetEnvironmentVariable($name, [EnvironmentVariableTarget]::Process)
        if ($null -ne $value -and $value.Length -gt 0) {
            $nonemptyVariables.Add($name)
        }
    }

    if ($nonemptyVariables.Count -gt 0) {
        throw "Forbidden nonempty Git environment variable(s): $($nonemptyVariables -join ', ')."
    }
}

function Test-ProtectedPath {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Path
    )

    return (
        $Path.Equals($protectedSourceDirectory, [StringComparison]::Ordinal) -or
        $Path.StartsWith("$protectedSourceDirectory/", [StringComparison]::Ordinal) -or
        $Path.Equals($protectedPlanPath, [StringComparison]::Ordinal)
    )
}

function Get-OrdinalSortedUnique {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyCollection()]
        [string[]] $Values
    )

    $set = [Collections.Generic.HashSet[string]]::new([StringComparer]::Ordinal)
    foreach ($value in $Values) {
        [void] $set.Add($value)
    }

    $result = [string[]] @($set)
    [Array]::Sort($result, [StringComparer]::Ordinal)
    return $result
}

function Assert-IgnoreRules {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root
    )

    $ignorePath = Join-Path -Path $Root -ChildPath '.gitignore'
    if (-not (Test-Path -LiteralPath $ignorePath -PathType Leaf)) {
        throw "Required root .gitignore is missing: $ignorePath"
    }

    $ignoreLines = [IO.File]::ReadAllLines($ignorePath)
    foreach ($rule in $requiredIgnoreRules) {
        $occurrences = @($ignoreLines | Where-Object { $_ -ceq $rule }).Count
        if ($occurrences -ne 1) {
            throw "Root .gitignore must contain exactly one literal '$rule' rule; found $occurrences."
        }
    }
}

function Read-ProtectedBlobManifest {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root
    )

    $manifestPath = Join-Path -Path $Root -ChildPath $protectedBlobManifestPath
    if (-not (Test-Path -LiteralPath $manifestPath -PathType Leaf)) {
        throw "Protected source blob manifest is missing: $manifestPath"
    }

    $lines = [IO.File]::ReadAllLines($manifestPath)
    if ($lines.Count -lt 3 -or $lines[0] -cnotmatch '^object-format=(sha1|sha256)$') {
        throw "Protected source blob manifest must begin with exactly 'object-format=sha1' or 'object-format=sha256'."
    }

    $objectFormat = $Matches[1]
    if ($lines[1] -cnotmatch '^file-count=([1-9][0-9]*)$') {
        throw "Protected source blob manifest must contain a positive integer 'file-count=' on line 2."
    }
    $fileCount = [int]::Parse($Matches[1], [Globalization.CultureInfo]::InvariantCulture)
    $objectIdLength = if ($objectFormat -ceq 'sha1') { 40 } else { 64 }
    $objectIdPattern = "^[0-9a-f]{$objectIdLength}$"
    $blobIds = [string[]] @($lines | Select-Object -Skip 2)
    foreach ($blobId in $blobIds) {
        if ($blobId -cnotmatch $objectIdPattern) {
            throw "Protected source blob manifest contains a malformed $objectFormat blob ID: '$blobId'."
        }
    }

    $sortedBlobIds = [string[]] @($blobIds)
    [Array]::Sort($sortedBlobIds, [StringComparer]::Ordinal)
    for ($index = 0; $index -lt $blobIds.Count; $index++) {
        if ($blobIds[$index] -cne $sortedBlobIds[$index]) {
            throw 'Protected source blob manifest IDs must be sorted in ordinal order.'
        }
        if ($index -gt 0 -and $blobIds[$index] -ceq $blobIds[$index - 1]) {
            throw "Protected source blob manifest contains duplicate blob ID '$($blobIds[$index])'."
        }
    }
    if ($fileCount -lt $blobIds.Count) {
        throw "Protected source blob manifest file count $fileCount cannot be smaller than its $($blobIds.Count) unique blob IDs."
    }

    $blobIdSet = [Collections.Generic.HashSet[string]]::new([StringComparer]::Ordinal)
    foreach ($blobId in $blobIds) {
        [void] $blobIdSet.Add($blobId)
    }

    return [pscustomobject] @{
        ObjectFormat = $objectFormat
        FileCount = $fileCount
        BlobIds = $blobIds
        BlobIdSet = $blobIdSet
    }
}

function Get-LivePrototypeState {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root,

        [Parameter(Mandatory = $true)]
        [ValidateSet('sha1', 'sha256')]
        [string] $ObjectFormat
    )

    $prototypeRoot = Join-Path -Path $Root -ChildPath $protectedSourceDirectory
    $prototypeFiles = [string[]] @(
        Get-ChildItem -LiteralPath $prototypeRoot -Recurse -Force -File |
            ForEach-Object { $_.FullName }
    )
    [Array]::Sort($prototypeFiles, [StringComparer]::Ordinal)

    $objectIdLength = if ($ObjectFormat -ceq 'sha1') { 40 } else { 64 }
    $objectIdPattern = "^[0-9a-f]{$objectIdLength}$"
    $blobIdSet = [Collections.Generic.HashSet[string]]::new([StringComparer]::Ordinal)
    foreach ($file in $prototypeFiles) {
        $relativePath = $file.Substring($Root.Length).TrimStart([IO.Path]::DirectorySeparatorChar, [IO.Path]::AltDirectorySeparatorChar)
        $relativeGitPath = $relativePath.Replace([IO.Path]::DirectorySeparatorChar, '/')
        $hashOutput = @(Invoke-GitReadOnly -Root $Root -Arguments @(
            'hash-object', '--no-filters', '--', $relativeGitPath
        ))
        if ($hashOutput.Count -ne 1) {
            throw "git hash-object returned an unexpected result for literal path '$relativeGitPath'."
        }

        $blobId = $hashOutput[0].ToString().Trim()
        if ($blobId -cnotmatch $objectIdPattern) {
            throw "git hash-object returned an invalid object ID for literal path '$relativeGitPath': $blobId"
        }
        [void] $blobIdSet.Add($blobId)
    }

    $blobIds = [string[]] @($blobIdSet)
    [Array]::Sort($blobIds, [StringComparer]::Ordinal)
    return [pscustomobject] @{
        FileCount = $prototypeFiles.Count
        BlobIds = $blobIds
    }
}

function Assert-LivePrototypeMatchesManifest {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root,

        [Parameter(Mandatory = $true)]
        [pscustomobject] $Manifest
    )

    $prototypeRoot = Join-Path -Path $Root -ChildPath $protectedSourceDirectory
    if (-not (Test-Path -LiteralPath $prototypeRoot)) {
        return
    }
    if (-not (Test-Path -LiteralPath $prototypeRoot -PathType Container)) {
        throw "Protected prototype path exists but is not a directory: $prototypeRoot"
    }

    $liveState = Get-LivePrototypeState -Root $Root -ObjectFormat $Manifest.ObjectFormat
    if ($liveState.FileCount -ne $Manifest.FileCount) {
        throw "Live prototype file count $($liveState.FileCount) does not match immutable manifest count $($Manifest.FileCount)."
    }
    if ($liveState.BlobIds.Count -ne $Manifest.BlobIds.Count) {
        throw "Live prototype blob IDs do not match immutable manifest (live unique count $($liveState.BlobIds.Count), manifest count $($Manifest.BlobIds.Count))."
    }
    for ($index = 0; $index -lt $liveState.BlobIds.Count; $index++) {
        if ($liveState.BlobIds[$index] -cne $Manifest.BlobIds[$index]) {
            throw "Live prototype blob IDs do not match immutable manifest at ordinal index $index."
        }
    }
}

function Get-IndexEntries {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root
    )

    $lines = @(Invoke-GitReadOnly -Root $Root -Arguments @(
        '-c', 'core.quotePath=false',
        'ls-files', '--stage'
    ))
    $entries = [Collections.Generic.List[object]]::new()
    foreach ($lineValue in $lines) {
        $line = $lineValue.ToString()
        if ($line.Length -eq 0) {
            continue
        }
        if ($line -notmatch '^[0-7]{6} ([0-9a-fA-F]{40}(?:[0-9a-fA-F]{24})?) ([0-3])\t(.*)$') {
            throw "Unable to parse git ls-files --stage output; refusing to continue: $line"
        }
        $entries.Add([pscustomobject] @{
            BlobId = $Matches[1]
            Stage = $Matches[2]
            Path = $Matches[3]
        })
    }

    return $entries
}

function Get-ReachableObjects {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root
    )

    $lines = @(Invoke-GitReadOnly -Root $Root -Arguments @(
        '-c', 'core.quotePath=false',
        'rev-list', '--objects', '--all', '--reflog'
    ))
    $objects = [Collections.Generic.List[object]]::new()
    foreach ($lineValue in $lines) {
        $line = $lineValue.ToString()
        if ($line.Length -eq 0) {
            continue
        }
        if ($line -notmatch '^([0-9a-fA-F]{40}(?:[0-9a-fA-F]{24})?)(?: (.*))?$') {
            throw "Unable to parse git rev-list --objects --all output; refusing to continue: $line"
        }
        $objects.Add([pscustomobject] @{
            ObjectId = $Matches[1]
            Path = if ($Matches.Count -gt 2) { $Matches[2] } else { '' }
        })
    }

    return $objects
}

function Get-ReachableChangedPaths {
    param(
        [Parameter(Mandatory = $true)]
        [string] $Root
    )

    $lines = @(Invoke-GitReadOnly -Root $Root -Arguments @(
        '-c', 'core.quotePath=false',
        'log', '--all', '--reflog', '--root', '-m', '--format=', '--name-only', '--no-renames', '--'
    ))
    return [string[]] @(
        $lines |
            ForEach-Object { $_.ToString() } |
            Where-Object { $_.Length -gt 0 }
    )
}

function Invoke-SourceExclusionGuard {
    $resolvedRoot = (Resolve-Path -LiteralPath $RepositoryRoot).Path
    if (-not (Test-Path -LiteralPath $resolvedRoot -PathType Container)) {
        throw "Repository root is not a directory: $resolvedRoot"
    }
    $resolvedRoot = $resolvedRoot.TrimEnd([IO.Path]::DirectorySeparatorChar, [IO.Path]::AltDirectorySeparatorChar)

    Assert-CleanGitEnvironment
    Assert-IgnoreRules -Root $resolvedRoot
    $manifest = Read-ProtectedBlobManifest -Root $resolvedRoot
    Assert-LivePrototypeMatchesManifest -Root $resolvedRoot -Manifest $manifest

    $gitMarker = Join-Path -Path $resolvedRoot -ChildPath '.git'
    if (-not (Test-Path -LiteralPath $gitMarker)) {
        Write-Output "Source exclusion guard passed: denylist and immutable blob manifest verified for pre-Git-init root '$resolvedRoot'."
        return
    }

    $insideWorkTree = @(Invoke-GitReadOnly -Root $resolvedRoot -Arguments @(
        'rev-parse', '--is-inside-work-tree'
    ))
    if ($insideWorkTree.Count -ne 1 -or $insideWorkTree[0].ToString().Trim() -cne 'true') {
        throw "Root contains .git but Git did not identify it as a work tree: $resolvedRoot"
    }

    $topLevelOutput = @(Invoke-GitReadOnly -Root $resolvedRoot -Arguments @(
        'rev-parse', '--show-toplevel'
    ))
    if ($topLevelOutput.Count -ne 1) {
        throw 'git rev-parse --show-toplevel returned an unexpected result.'
    }
    $gitTopLevel = [IO.Path]::GetFullPath($topLevelOutput[0].ToString().Trim()).TrimEnd([IO.Path]::DirectorySeparatorChar, [IO.Path]::AltDirectorySeparatorChar)
    $isWindowsVariable = Get-Variable -Name IsWindows -ErrorAction SilentlyContinue
    $runningOnWindows = if ($null -eq $isWindowsVariable) {
        $env:OS -ceq 'Windows_NT'
    }
    else {
        [bool] $isWindowsVariable.Value
    }
    $pathComparison = if ($runningOnWindows) {
        [StringComparison]::OrdinalIgnoreCase
    }
    else {
        [StringComparison]::Ordinal
    }
    if (-not $gitTopLevel.Equals($resolvedRoot, $pathComparison)) {
        throw "Guard root '$resolvedRoot' is not the Git top level '$gitTopLevel'."
    }

    $objectFormatOutput = @(Invoke-GitReadOnly -Root $resolvedRoot -Arguments @(
        'rev-parse', '--show-object-format=storage'
    ))
    if ($objectFormatOutput.Count -ne 1) {
        throw 'git rev-parse --show-object-format=storage returned an unexpected result.'
    }
    $repositoryObjectFormat = $objectFormatOutput[0].ToString().Trim()
    if ($repositoryObjectFormat -cne $manifest.ObjectFormat) {
        throw "Repository object format '$repositoryObjectFormat' does not match protected manifest expectation '$($manifest.ObjectFormat)'."
    }

    $prototypeBlobIds = $manifest.BlobIdSet
    $indexEntries = Get-IndexEntries -Root $resolvedRoot
    $reachableObjects = Get-ReachableObjects -Root $resolvedRoot
    $reachableChangedPaths = Get-ReachableChangedPaths -Root $resolvedRoot

    $protectedIndexPaths = [Collections.Generic.List[string]]::new()
    $protectedIndexBlobIds = [Collections.Generic.List[string]]::new()
    foreach ($entry in $indexEntries) {
        if (Test-ProtectedPath -Path $entry.Path) {
            $protectedIndexPaths.Add($entry.Path)
        }
        if ($prototypeBlobIds.Contains($entry.BlobId)) {
            $protectedIndexBlobIds.Add("$($entry.BlobId) $($entry.Path)")
        }
    }

    $protectedHistoryPaths = [Collections.Generic.List[string]]::new()
    foreach ($path in $reachableChangedPaths) {
        if (Test-ProtectedPath -Path $path) {
            $protectedHistoryPaths.Add($path)
        }
    }
    foreach ($object in $reachableObjects) {
        if ($object.Path.Length -gt 0 -and (Test-ProtectedPath -Path $object.Path)) {
            $protectedHistoryPaths.Add($object.Path)
        }
    }

    $protectedHistoryBlobIds = [Collections.Generic.List[string]]::new()
    foreach ($object in $reachableObjects) {
        if ($prototypeBlobIds.Contains($object.ObjectId)) {
            $displayPath = if ($object.Path.Length -gt 0) { $object.Path } else { '<no path emitted>' }
            $protectedHistoryBlobIds.Add("$($object.ObjectId) $displayPath")
        }
    }

    $violations = [Collections.Generic.List[string]]::new()
    $indexPaths = @(Get-OrdinalSortedUnique -Values $protectedIndexPaths.ToArray())
    if ($indexPaths.Count -gt 0) {
        $violations.Add("protected path is staged/tracked: $($indexPaths -join ', ')")
    }
    $historyPaths = @(Get-OrdinalSortedUnique -Values $protectedHistoryPaths.ToArray())
    if ($historyPaths.Count -gt 0) {
        $violations.Add("protected path exists in reachable history: $($historyPaths -join ', ')")
    }
    $indexBlobs = @(Get-OrdinalSortedUnique -Values $protectedIndexBlobIds.ToArray())
    if ($indexBlobs.Count -gt 0) {
        $violations.Add("protected prototype blob is staged/tracked: $($indexBlobs -join ', ')")
    }
    $historyBlobs = @(Get-OrdinalSortedUnique -Values $protectedHistoryBlobIds.ToArray())
    if ($historyBlobs.Count -gt 0) {
        $violations.Add("protected prototype blob exists in reachable history: $($historyBlobs -join ', ')")
    }

    if ($violations.Count -gt 0) {
        throw "Source exclusion violations detected:$([Environment]::NewLine)- $($violations -join "$([Environment]::NewLine)- ")"
    }

    Write-Output "Source exclusion guard passed: denylist, immutable blob manifest, index, refs, and reflogs verified for '$resolvedRoot'."
}

try {
    Invoke-SourceExclusionGuard
}
catch {
    $singleLineMessage = $_.Exception.Message -replace '[\r\n]+', ' | '
    [Console]::Error.WriteLine("Source exclusion guard failed: {0}", $singleLineMessage)
    exit 1
}
