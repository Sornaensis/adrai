function Invoke-LinuxSelfCheck {
    # These are control-plane fixtures. Product executables still receive their
    # real argv without a shell. All cases share the runner's containment clock.
    $shell = Resolve-AbsoluteFile -Path '/bin/sh' -Label 'SelfCheck shell' -Executable
    $environment = Get-ProcessEnvironment $null $false
    $success = Invoke-OwnedProcess -Id 'selfcheck-success-argv' -Executable $shell -Arguments @(
        '-c', 'printf "%s\n" "$1"', 'fixture', 'space " quote λ'
    ) -WorkingDirectory $script:EvidenceDirectory -Environment $environment
    Assert-SuccessfulInvocation $success
    if ([IO.File]::ReadAllText($success.stdout).TrimEnd("`n") -cne 'space " quote λ') { throw 'Linux exact argv self-check failed.' }
    $nonzero = Invoke-OwnedProcess -Id 'selfcheck-nonzero' -Executable $shell -Arguments @('-c','exit 41') `
        -WorkingDirectory $script:EvidenceDirectory -Environment $environment
    if ($nonzero.exitCode -ne 41 -or -not $nonzero.cleanupVerified) { throw 'Linux nonzero exit/cleanup self-check failed.' }
    $missing = Join-Path $script:EvidenceDirectory 'missing-native-payload'
    $failure = $null
    try {
        [void](Invoke-OwnedProcess -Id 'selfcheck-preexec-failure' -Executable $missing -Arguments @() `
            -WorkingDirectory $script:EvidenceDirectory -Environment $environment)
    } catch [Adrai.RetainedTests.OwnedLaunchException] { $failure = $_.Exception }
    if ($null -eq $failure -or -not $failure.ProcessCleanupVerified -or $failure.NativeErrorCode -ne 2) {
        throw 'Linux pre-exec failure/verified namespace cleanup self-check failed.'
    }
    # A detached descendant stays alive and holds payload stdout after its root
    # exits. Its namespace is authority; there is no PID/membership enumeration.
    $token = [Guid]::NewGuid().ToString('N')
    $readyPath = Join-Path $script:EvidenceDirectory "detached-$token-ready"
    $releasePath = Join-Path $script:EvidenceDirectory "detached-$token-release"
    $owner = [Adrai.RetainedTests.OwnedJob]::new()
    $payload = $null
    $pollSignal = [Threading.ManualResetEvent]::new($false)
    try {
        $payload = $owner.Launch($shell,@('-c',
            'mkfifo "$2"; setsid /bin/sh -c ''printf ready > "$1.tmp" && mv "$1.tmp" "$1"; read reply < "$2"'' fixture "$1" "$2" & exit 0',
            'fixture',$readyPath,$releasePath),$script:EvidenceDirectory,$null,
            (Join-Path $script:EvidenceDirectory 'detached.stdout'),(Join-Path $script:EvidenceDirectory 'detached.stderr'),
            [int][Math]::Max(0,(Get-RemainingMilliseconds)-$CleanupReserveSeconds*1000))
        while (-not (Test-Path -LiteralPath $readyPath -PathType Leaf)) {
            if ($owner.ActiveProcessCount() -eq 0) { throw 'Detached fixture ended before readiness.' }
            if ((Get-RemainingMilliseconds)-$CleanupReserveSeconds*1000 -le 0) { throw 'Shared SelfCheck containment expired before readiness.' }
            [void]$pollSignal.WaitOne(10)
        }
        if ([IO.File]::ReadAllText($readyPath) -cne 'ready') { throw 'Detached fixture readiness frame was invalid.' }
        if (-not $payload.WaitForExit([int][Math]::Max(0,(Get-RemainingMilliseconds)-$CleanupReserveSeconds*1000)) -or
            $payload.GetExitCode() -ne 0 -or $owner.ActiveProcessCount() -eq 0) {
            throw 'Root exit with surviving detached namespace was not observed.'
        }
        $owner.Terminate(125)
        if (-not $owner.WaitForEmpty([int][Math]::Max(0,(Get-RemainingMilliseconds)))) { throw 'Detached namespace cleanup was not verified.' }
    } finally {
        $owner.Terminate(125)
        $joined=$owner.WaitForEmpty([int][Math]::Max(0,(Get-RemainingMilliseconds)))
        if($payload){$payload.Dispose()};$owner.Dispose();$pollSignal.Dispose()
        if(-not$joined){throw 'Linux SelfCheck owned namespace could not be joined.'}
    }
    $script:Evidence.linuxOwnershipSelfCheck = 'argv, nonzero, pre-exec errno, namespace cancellation with detached held-output child'
    $script:Evidence.linuxNamespaceCleanupVerified = $true
}
