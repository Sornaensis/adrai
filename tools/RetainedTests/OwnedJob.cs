using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace Adrai.RetainedTests
{
    public sealed class OwnedLaunchException : Exception
    {
        internal OwnedLaunchException(string message, int nativeErrorCode, bool processCleanupVerified, Exception innerException)
            : base(message, innerException)
        {
            NativeErrorCode = nativeErrorCode;
            ProcessCleanupVerified = processCleanupVerified;
        }

        public int NativeErrorCode { get; private set; }
        public bool ProcessCleanupVerified { get; private set; }
    }

    public sealed class OwnedProcess : IDisposable
    {
        private IntPtr processHandle;

        internal OwnedProcess(IntPtr processHandle, int processId)
        {
            this.processHandle = processHandle;
            ProcessId = processId;
        }

        public int ProcessId { get; private set; }

        public bool WaitForExit(int milliseconds)
        {
            EnsureOpen();
            uint result = NativeMethods.WaitForSingleObject(processHandle, checked((uint)milliseconds));
            if (result == NativeMethods.WaitObject0) return true;
            if (result == NativeMethods.WaitTimeout) return false;
            throw new Win32Exception(Marshal.GetLastWin32Error(), "WaitForSingleObject(process) failed.");
        }

        public int GetExitCode()
        {
            EnsureOpen();
            uint exitCode;
            if (!NativeMethods.GetExitCodeProcess(processHandle, out exitCode))
                throw new Win32Exception(Marshal.GetLastWin32Error(), "GetExitCodeProcess failed.");
            if (exitCode == NativeMethods.StillActive)
                throw new InvalidOperationException("The owned process has not exited.");
            return unchecked((int)exitCode);
        }

        private void EnsureOpen()
        {
            if (processHandle == IntPtr.Zero)
                throw new ObjectDisposedException("OwnedProcess");
        }

        public void Dispose()
        {
            if (processHandle != IntPtr.Zero)
            {
                NativeMethods.CloseHandle(processHandle);
                processHandle = IntPtr.Zero;
            }
        }
    }

    public sealed class OwnedJobFailureSnapshot
    {
        internal OwnedJobFailureSnapshot()
        {
            processes = new OwnedJobProcessSnapshot[0];
        }

        public uint assignedProcessCount { get; internal set; }
        public uint returnedProcessIdCount { get; internal set; }
        public bool complete { get; internal set; }
        public bool truncated { get; internal set; }
        public bool partialEvidence { get; internal set; }
        public string queryError { get; internal set; }
        public OwnedJobProcessSnapshot[] processes { get; internal set; }
    }

    public sealed class OwnedJobProcessSnapshot
    {
        public ulong processId { get; internal set; }
        public string identityState { get; internal set; }
        public bool? confirmedJobMembership { get; internal set; }
        public string membershipError { get; internal set; }
        public string openError { get; internal set; }
        public string imagePath { get; internal set; }
        public string imageName { get; internal set; }
        public string imageQueryError { get; internal set; }
        public long? creationTimeFileTime { get; internal set; }
        public string creationTimeUtc { get; internal set; }
        public string creationTimeQueryError { get; internal set; }
        public string zeroWaitState { get; internal set; }
        public int? exitCode { get; internal set; }
        public string waitQueryError { get; internal set; }
        public string exitCodeQueryError { get; internal set; }
        public bool partialEvidence { get; internal set; }
    }

    public sealed class OwnedJob : IDisposable
    {
        private IntPtr jobHandle;

        public OwnedJob()
        {
            jobHandle = NativeMethods.CreateJobObject(IntPtr.Zero, null);
            if (jobHandle == IntPtr.Zero)
                throw new Win32Exception(Marshal.GetLastWin32Error(), "CreateJobObject failed.");

            var limits = new NativeMethods.JobObjectExtendedLimitInformation();
            // Do not enable BREAKAWAY_OK or SILENT_BREAKAWAY_OK. Descendants
            // therefore remain in this job unless Windows rejects assignment.
            limits.BasicLimitInformation.LimitFlags = NativeMethods.JobObjectLimitKillOnJobClose;
            int size = Marshal.SizeOf(typeof(NativeMethods.JobObjectExtendedLimitInformation));
            IntPtr buffer = Marshal.AllocHGlobal(size);
            try
            {
                Marshal.StructureToPtr(limits, buffer, false);
                if (!NativeMethods.SetInformationJobObject(
                        jobHandle,
                        NativeMethods.JobObjectInfoType.ExtendedLimitInformation,
                        buffer,
                        (uint)size))
                    throw new Win32Exception(Marshal.GetLastWin32Error(), "SetInformationJobObject failed.");
            }
            catch
            {
                NativeMethods.CloseHandle(jobHandle);
                jobHandle = IntPtr.Zero;
                throw;
            }
            finally
            {
                Marshal.FreeHGlobal(buffer);
            }
        }

        public OwnedProcess Launch(
            string executable,
            string[] arguments,
            string workingDirectory,
            IDictionary environmentOverrides,
            string stdoutPath,
            string stderrPath,
            int failureCleanupMilliseconds)
        {
            EnsureOpen();
            if (string.IsNullOrWhiteSpace(executable) || !Path.IsPathRooted(executable))
                throw new ArgumentException("Executable must be an absolute path.", "executable");
            if (string.IsNullOrWhiteSpace(workingDirectory) || !Path.IsPathRooted(workingDirectory))
                throw new ArgumentException("Working directory must be an absolute path.", "workingDirectory");
            if (failureCleanupMilliseconds < 0)
                throw new ArgumentOutOfRangeException("failureCleanupMilliseconds");

            IntPtr stdoutHandle = IntPtr.Zero;
            IntPtr stderrHandle = IntPtr.Zero;
            IntPtr stdinHandle = IntPtr.Zero;
            IntPtr environmentBlock = IntPtr.Zero;
            NativeMethods.ProcessInformation process = new NativeMethods.ProcessInformation();
            bool processCreated = false;
            bool cleanupAttempted = false;
            try
            {
                stdoutHandle = OpenOutput(stdoutPath);
                stderrHandle = OpenOutput(stderrPath);
                stdinHandle = OpenInputNull();

                var startup = new NativeMethods.StartupInfo();
                startup.cb = Marshal.SizeOf(typeof(NativeMethods.StartupInfo));
                startup.dwFlags = NativeMethods.StartfUseStdHandles | NativeMethods.StartfUseShowWindow;
                startup.wShowWindow = NativeMethods.SwHide;
                startup.hStdInput = stdinHandle;
                startup.hStdOutput = stdoutHandle;
                startup.hStdError = stderrHandle;

                string commandLine = BuildCommandLine(executable, arguments ?? new string[0]);
                environmentBlock = BuildEnvironmentBlock(environmentOverrides);
                bool created = NativeMethods.CreateProcess(
                    executable,
                    new StringBuilder(commandLine),
                    IntPtr.Zero,
                    IntPtr.Zero,
                    true,
                    NativeMethods.CreateSuspended | NativeMethods.CreateUnicodeEnvironment | NativeMethods.CreateNoWindow,
                    environmentBlock,
                    workingDirectory,
                    ref startup,
                    out process);
                if (!created)
                    throw new Win32Exception(Marshal.GetLastWin32Error(), "CreateProcessW failed for " + executable + ".");
                processCreated = true;

                // The primary thread is still suspended here, so no child can escape
                // before the root process is assigned to the non-breakaway Job Object.
                if (!NativeMethods.AssignProcessToJobObject(jobHandle, process.hProcess))
                {
                    int error = Marshal.GetLastWin32Error();
                    cleanupAttempted = true;
                    try
                    {
                        TerminateAndVerifyProcess(process.hProcess, 125, failureCleanupMilliseconds);
                        processCreated = false;
                        throw new OwnedLaunchException(
                            "AssignProcessToJobObject failed; cleanup of the unassigned suspended process was verified.",
                            error,
                            true,
                            new Win32Exception(error));
                    }
                    catch (OwnedLaunchException)
                    {
                        throw;
                    }
                    catch (Exception cleanupFailure)
                    {
                        throw new OwnedLaunchException(
                            "AssignProcessToJobObject failed and cleanup of the unassigned suspended process could not be verified.",
                            error,
                            false,
                            cleanupFailure);
                    }
                }

                uint resumeResult = NativeMethods.ResumeThread(process.hThread);
                if (resumeResult == UInt32.MaxValue)
                {
                    int error = Marshal.GetLastWin32Error();
                    cleanupAttempted = true;
                    if (!NativeMethods.TerminateJobObject(jobHandle, 125))
                        throw new Win32Exception(Marshal.GetLastWin32Error(), "ResumeThread failed and TerminateJobObject also failed.");
                    VerifyExitedProcess(process.hProcess, failureCleanupMilliseconds);
                    processCreated = false;
                    throw new Win32Exception(error, "ResumeThread failed; the owned Job Object was terminated.");
                }

                NativeMethods.CloseHandle(process.hThread);
                process.hThread = IntPtr.Zero;
                var owned = new OwnedProcess(process.hProcess, checked((int)process.dwProcessId));
                process.hProcess = IntPtr.Zero;
                return owned;
            }
            catch
            {
                if (processCreated && !cleanupAttempted && process.hProcess != IntPtr.Zero)
                {
                    TerminateAndVerifyProcess(process.hProcess, 125, failureCleanupMilliseconds);
                }
                throw;
            }
            finally
            {
                if (process.hThread != IntPtr.Zero) NativeMethods.CloseHandle(process.hThread);
                if (process.hProcess != IntPtr.Zero) NativeMethods.CloseHandle(process.hProcess);
                if (environmentBlock != IntPtr.Zero) Marshal.FreeHGlobal(environmentBlock);
                if (stdinHandle != IntPtr.Zero) NativeMethods.CloseHandle(stdinHandle);
                if (stderrHandle != IntPtr.Zero) NativeMethods.CloseHandle(stderrHandle);
                if (stdoutHandle != IntPtr.Zero) NativeMethods.CloseHandle(stdoutHandle);
            }
        }

        public uint ActiveProcessCount()
        {
            EnsureOpen();
            var accounting = new NativeMethods.JobObjectBasicAccountingInformation();
            int size = Marshal.SizeOf(typeof(NativeMethods.JobObjectBasicAccountingInformation));
            IntPtr buffer = Marshal.AllocHGlobal(size);
            try
            {
                uint returned;
                if (!NativeMethods.QueryInformationJobObject(
                        jobHandle,
                        NativeMethods.JobObjectInfoType.BasicAccountingInformation,
                        buffer,
                        (uint)size,
                        out returned))
                    throw new Win32Exception(Marshal.GetLastWin32Error(), "QueryInformationJobObject failed.");
                accounting = (NativeMethods.JobObjectBasicAccountingInformation)Marshal.PtrToStructure(
                    buffer,
                    typeof(NativeMethods.JobObjectBasicAccountingInformation));
                return accounting.ActiveProcesses;
            }
            finally
            {
                Marshal.FreeHGlobal(buffer);
            }
        }

        public OwnedJobFailureSnapshot CaptureFailureSnapshot()
        {
            EnsureOpen();
            const int initialEntryCapacity = 16;
            const int maximumEntryCapacity = 256;
            const int maximumQueryAttempts = 2;
            int entryCapacity = initialEntryCapacity;
            var snapshot = new OwnedJobFailureSnapshot();
            var processIds = new List<ulong>();

            for (int attempt = 0; attempt < maximumQueryAttempts; attempt++)
            {
                int size = checked(8 + entryCapacity * IntPtr.Size);
                IntPtr buffer = Marshal.AllocHGlobal(size);
                try
                {
                    for (int offset = 0; offset < size; offset++) Marshal.WriteByte(buffer, offset, 0);
                    uint returnedLength;
                    bool queried = NativeMethods.QueryInformationJobObject(
                        jobHandle,
                        NativeMethods.JobObjectInfoType.BasicProcessIdList,
                        buffer,
                        checked((uint)size),
                        out returnedLength);
                    int error = queried ? 0 : Marshal.GetLastWin32Error();
                    uint assigned = unchecked((uint)Marshal.ReadInt32(buffer, 0));
                    uint returned = unchecked((uint)Marshal.ReadInt32(buffer, 4));
                    snapshot.assignedProcessCount = assigned;
                    snapshot.returnedProcessIdCount = returned;

                    if (!queried && error == NativeMethods.ErrorMoreData && attempt + 1 < maximumQueryAttempts && entryCapacity < maximumEntryCapacity)
                    {
                        entryCapacity = (int)Math.Min(
                            maximumEntryCapacity,
                            Math.Max((long)entryCapacity * 2L, (long)assigned));
                        continue;
                    }

                    int readable = (int)Math.Min((long)returned, (long)entryCapacity);
                    for (int index = 0; index < readable; index++)
                    {
                        int offset = 8 + index * IntPtr.Size;
                        ulong processId = IntPtr.Size == 8
                            ? unchecked((ulong)Marshal.ReadInt64(buffer, offset))
                            : unchecked((uint)Marshal.ReadInt32(buffer, offset));
                        processIds.Add(processId);
                    }

                    snapshot.truncated = returned > (uint)entryCapacity || returned < assigned;
                    snapshot.complete = queried && !snapshot.truncated;
                    if (!queried) snapshot.queryError = NativeError("QueryInformationJobObject(JobObjectBasicProcessIdList) failed", error);
                    break;
                }
                finally
                {
                    Marshal.FreeHGlobal(buffer);
                }
            }

            var processes = new List<OwnedJobProcessSnapshot>();
            foreach (ulong processId in processIds)
                processes.Add(CaptureProcessSnapshot(processId));
            snapshot.processes = processes.ToArray();
            snapshot.partialEvidence = !snapshot.complete;
            foreach (OwnedJobProcessSnapshot process in snapshot.processes)
                snapshot.partialEvidence = snapshot.partialEvidence || process.partialEvidence;
            return snapshot;
        }

        public bool WaitForEmpty(int milliseconds)
        {
            EnsureOpen();
            var stopwatch = Stopwatch.StartNew();
            while (ActiveProcessCount() != 0)
            {
                int remaining = milliseconds - checked((int)Math.Min(stopwatch.ElapsedMilliseconds, Int32.MaxValue));
                if (remaining <= 0) return false;
                uint result = NativeMethods.WaitForSingleObject(jobHandle, (uint)Math.Min(remaining, 50));
                if (result != NativeMethods.WaitObject0 && result != NativeMethods.WaitTimeout)
                    throw new Win32Exception(Marshal.GetLastWin32Error(), "WaitForSingleObject(job) failed.");
            }
            return true;
        }

        public void Terminate(uint exitCode)
        {
            EnsureOpen();
            if (!NativeMethods.TerminateJobObject(jobHandle, exitCode))
                throw new Win32Exception(Marshal.GetLastWin32Error(), "TerminateJobObject failed.");
        }

        public void Dispose()
        {
            if (jobHandle != IntPtr.Zero)
            {
                NativeMethods.CloseHandle(jobHandle);
                jobHandle = IntPtr.Zero;
            }
        }

        private void EnsureOpen()
        {
            if (jobHandle == IntPtr.Zero)
                throw new ObjectDisposedException("OwnedJob");
        }

        private OwnedJobProcessSnapshot CaptureProcessSnapshot(ulong processId)
        {
            var snapshot = new OwnedJobProcessSnapshot();
            snapshot.processId = processId;
            snapshot.identityState = "unconfirmed";
            snapshot.zeroWaitState = "unqueried";
            if (processId > UInt32.MaxValue)
            {
                snapshot.identityState = "invalid-process-id";
                snapshot.openError = "Job Object returned a process ID outside the Win32 process-ID range.";
                snapshot.partialEvidence = true;
                return snapshot;
            }

            IntPtr process = NativeMethods.OpenProcess(
                NativeMethods.ProcessQueryLimitedInformation | NativeMethods.Synchronize,
                false,
                (uint)processId);
            if (process == IntPtr.Zero)
            {
                int error = Marshal.GetLastWin32Error();
                snapshot.identityState = error == NativeMethods.ErrorAccessDenied
                    ? "inaccessible"
                    : error == NativeMethods.ErrorInvalidParameter ? "disappeared" : "unavailable";
                snapshot.openError = NativeError("OpenProcess failed", error);
                snapshot.partialEvidence = true;
                return snapshot;
            }

            try
            {
                bool inJob;
                if (NativeMethods.IsProcessInJob(process, jobHandle, out inJob))
                {
                    snapshot.confirmedJobMembership = inJob;
                    snapshot.identityState = inJob ? "confirmed" : "not-in-job-or-reused";
                    if (!inJob) snapshot.partialEvidence = true;
                }
                else
                {
                    int error = Marshal.GetLastWin32Error();
                    snapshot.membershipError = NativeError("IsProcessInJob failed", error);
                    snapshot.identityState = "unconfirmed";
                    snapshot.partialEvidence = true;
                }

                var image = new StringBuilder(32768);
                uint imageLength = checked((uint)image.Capacity);
                if (NativeMethods.QueryFullProcessImageName(process, 0, image, ref imageLength))
                {
                    snapshot.imagePath = image.ToString(0, checked((int)imageLength));
                    snapshot.imageName = Path.GetFileName(snapshot.imagePath);
                }
                else
                {
                    snapshot.imageQueryError = NativeError("QueryFullProcessImageName failed", Marshal.GetLastWin32Error());
                    snapshot.partialEvidence = true;
                }

                NativeMethods.FileTime creation;
                NativeMethods.FileTime exit;
                NativeMethods.FileTime kernel;
                NativeMethods.FileTime user;
                if (NativeMethods.GetProcessTimes(process, out creation, out exit, out kernel, out user))
                {
                    long creationFileTime = ((long)creation.HighDateTime << 32) | creation.LowDateTime;
                    snapshot.creationTimeFileTime = creationFileTime;
                    snapshot.creationTimeUtc = DateTime.FromFileTimeUtc(creationFileTime).ToString("o");
                }
                else
                {
                    snapshot.creationTimeQueryError = NativeError("GetProcessTimes failed", Marshal.GetLastWin32Error());
                    snapshot.partialEvidence = true;
                }

                uint wait = NativeMethods.WaitForSingleObject(process, 0);
                if (wait == NativeMethods.WaitTimeout)
                {
                    snapshot.zeroWaitState = "running";
                }
                else if (wait == NativeMethods.WaitObject0)
                {
                    snapshot.zeroWaitState = "signaled";
                    uint exitCode;
                    if (NativeMethods.GetExitCodeProcess(process, out exitCode) && exitCode != NativeMethods.StillActive)
                        snapshot.exitCode = unchecked((int)exitCode);
                    else
                    {
                        int error = Marshal.GetLastWin32Error();
                        snapshot.exitCodeQueryError = exitCode == NativeMethods.StillActive
                            ? "The zero-wait state was signaled but the process exit code was STILL_ACTIVE."
                            : NativeError("GetExitCodeProcess failed", error);
                        snapshot.partialEvidence = true;
                    }
                }
                else
                {
                    snapshot.zeroWaitState = "query-error";
                    snapshot.waitQueryError = NativeError("WaitForSingleObject(process, 0) failed", Marshal.GetLastWin32Error());
                    snapshot.partialEvidence = true;
                }
                return snapshot;
            }
            finally
            {
                NativeMethods.CloseHandle(process);
            }
        }

        private static string NativeError(string operation, int error)
        {
            return operation + " (" + error + "): " + new Win32Exception(error).Message;
        }

        private static IntPtr OpenOutput(string path)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath(path)));
            var security = InheritableSecurityAttributes();
            IntPtr handle = NativeMethods.CreateFile(
                path,
                NativeMethods.GenericWrite,
                NativeMethods.FileShareRead | NativeMethods.FileShareWrite,
                ref security,
                NativeMethods.CreateAlways,
                NativeMethods.FileAttributeNormal,
                IntPtr.Zero);
            if (handle == NativeMethods.InvalidHandleValue)
                throw new Win32Exception(Marshal.GetLastWin32Error(), "Unable to open output file " + path + ".");
            return handle;
        }

        private static void TerminateAndVerifyProcess(IntPtr process, uint exitCode, int milliseconds)
        {
            if (!NativeMethods.TerminateProcess(process, exitCode))
                throw new Win32Exception(Marshal.GetLastWin32Error(), "TerminateProcess failed for a suspended launch.");
            VerifyExitedProcess(process, milliseconds);
        }

        private static void VerifyExitedProcess(IntPtr process, int milliseconds)
        {
            uint wait = NativeMethods.WaitForSingleObject(process, checked((uint)milliseconds));
            if (wait == NativeMethods.WaitTimeout)
                throw new TimeoutException("Suspended-process cleanup exceeded the caller's remaining cleanup budget.");
            if (wait != NativeMethods.WaitObject0)
                throw new Win32Exception(Marshal.GetLastWin32Error(), "Waiting for suspended-process cleanup failed.");
            uint exitCode;
            if (!NativeMethods.GetExitCodeProcess(process, out exitCode))
                throw new Win32Exception(Marshal.GetLastWin32Error(), "Reading suspended-process cleanup state failed.");
            if (exitCode == NativeMethods.StillActive)
                throw new InvalidOperationException("Suspended-process cleanup wait completed but the process is still active.");
        }

        private static IntPtr OpenInputNull()
        {
            var security = InheritableSecurityAttributes();
            IntPtr handle = NativeMethods.CreateFile(
                "NUL",
                NativeMethods.GenericRead,
                NativeMethods.FileShareRead | NativeMethods.FileShareWrite,
                ref security,
                NativeMethods.OpenExisting,
                NativeMethods.FileAttributeNormal,
                IntPtr.Zero);
            if (handle == NativeMethods.InvalidHandleValue)
                throw new Win32Exception(Marshal.GetLastWin32Error(), "Unable to open NUL for child stdin.");
            return handle;
        }

        private static NativeMethods.SecurityAttributes InheritableSecurityAttributes()
        {
            var security = new NativeMethods.SecurityAttributes();
            security.nLength = Marshal.SizeOf(typeof(NativeMethods.SecurityAttributes));
            security.bInheritHandle = true;
            return security;
        }

        private static string BuildCommandLine(string executable, string[] arguments)
        {
            var builder = new StringBuilder();
            builder.Append(QuoteArgument(executable));
            foreach (string argument in arguments)
            {
                builder.Append(' ');
                builder.Append(QuoteArgument(argument ?? String.Empty));
            }
            return builder.ToString();
        }

        private static string QuoteArgument(string argument)
        {
            if (argument.Length != 0 && argument.IndexOfAny(new[] { ' ', '\t', '\n', '\v', '"' }) < 0)
                return argument;

            var result = new StringBuilder();
            result.Append('"');
            int backslashes = 0;
            foreach (char value in argument)
            {
                if (value == '\\')
                {
                    backslashes++;
                }
                else if (value == '"')
                {
                    result.Append('\\', backslashes * 2 + 1);
                    result.Append('"');
                    backslashes = 0;
                }
                else
                {
                    result.Append('\\', backslashes);
                    result.Append(value);
                    backslashes = 0;
                }
            }
            result.Append('\\', backslashes * 2);
            result.Append('"');
            return result.ToString();
        }

        private static IntPtr BuildEnvironmentBlock(IDictionary overrides)
        {
            var values = new SortedDictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            foreach (DictionaryEntry entry in Environment.GetEnvironmentVariables())
                values[(string)entry.Key] = (string)entry.Value;
            if (overrides != null)
            {
                foreach (DictionaryEntry entry in overrides)
                {
                    string key = Convert.ToString(entry.Key);
                    if (entry.Value == null) values.Remove(key);
                    else values[key] = Convert.ToString(entry.Value);
                }
            }

            var block = new StringBuilder();
            foreach (var entry in values)
            {
                block.Append(entry.Key);
                block.Append('=');
                block.Append(entry.Value);
                block.Append('\0');
            }
            block.Append('\0');
            byte[] bytes = Encoding.Unicode.GetBytes(block.ToString());
            IntPtr pointer = Marshal.AllocHGlobal(bytes.Length);
            Marshal.Copy(bytes, 0, pointer, bytes.Length);
            return pointer;
        }
    }

    internal static class NativeMethods
    {
        internal const uint CreateSuspended = 0x00000004;
        internal const uint CreateNoWindow = 0x08000000;
        internal const uint CreateUnicodeEnvironment = 0x00000400;
        internal const uint StartfUseShowWindow = 0x00000001;
        internal const uint StartfUseStdHandles = 0x00000100;
        internal const short SwHide = 0;
        internal const uint JobObjectLimitKillOnJobClose = 0x00002000;
        internal const uint WaitObject0 = 0;
        internal const uint WaitTimeout = 258;
        internal const uint StillActive = 259;
        internal const uint GenericRead = 0x80000000;
        internal const uint GenericWrite = 0x40000000;
        internal const uint ProcessQueryLimitedInformation = 0x00001000;
        internal const uint Synchronize = 0x00100000;
        internal const uint FileShareRead = 0x00000001;
        internal const uint FileShareWrite = 0x00000002;
        internal const uint CreateAlways = 2;
        internal const uint OpenExisting = 3;
        internal const uint FileAttributeNormal = 0x00000080;
        internal const int ErrorAccessDenied = 5;
        internal const int ErrorInvalidParameter = 87;
        internal const int ErrorMoreData = 234;
        internal static readonly IntPtr InvalidHandleValue = new IntPtr(-1);

        internal enum JobObjectInfoType
        {
            BasicAccountingInformation = 1,
            BasicProcessIdList = 3,
            ExtendedLimitInformation = 9
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct FileTime
        {
            internal uint LowDateTime;
            internal uint HighDateTime;
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct SecurityAttributes
        {
            internal int nLength;
            internal IntPtr lpSecurityDescriptor;
            [MarshalAs(UnmanagedType.Bool)] internal bool bInheritHandle;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        internal struct StartupInfo
        {
            internal int cb;
            internal string lpReserved;
            internal string lpDesktop;
            internal string lpTitle;
            internal uint dwX;
            internal uint dwY;
            internal uint dwXSize;
            internal uint dwYSize;
            internal uint dwXCountChars;
            internal uint dwYCountChars;
            internal uint dwFillAttribute;
            internal uint dwFlags;
            internal short wShowWindow;
            internal short cbReserved2;
            internal IntPtr lpReserved2;
            internal IntPtr hStdInput;
            internal IntPtr hStdOutput;
            internal IntPtr hStdError;
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct ProcessInformation
        {
            internal IntPtr hProcess;
            internal IntPtr hThread;
            internal uint dwProcessId;
            internal uint dwThreadId;
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct JobObjectBasicAccountingInformation
        {
            internal long TotalUserTime;
            internal long TotalKernelTime;
            internal long ThisPeriodTotalUserTime;
            internal long ThisPeriodTotalKernelTime;
            internal uint TotalPageFaultCount;
            internal uint TotalProcesses;
            internal uint ActiveProcesses;
            internal uint TotalTerminatedProcesses;
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct JobObjectBasicLimitInformation
        {
            internal long PerProcessUserTimeLimit;
            internal long PerJobUserTimeLimit;
            internal uint LimitFlags;
            internal UIntPtr MinimumWorkingSetSize;
            internal UIntPtr MaximumWorkingSetSize;
            internal uint ActiveProcessLimit;
            internal UIntPtr Affinity;
            internal uint PriorityClass;
            internal uint SchedulingClass;
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct IoCounters
        {
            internal ulong ReadOperationCount;
            internal ulong WriteOperationCount;
            internal ulong OtherOperationCount;
            internal ulong ReadTransferCount;
            internal ulong WriteTransferCount;
            internal ulong OtherTransferCount;
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct JobObjectExtendedLimitInformation
        {
            internal JobObjectBasicLimitInformation BasicLimitInformation;
            internal IoCounters IoInfo;
            internal UIntPtr ProcessMemoryLimit;
            internal UIntPtr JobMemoryLimit;
            internal UIntPtr PeakProcessMemoryUsed;
            internal UIntPtr PeakJobMemoryUsed;
        }

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        internal static extern IntPtr CreateJobObject(IntPtr securityAttributes, string name);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool SetInformationJobObject(
            IntPtr job,
            JobObjectInfoType informationClass,
            IntPtr information,
            uint informationLength);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool QueryInformationJobObject(
            IntPtr job,
            JobObjectInfoType informationClass,
            IntPtr information,
            uint informationLength,
            out uint returnLength);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool AssignProcessToJobObject(IntPtr job, IntPtr process);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool IsProcessInJob(
            IntPtr process,
            IntPtr job,
            [MarshalAs(UnmanagedType.Bool)] out bool result);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool TerminateJobObject(IntPtr job, uint exitCode);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool CreateProcess(
            string applicationName,
            StringBuilder commandLine,
            IntPtr processAttributes,
            IntPtr threadAttributes,
            [MarshalAs(UnmanagedType.Bool)] bool inheritHandles,
            uint creationFlags,
            IntPtr environment,
            string currentDirectory,
            ref StartupInfo startupInfo,
            out ProcessInformation processInformation);

        [DllImport("kernel32.dll", SetLastError = true)]
        internal static extern uint ResumeThread(IntPtr thread);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool TerminateProcess(IntPtr process, uint exitCode);

        [DllImport("kernel32.dll", SetLastError = true)]
        internal static extern uint WaitForSingleObject(IntPtr handle, uint milliseconds);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool GetExitCodeProcess(IntPtr process, out uint exitCode);

        [DllImport("kernel32.dll", SetLastError = true)]
        internal static extern IntPtr OpenProcess(
            uint desiredAccess,
            [MarshalAs(UnmanagedType.Bool)] bool inheritHandle,
            uint processId);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool QueryFullProcessImageName(
            IntPtr process,
            uint flags,
            StringBuilder executableName,
            ref uint size);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool GetProcessTimes(
            IntPtr process,
            out FileTime creationTime,
            out FileTime exitTime,
            out FileTime kernelTime,
            out FileTime userTime);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool CloseHandle(IntPtr handle);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        internal static extern IntPtr CreateFile(
            string fileName,
            uint desiredAccess,
            uint shareMode,
            ref SecurityAttributes securityAttributes,
            uint creationDisposition,
            uint flagsAndAttributes,
            IntPtr templateFile);
    }
}
