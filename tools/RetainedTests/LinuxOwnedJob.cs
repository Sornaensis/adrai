// Loaded only on Linux PowerShell/.NET. Windows continues to compile OwnedJob.cs
// with its existing PowerShell 5.1 compiler and native Job Object implementation.
using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;

namespace Adrai.RetainedTests
{
    public sealed class OwnedLaunchException : Exception
    {
        public OwnedLaunchException(string message, int error, bool clean, Exception inner)
            : base(message, inner) { NativeErrorCode = error; ProcessCleanupVerified = clean; }
        public int NativeErrorCode { get; private set; }
        public bool ProcessCleanupVerified { get; private set; }
    }

    public sealed class OwnedProcess : IDisposable
    {
        private OwnedJob owner;
        internal OwnedProcess(OwnedJob job) { owner = job; }
        // Controller PID is diagnostic metadata; cleanup never selects a PID.
        public int ProcessId { get { return owner.ControllerId; } }
        public bool WaitForExit(int milliseconds) { return owner.WaitForRoot(milliseconds); }
        public int GetExitCode() { return owner.RootCode; }
        public void Dispose() { owner = null; }
    }

    public sealed class OwnedJob : IDisposable
    {
        public static string HelperPath { get; set; }
        private Process controller;
        private Thread statusReader, errorReader;
        private readonly ManualResetEvent ready = new ManualResetEvent(false);
        private readonly ManualResetEvent created = new ManualResetEvent(false);
        private readonly ManualResetEvent executed = new ManualResetEvent(false);
        private readonly ManualResetEvent root = new ManualResetEvent(false);
        private readonly ManualResetEvent ended = new ManualResetEvent(false);
        private readonly object sync = new object();
        private readonly StringBuilder diagnostics = new StringBuilder();
        private bool treeReaped, readerFailed, disposed, started;
        private int launchError, cleanupError, rootCode;
        private int namespaceId;
        internal int ControllerId { get { return controller.Id; } }
        internal int RootCode
        {
            get { if (!root.WaitOne(0)) throw new InvalidOperationException("Payload root has not exited."); return rootCode; }
        }
        private void ReadStatus()
        {
            try
            {
                // Native status is framed, fixed-vocabulary and separate from
                // payload output. Bound each line rather than ReadLine buffers.
                var line = new StringBuilder();
                int c;
                while ((c = controller.StandardOutput.Read()) >= 0)
                {
                    if (c != '\n')
                    {
                        if (line.Length == 80) throw new IOException("Owner status frame too long.");
                        line.Append((char)c); continue;
                    }
                    string value = line.ToString(); line.Clear();
                    lock (sync)
                    {
                        int number;
                        if (value.StartsWith("CREATED ", StringComparison.Ordinal) && int.TryParse(value.Substring(8), out number) && number > 0 && !created.WaitOne(0))
                        { namespaceId = number; created.Set(); }
                        else if (value == "READY" && created.WaitOne(0) && !ready.WaitOne(0)) ready.Set();
                        else if (value == "EXEC" && ready.WaitOne(0) && !executed.WaitOne(0)) executed.Set();
                        else if (value.StartsWith("ROOT ", StringComparison.Ordinal) && int.TryParse(value.Substring(5), out number) && !root.WaitOne(0))
                        { rootCode = number; root.Set(); }
                        else if (value.StartsWith("ERROR ", StringComparison.Ordinal) && int.TryParse(value.Substring(6), out number) && launchError == 0)
                        { launchError = number; }
                        else if (value.StartsWith("FAULT ", StringComparison.Ordinal) && int.TryParse(value.Substring(6), out number) && number > 0 && cleanupError == 0)
                        { cleanupError = number; AppendDiagnostic("Namespace termination failed, errno=" + number + ". "); }
                        else if (value == "TREE" && !treeReaped) treeReaped = true;
                        else throw new IOException("Invalid owner status sequence.");
                    }
                }
                if (line.Length != 0) throw new IOException("Truncated owner status frame.");
            }
            catch (Exception error)
            {
                lock (sync) { readerFailed = true; AppendDiagnostic(error.Message); }
                RequestStop();
            }
            finally { ended.Set(); }
        }
        private void AppendDiagnostic(string text)
        {
            int remaining = 4096 - diagnostics.Length;
            if (remaining > 0) diagnostics.Append(text, 0, Math.Min(text.Length, remaining));
        }
        private void ReadDiagnostics()
        {
            try
            {
                int c;
                while ((c = controller.StandardError.Read()) >= 0)
                    lock (sync) { if (diagnostics.Length < 4096) diagnostics.Append((char)c); }
            }
            catch (Exception error) { lock (sync) { readerFailed = true; AppendDiagnostic(error.Message); } }
        }
        private void RequestStop()
        {
            lock (sync)
            {
                if (controller == null || !started) return;
                try { controller.StandardInput.Close(); } catch (IOException) { }
                catch (ObjectDisposedException) { }
            }
        }
        private bool WaitSignal(WaitHandle signal, int milliseconds)
        {
            var timer = Stopwatch.StartNew();
            for (;;)
            {
                lock (sync)
                {
                    if (readerFailed) throw new IOException("Owner status/diagnostic reader failed: " + diagnostics);
                    if (cleanupError != 0) throw new IOException("Linux namespace cleanup failed, errno=" + cleanupError);
                    if (launchError != 0) throw new IOException("Linux owner launch failed, errno=" + launchError);
                    if (signal.WaitOne(0)) return true;
                    if (ended.WaitOne(0)) throw new IOException("Owner exited before expected status.");
                }
                int left = Math.Max(0, milliseconds - (int)Math.Min(timer.ElapsedMilliseconds, int.MaxValue));
                if (left == 0) return false;
                WaitHandle.WaitAny(new[] { signal, ended }, Math.Min(10, left));
            }
        }
        public OwnedProcess Launch(string executable, string[] arguments, string workingDirectory,
            IDictionary environmentOverrides, string stdoutPath, string stderrPath, int failureCleanupMilliseconds)
        {
            if (controller != null || disposed) throw new InvalidOperationException("Owner cannot be reused.");
            if (String.IsNullOrEmpty(HelperPath) || !Path.IsPathFullyQualified(HelperPath))
                throw new ArgumentException("An absolute verified Linux ownership helper is required.");
            var start = new ProcessStartInfo(HelperPath) { UseShellExecute = false, WorkingDirectory = workingDirectory,
                RedirectStandardInput = true, RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (string value in new[] { workingDirectory, stdoutPath, stderrPath, executable }) AddArgument(start, value);
            foreach (string value in arguments) AddArgument(start, value);
            if (environmentOverrides != null) foreach (DictionaryEntry entry in environmentOverrides)
            {
                string key = (string)entry.Key;
                if (entry.Value == null) start.Environment.Remove(key);
                else start.Environment[key] = (string)entry.Value;
            }
            try
            {
                controller = new Process { StartInfo = start };
                if (!controller.Start()) throw new IOException("Ownership controller did not start.");
                started = true;
                statusReader = new Thread(ReadStatus) { IsBackground = true };
                errorReader = new Thread(ReadDiagnostics) { IsBackground = true };
                statusReader.Start(); errorReader.Start();
                var timer = Stopwatch.StartNew();
                if (!WaitSignal(created, failureCleanupMilliseconds)) throw new IOException("Namespace creation containment expired.");
                controller.StandardInput.Write('A'); controller.StandardInput.Flush();
                int beforeReady = Math.Max(0, failureCleanupMilliseconds - (int)Math.Min(timer.ElapsedMilliseconds, int.MaxValue));
                if (!WaitSignal(ready, beforeReady)) throw new IOException("Owner readiness containment expired.");
                controller.StandardInput.Write('G'); controller.StandardInput.Flush();
                int left = Math.Max(0, failureCleanupMilliseconds - (int)Math.Min(timer.ElapsedMilliseconds, int.MaxValue));
                if (!WaitSignal(executed, left)) throw new IOException("Owner exec acknowledgement containment expired.");
                return new OwnedProcess(this);
            }
            catch (Exception error)
            {
                if (!started && controller != null) { controller.Dispose(); controller = null; }
                RequestStop();
                bool clean = controller == null;
                if (controller != null)
                {
                    try { clean = WaitForEmpty(failureCleanupMilliseconds); }
                    catch { clean = false; }
                }
                throw new OwnedLaunchException(error.Message, launchError, clean, error);
            }
        }
        private static void AddArgument(ProcessStartInfo start, string value)
        {
            if (value == null || value.IndexOf('\0') >= 0) throw new ArgumentException("Invalid native argument.");
            start.ArgumentList.Add(value);
        }
        internal bool WaitForRoot(int milliseconds) { return WaitSignal(root, milliseconds); }
        public void Terminate(int ignoredCode) { RequestStop(); }
        public uint ActiveProcessCount()
        {
            if (controller == null || !started) return 0;
            return WaitForEmpty(0) ? 0U : 1U;
        }
        public bool WaitForEmpty(int milliseconds)
        {
            if (controller == null || !started) return true;
            var timer = Stopwatch.StartNew();
            if (!controller.WaitForExit(milliseconds)) return false;
            int left = Math.Max(0, milliseconds - (int)Math.Min(timer.ElapsedMilliseconds, int.MaxValue));
            if (statusReader != null && !statusReader.Join(left)) return false;
            left = Math.Max(0, milliseconds - (int)Math.Min(timer.ElapsedMilliseconds, int.MaxValue));
            if (errorReader != null && !errorReader.Join(left)) return false;
            lock (sync) { return treeReaped && !readerFailed && cleanupError == 0; }
        }
        public object CaptureFailureSnapshot()
        {
            return new { complete = false, partialEvidence = true, memberEnumerationSupported = false,
                activeNamespace = ActiveProcessCount() != 0,
                authority = "native controller pidfd and unreaped namespace PID1", controllerId = ControllerId,
                namespacePid = namespaceId,
                rootExited = root.WaitOne(0), namespaceReaped = treeReaped, cleanupNativeErrorCode = cleanupError };
        }
        public void Dispose()
        {
            if (disposed) return;
            RequestStop();
            // Disposing is not a cleanup assertion. Caller must verify/join with
            // its finite shared cleanup allocation before releasing resources.
            if (controller == null || WaitForEmpty(0))
            {
                if (controller != null) controller.Dispose();
                created.Dispose(); ready.Dispose(); executed.Dispose(); root.Dispose(); ended.Dispose();
            }
            disposed = true;
        }
    }
}
