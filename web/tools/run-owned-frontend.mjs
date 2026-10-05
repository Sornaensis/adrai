import { spawn } from 'node:child_process';
import { access, stat } from 'node:fs/promises';
import { constants } from 'node:fs';
import { dirname, isAbsolute, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { performance } from 'node:perf_hooks';

const started = performance.now();
const startedUtc = Date.now();
const repository = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const [mode, ...arguments_] = process.argv.slice(2);
const scripts = {
  components: join(repository, 'web/tests/run-components.ps1'),
  browser: join(repository, 'browser-tests/support/run-browser-tests.ps1'),
};
if (!scripts[mode]) throw new Error('Choose components or browser.');
const supplied = process.env.ADRAI_FRONTEND_REMAINING_MS ?? '3600000';
if (!/^\d+$/.test(supplied) || !Number.isSafeInteger(Number(supplied)) || Number(supplied) < 1) {
  throw new Error('ADRAI_FRONTEND_REMAINING_MS must be a positive remaining execution and cleanup budget.');
}
const powershell = process.env.PWSH_EXE ?? (process.platform === 'win32'
  ? join(process.env.SystemRoot ?? process.env.SYSTEMROOT ?? '', 'System32/WindowsPowerShell/v1.0/powershell.exe')
  : '');
if (!isAbsolute(powershell) || !(await stat(powershell)).isFile()) {
  throw new Error('Select an absolute PowerShell executable with PWSH_EXE.');
}
await access(powershell, process.platform === 'win32' ? constants.F_OK : constants.X_OK);
const remaining = Math.floor(Number(supplied) - (performance.now() - started));
if (remaining < 1) throw new Error('The frontend execution and cleanup budget expired before launch.');
const expiresUtc = new Date(startedUtc + Number(supplied)).toISOString();
const scriptArguments = mode === 'browser'
  ? ['-TestArgumentsBase64', Buffer.from(JSON.stringify(arguments_), 'utf8').toString('base64')]
  : arguments_;
const environment = { ...process.env };
if (process.platform === 'win32') {
  // A PowerShell 7 caller can export a module path that omits Windows
  // PowerShell's built-ins. Bind the selected engine's modules first.
  const moduleKeys = Object.keys(environment).filter(key => key.toUpperCase() === 'PSMODULEPATH');
  const inheritedModules = moduleKeys.map(key => environment[key]).filter(Boolean).join(';');
  for (const key of moduleKeys) delete environment[key];
  environment.PSModulePath = [join(dirname(powershell), 'Modules'), inheritedModules].filter(Boolean).join(';');
}
const child = spawn(powershell, [
  '-NoLogo', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', scripts[mode],
  '-NodeExe', process.execPath, '-NodePlatform', process.platform, '-NodeArchitecture', process.arch,
  '-RemainingMilliseconds', String(remaining), '-ExpiresUtc', expiresUtc, ...scriptArguments,
], { stdio: 'inherit', windowsHide: true, shell: false, env: environment });
let launchFailed = false;
let expired = false;
child.once('error', error => { launchFailed = true; console.error(error.message); process.exitCode = 1; });
const stop = () => { child.kill(); };
const deadline = setTimeout(() => { expired = true; stop(); },
  Math.max(0, Number(supplied) - (performance.now() - started)));
process.on('SIGINT', stop);
process.on('SIGTERM', stop);
child.once('close', code => {
  clearTimeout(deadline);
  process.removeListener('SIGINT', stop);
  process.removeListener('SIGTERM', stop);
  process.exitCode = launchFailed || expired || performance.now() - started > Number(supplied) ? 1 : code ?? 1;
});
