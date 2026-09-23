import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process';
import { mkdtempSync, realpathSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve, sep } from 'node:path';

export interface BrowserServerFixture {
  bootstrapUrl: string;
  origin: string;
  repository: string;
}

const projectRoot = resolve(__dirname, '..', '..');

export async function checked(command: string, args: string[], cwd: string, timeout = 30_000): Promise<string> {
  if (process.env.P704_OWNED_JOB !== '1') throw new Error('P7-04 browser children require the owned-job supervisor');
  const child = spawn(command, args, { cwd, stdio: 'pipe', windowsHide: true });
  let stdout = '';
  let stderr = '';
  child.stdout.on('data', (chunk: Buffer) => { stdout = (stdout + chunk.toString('utf8')).slice(-65_536); });
  child.stderr.on('data', (chunk: Buffer) => { stderr = (stderr + chunk.toString('utf8')).slice(-65_536); });
  let timer: NodeJS.Timeout | undefined;
  const closed = new Promise<number | null>((resolveClose, rejectClose) => {
    child.once('error', rejectClose);
    child.once('close', resolveClose);
  });
  try {
    const status = await Promise.race([
      closed,
      new Promise<never>((_, rejectTimeout) => {
        timer = setTimeout(() => rejectTimeout(new Error(`${command} ${args[0] ?? ''} timed out`)), timeout);
      }),
    ]);
    if (status !== 0) throw new Error(`${command} ${args[0] ?? ''} failed (${status}): ${stderr.trim()}`);
    return stdout.trim();
  } catch (error) {
    await stopOwned(child);
    throw error;
  } finally {
    if (timer) clearTimeout(timer);
  }
}

async function executable(): Promise<string> {
  if (process.env.ADRAI_EXE) return resolve(process.env.ADRAI_EXE);
  const installRoot = await checked('stack', ['path', '--local-install-root'], projectRoot);
  return join(installRoot, 'bin', process.platform === 'win32' ? 'adrai.exe' : 'adrai');
}

function waitForReady(child: ChildProcessWithoutNullStreams): Promise<string> {
  return new Promise((resolveReady, rejectReady) => {
    let stdout = '';
    let stderr = '';
    const deadline = setTimeout(() => finish(new Error(`web readiness timed out: ${stderr.slice(-1200)}`)), 30_000);
    const finish = (error?: Error, url?: string) => {
      clearTimeout(deadline);
      child.stdout.off('data', onData);
      child.stderr.off('data', onErrorData);
      child.off('exit', onExit);
      child.off('error', onSpawnError);
      if (error) rejectReady(error);
      else resolveReady(url!);
    };
    const onData = (chunk: Buffer) => {
      stdout += chunk.toString('utf8');
      const ready = stdout.match(/ADRAI web ready at (http:\/\/127\.0\.0\.1:\d+\/\?token=[^\s]+)/);
      if (ready) finish(undefined, ready[1]);
    };
    const onErrorData = (chunk: Buffer) => { stderr += chunk.toString('utf8'); };
    const onExit = (code: number | null) => finish(new Error(`web server exited before readiness (${code}): ${stderr.slice(-1200)}`));
    const onSpawnError = (error: Error) => finish(new Error(`web server failed to start: ${error.message}`));
    child.stdout.on('data', onData);
    child.stderr.on('data', onErrorData);
    child.once('exit', onExit);
    child.once('error', onSpawnError);
  });
}

async function stopOwned(child: ChildProcessWithoutNullStreams): Promise<void> {
  if (!child.pid || child.exitCode !== null || child.signalCode !== null) return;
  const exited = new Promise<void>((resolveExit) => child.once('close', () => resolveExit()));
  child.kill('SIGTERM');
  let timer: NodeJS.Timeout | undefined;
  try {
    await Promise.race([exited, new Promise<never>((_, reject) => { timer = setTimeout(() => reject(new Error('owned web server did not exit')), 10_000); })]);
  } finally {
    if (timer) clearTimeout(timer);
  }
}

export async function withBrowserServer<T>(action: (fixture: BrowserServerFixture) => Promise<T>): Promise<T> {
  const temporary = mkdtempSync(join(tmpdir(), 'adrai-p704-browser-'));
  const repository = join(temporary, 'repo');
  const fs = await import('node:fs');
  fs.mkdirSync(repository);
  let child: ChildProcessWithoutNullStreams | undefined;
  try {
    await checked('git', ['init', '--initial-branch=main'], repository);
    await checked('git', ['config', 'user.name', 'ADRAI browser test'], repository);
    await checked('git', ['config', 'user.email', 'browser-test@example.invalid'], repository);
    writeFileSync(join(repository, 'seed.txt'), 'runtime browser seed\n');
    await checked('git', ['add', '--', 'seed.txt'], repository);
    await checked('git', ['commit', '-m', 'seed'], repository);
    const adrai = await executable();
    await checked(adrai, ['init'], repository, 60_000);
    child = spawn(adrai, ['web', '--no-open'], { cwd: repository, stdio: 'pipe', windowsHide: true });
    console.log(`P704_WEB_SERVER_PID=${child.pid}`);
    const bootstrapUrl = await waitForReady(child);
    console.log(`P704_ORIGIN=${new URL(bootstrapUrl).origin}`);
    return await action({ bootstrapUrl, origin: new URL(bootstrapUrl).origin, repository });
  } finally {
    try {
      if (child) await stopOwned(child);
    } finally {
      const root = realpathSync(tmpdir());
      const target = realpathSync(temporary);
      if (!target.startsWith(root + sep) || !target.startsWith(resolve(tmpdir(), 'adrai-p704-browser-'))) {
        throw new Error(`refusing to remove unexpected browser fixture path: ${target}`);
      }
      rmSync(target, { recursive: true, force: true });
    }
  }
}
