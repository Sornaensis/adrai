import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, lstatSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, realpathSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve, sep } from 'node:path';
import { expect, type Page, type Response } from '@playwright/test';

export type SeedKind = 'main' | 'linked' | 'conflicts' | 'paging';
export type DecisionRef = { adr: string; title: string; summary: string; body: string; at: string };
export type Sentinel = { path: string; bytes: string };
export interface P705Fixture {
  bootstrapUrl: string;
  origin: string;
  repository: string;
  mainRepository: string;
  linkedRepository?: string;
  adraiExe: string;
  baseHead: string;
  head: string;
  search: { domain: string; file: string; actor: string };
  decisions: { primary: DecisionRef; secondary?: DecisionRef; relevance?: DecisionRef; safeText?: DecisionRef; paging?: DecisionRef[]; conflicted?: DecisionRef; conflictByAxis?: { decision: DecisionRef; scope: DecisionRef; domain: DecisionRef; status: DecisionRef } };
  conflictHeads?: { decision: string[]; scope: string[]; domain: string[]; status: string[] };
  sentinels: { staged: Sentinel; unstaged: Sentinel; untracked: Sentinel };
  git(args: string[], cwd?: string): Promise<string>;
  adrai(args: string[], cwd?: string): Promise<string>;
  currentHead(cwd?: string): Promise<string>;
  assertSentinels(): Promise<void>;
  holdRepositoryLock(): Promise<() => Promise<void>>;
  diagnoseExactArchive(page: Page, oid: string, path: string): Promise<{
    archive: { exists: boolean; bytes?: number; sha256?: string; mtime_ms?: number };
    response: { status: number; code?: string; as_of?: string; message_class?: string };
    doctor?: { status: number; code?: string; as_of?: string };
    archive_after_sha256?: string;
  }>;
}

const oidPattern = /^[a-f0-9]{40}$/;
const actor = 'human:browser-fixture';
const sourceFile = 'src/feature.ts';

async function stopOwned(child: ChildProcessWithoutNullStreams): Promise<void> {
  if (!child.pid || child.exitCode !== null || child.signalCode !== null) return;
  const closed = new Promise<void>((resolveClose) => child.once('close', () => resolveClose()));
  child.kill('SIGTERM');
  let timer: NodeJS.Timeout | undefined;
  try {
    await Promise.race([closed, new Promise<never>((_, reject) => { timer = setTimeout(() => reject(new Error('owned child did not exit')), 8_000); })]);
  } finally {
    if (timer) clearTimeout(timer);
  }
}

async function checked(command: string, args: string[], cwd: string, timeoutMs = 20_000, acceptedStatuses = [0]): Promise<string> {
  if (process.env.P705_OWNED_JOB !== '1') throw new Error('P7-05 browser children require the owned-job supervisor');
  const child = spawn(command, args, { cwd, stdio: 'pipe', windowsHide: true });
  let stdout = '';
  let stderr = '';
  let timer: NodeJS.Timeout | undefined;
  const closed = new Promise<number | null>((resolveClose, rejectClose) => {
    child.once('error', rejectClose);
    child.once('close', resolveClose);
    child.stdout.on('data', (chunk: Buffer) => { stdout = (stdout + chunk.toString('utf8')).slice(-65_536); });
    child.stderr.on('data', (chunk: Buffer) => { stderr = (stderr + chunk.toString('utf8')).slice(-65_536); });
  });
  try {
    const status = await Promise.race([
      closed,
      new Promise<never>((_, rejectTimeout) => { timer = setTimeout(() => rejectTimeout(new Error(`${command} ${args[0] ?? ''} timed out`)), timeoutMs); }),
    ]);
    if (status === null || !acceptedStatuses.includes(status)) throw new Error(`${command} ${args[0] ?? ''} failed (${status}): ${stderr.trim()}`);
    return stdout.trimEnd();
  } catch (error) {
    await stopOwned(child);
    throw error;
  } finally {
    if (timer) clearTimeout(timer);
  }
}

function waitForReady(child: ChildProcessWithoutNullStreams): Promise<string> {
  return new Promise((resolveReady, rejectReady) => {
    let stdout = '';
    let stderr = '';
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
      stdout = (stdout + chunk.toString('utf8')).slice(-4_096);
      const ready = stdout.match(/ADRAI web ready at (http:\/\/127\.0\.0\.1:\d+\/\?token=[^\s]+)/);
      if (ready) finish(undefined, ready[1]);
    };
    const onErrorData = (chunk: Buffer) => { stderr = (stderr + chunk.toString('utf8')).slice(-1_200); };
    const onExit = (code: number | null) => finish(new Error(`web server exited before readiness (${code}): ${stderr}`));
    const onSpawnError = (error: Error) => finish(new Error(`web server failed to start: ${error.message}`));
    const deadline = setTimeout(() => finish(new Error(`web readiness timed out: ${stderr}`)), 25_000);
    child.stdout.on('data', onData);
    child.stderr.on('data', onErrorData);
    child.once('exit', onExit);
    child.once('error', onSpawnError);
  });
}

async function createDecision(adraiExe: string, repository: string, title: string, summary: string, body: string, domain = 'core'): Promise<{ adr: string; commit: string }> {
  const output = await checked(adraiExe, [
    'create', '--title', title, '--summary', summary, '--body', body,
    '--domain', domain, '--applies-to', 'src/**', '--actor', actor, '--json',
  ], repository, 30_000);
  const result = JSON.parse(output);
  if (!result.adr || !oidPattern.test(result.commit)) throw new Error('CLI create did not return a valid ADR and commit');
  return { adr: result.adr, commit: result.commit };
}

type ConflictSeed = { baseHead: string; head: string; adrs: { simultaneous: string; decision: string; scope: string; domain: string; status: string } };

async function seedConflicts(adraiExe: string, repository: string): Promise<ConflictSeed> {
  let phase = 'executable';
  try {
  const emitter = process.env.P705_WINDOW_FIXTURE_EXE;
  if (!emitter || !existsSync(emitter)) throw new Error('conflict seed requires the exact built fixture executable');
  phase = 'initial-head';
  const initialHead = await checked('git', ['rev-parse', 'HEAD'], repository);
  phase = 'emitter';
  const emitted = await checked(emitter, ['--conflicts', repository], repository, 50_000);
  phase = 'emitter-json';
  const result = JSON.parse(emitted);
  phase = 'emitter-shape';
  const expectedKeys = ['decision', 'domain', 'scope', 'simultaneous', 'status'];
  const adrs = result.adrs as Record<string, string> | undefined;
  if (!oidPattern.test(result.baseHead) || !oidPattern.test(result.head) || result.head === result.baseHead ||
      result.baseHead === initialHead || result.transactions !== 21 || !adrs ||
      JSON.stringify(Object.keys(adrs).sort()) !== JSON.stringify(expectedKeys) ||
      new Set(Object.values(adrs)).size !== 5 || Object.values(adrs).some((adr) => !/^A[A-Z0-9]{26}$/.test(adr))) {
    throw new Error('typed conflict fixture did not report five valid ADRs, 21 transactions, and advanced exact OIDs');
  }
  phase = 'head';
  const head = await checked('git', ['rev-parse', 'HEAD'], repository);
  if (head !== result.head) throw new Error('typed conflict fixture HEAD differs from its reported merge commit');
  phase = 'ancestry';
  await checked('git', ['merge-base', '--is-ancestor', result.baseHead, head], repository);
  phase = 'doctor';
  const doctor = JSON.parse(await checked(adraiExe, ['doctor', '--json'], repository, 30_000, [0, 4]));
  const issues = doctor.issues ?? [];
  const expectedAdrs = new Set(Object.values(adrs));
  const actualAdrs = new Set(issues.map((issue: any) => issue.adr_id));
  if (issues.length !== 5 || issues.some((issue: any) => issue.code !== 'ADR_CONFLICT') || doctor.counts?.errors !== 5 ||
      actualAdrs.size !== expectedAdrs.size || [...actualAdrs].some((adr) => !expectedAdrs.has(adr))) {
    throw new Error(`conflict fixture doctor unexpected issues: errors=${doctor.counts?.errors ?? 'none'} codes=${issues.map((issue: any) => issue.code).join(',')}`);
  }
  return { baseHead: result.baseHead, head, adrs: {
    simultaneous: adrs.simultaneous,
    decision: adrs.decision,
    scope: adrs.scope,
    domain: adrs.domain,
    status: adrs.status,
  } };
  } catch (error) {
    if (phase === 'emitter') {
      const message = error instanceof Error ? error.message : '';
      const marker = /\bP705_CONFLICT_EMITTER_STAGE=(?:root|discovery|head|snapshot|typed|create|switch|amend|scope|domain|obsolete|merge|ancestry|topology|unknown)(?=\r?\n|$)/.exec(message);
      if (marker) console.log(marker[0]);
    }
    console.log(`P705_CONFLICT_SEED_FAILURE=${phase}`);
    throw new Error('typed conflict fixture setup failed');
  }
}

async function inspectConflicts(bootstrapUrl: string, seed: ConflictSeed): Promise<{
  conflicted: DecisionRef;
  conflictByAxis: NonNullable<P705Fixture['decisions']['conflictByAxis']>;
  conflictHeads: NonNullable<P705Fixture['conflictHeads']>;
}> {
  const origin = new URL(bootstrapUrl).origin;
    const bootstrap = await fetch(bootstrapUrl, { redirect: 'manual' });
    const cookie = bootstrap.headers.get('set-cookie')?.split(';')[0];
    if (!cookie || !/^adrai_/.test(cookie)) throw new Error('conflict preflight did not establish an authenticated session');
    const read = async (adr: string, view: 'collapsed' | 'exploded'): Promise<any> => {
      const deadline = Date.now() + 12_000;
      while (true) {
        const response = await fetch(`${origin}/api/v1/adrs/${adr}?at=${seed.head}&view=${view}`, { headers: { Cookie: cookie } });
        const body = await response.json() as any;
        if (response.status === 503 && body.error?.code === 'repository-busy' && Date.now() < deadline) {
          await new Promise((resolveWait) => setTimeout(resolveWait, 100));
          continue;
        }
        if (response.status !== 200 || body.metadata?.as_of?.oid !== seed.head || body.data?.adr !== adr) {
          throw new Error(`real conflict ${view} GET failed: status=${response.status} code=${body.error?.code ?? 'none'}`);
        }
        return body.data;
      }
    };
    const projection = await read(seed.adrs.simultaneous, 'collapsed');
    const exploded = await read(seed.adrs.simultaneous, 'exploded');
    if (!Array.isArray(exploded.operations) || exploded.operations.length < 2) throw new Error('conflict fixture lacks real operation provenance');
    const conflictHeads = {
      decision: projection.record_heads,
      scope: projection.scope_heads,
      domain: projection.domain_heads,
      status: projection.status_heads,
    };
    if (Object.values(conflictHeads).some((heads) => !Array.isArray(heads) || heads.length !== 2 || new Set(heads).size !== 2)) {
      throw new Error('real merged fixture did not produce two distinct heads on every conflict axis');
    }
    const decisionItems = exploded.operations.flatMap((operation: any) => operation.items ?? []);
    if (conflictHeads.decision.some((head: string) => {
      const item = decisionItems.find((candidate: any) => candidate.item === head);
      return typeof item?.body !== 'string' || item.body.trim() === '';
    })) {
      throw new Error('real merged fixture lacks a readable body for each decision head');
    }
    const ref = (adr: string, value: any): DecisionRef => {
      if (!value.title || !value.summary || !value.body) throw new Error(`real conflict projection lacks readable primary fields for ${adr}`);
      return { adr, title: value.title, summary: value.summary, body: value.body, at: seed.head };
    };
    const conflictByAxis = {
      decision: ref(seed.adrs.decision, await read(seed.adrs.decision, 'collapsed')),
      scope: ref(seed.adrs.scope, await read(seed.adrs.scope, 'collapsed')),
      domain: ref(seed.adrs.domain, await read(seed.adrs.domain, 'collapsed')),
      status: ref(seed.adrs.status, await read(seed.adrs.status, 'collapsed')),
    };
    console.log(`P705_CONFLICT_FIXTURE=head=${seed.head} four_axes=2,2,2,2 expected_adr_conflicts=5 nonconflict_integrity_errors=0`);
    return { conflicted: ref(seed.adrs.simultaneous, projection), conflictByAxis, conflictHeads };
}

function assertWithinOwnedRoot(target: string): void {
  const root = realpathSync(process.env.P705_OWNED_TEMP_ROOT ?? tmpdir());
  const actual = realpathSync(target);
  if (!actual.startsWith(root + sep) || !actual.startsWith(join(root, 'scenario-'))) {
    throw new Error('refusing to remove unexpected browser fixture root');
  }
}

function assertNoReparseEntries(root: string): void {
  const pending = [root];
  while (pending.length > 0) {
    const current = pending.pop()!;
    for (const name of readdirSync(current)) {
      const entry = join(current, name);
      const info = lstatSync(entry);
      if (info.isSymbolicLink()) throw new Error('refusing to remove a reparse entry inside browser fixture');
      if (info.isDirectory()) pending.push(entry);
    }
  }
}

export async function withP705Server<T>(options: { scenarioId: string; seed: SeedKind }, action: (fixture: P705Fixture) => Promise<T>): Promise<T> {
  if (!/^B(0[1-9]|1[0-5])$/.test(options.scenarioId)) throw new Error('unknown P7-05 scenario ID');
  if (process.env.P705_OWNED_JOB !== '1' || !process.env.ADRAI_EXE || !process.env.P705_OWNED_TEMP_ROOT) {
    throw new Error('P7-05 requires owned-job runner, exact executable and owned temporary root');
  }
  const temporary = mkdtempSync(join(process.env.P705_OWNED_TEMP_ROOT, 'scenario-'));
  const mainRepository = join(temporary, 'main');
  mkdirSync(mainRepository);
  const adraiExe = resolve(process.env.ADRAI_EXE);
  let server: ChildProcessWithoutNullStreams | undefined;
  let lockChild: ChildProcessWithoutNullStreams | undefined;
  try {
    await checked('git', ['init', '--initial-branch=main'], mainRepository);
    await checked('git', ['config', 'user.name', 'ADRAI browser fixture'], mainRepository);
    await checked('git', ['config', 'user.email', 'browser-fixture@example.invalid'], mainRepository);
    mkdirSync(join(mainRepository, 'src'));
    writeFileSync(join(mainRepository, sourceFile), 'export const browserFixture = "committed";\n');
    writeFileSync(join(mainRepository, 'seed.txt'), 'P7-05 real browser fixture\n');
    await checked('git', ['add', '--', sourceFile, 'seed.txt'], mainRepository);
    await checked('git', ['commit', '-m', 'seed browser source'], mainRepository);
    await checked(adraiExe, ['init'], mainRepository, 40_000);
    const primaryFields = { title: 'Seeded browser decision', summary: 'A checked repository fixture', body: 'Use the real repository fixture.\n' };
    const primaryCreated = await createDecision(adraiExe, mainRepository, primaryFields.title, primaryFields.summary, primaryFields.body);
    let baseHead = primaryCreated.commit;
    let linkedRepository: string | undefined;
    if (options.seed === 'linked') {
      linkedRepository = join(temporary, 'linked');
      await checked('git', ['worktree', 'add', '-b', `linked-${options.scenarioId.toLowerCase()}`, linkedRepository], mainRepository);
    }
    const repository = linkedRepository ?? mainRepository;
    const secondaryFields = { title: 'Secondary browser decision', summary: 'A later checked decision', body: 'Use the later decision.\n' };
    const secondaryCreated = await createDecision(adraiExe, repository, secondaryFields.title, secondaryFields.summary, secondaryFields.body);
    let head = secondaryCreated.commit;
    if (options.seed === 'paging') {
      const emitter = process.env.P705_WINDOW_FIXTURE_EXE;
      if (!emitter || !existsSync(emitter)) throw new Error('paging seed requires the exact built window fixture executable');
      const basis = await checked('git', ['rev-parse', 'HEAD'], mainRepository);
      if (basis !== head) throw new Error('paging seed basis drifted before bulk emission');
      const emitted = await checked(emitter, [mainRepository, basis, '1001'], mainRepository, 35_000);
      if (emitted !== `documents=4004 decisions=1001 basis=${basis}`) throw new Error('window fixture emitter did not report exact canonical document count/basis');
      await checked('git', ['add', '--', 'architecture/adrai'], mainRepository, 20_000);
      await checked('git', ['commit', '-m', 'seed 1001 sealed browser decisions'], mainRepository, 20_000);
      head = await checked('git', ['rev-parse', 'HEAD'], mainRepository);
      if (!oidPattern.test(head) || head === basis) throw new Error('paging bulk commit did not advance exact HEAD');
      const compiled = JSON.parse(await checked(adraiExe, ['compile', '--json'], mainRepository, 35_000));
      if (compiled.errors !== 0 || compiled.issues !== 0) {
        throw new Error(`window fixture compiler reported errors=${compiled.errors ?? 'none'} issues=${compiled.issues ?? 'none'}`);
      }
      console.log(`P705_WINDOW_FIXTURE=basis=${basis} head=${head} documents=4004 decisions=1001 compiler_errors=0`);
    }
    const decisions: P705Fixture['decisions'] = {
      primary: { adr: primaryCreated.adr, ...primaryFields, at: head },
      secondary: { adr: secondaryCreated.adr, ...secondaryFields, at: head },
    };
    if (options.scenarioId === 'B04') {
      const relevanceFields = {
        title: 'Browser source relevance',
        summary: 'The browserFixture source is governed here',
        body: 'The browserFixture in src/feature.ts is the concrete browser source.\n',
      };
      const relevanceCreated = await createDecision(adraiExe, repository, relevanceFields.title, relevanceFields.summary, relevanceFields.body);
      head = relevanceCreated.commit;
      decisions.relevance = { adr: relevanceCreated.adr, ...relevanceFields, at: head };
    }
    if (options.scenarioId === 'B05') {
      const safeFields = { title: 'Safe text browser decision', summary: 'Literal markup remains text', body: '<script>window.__adraiUnsafe = true</script>\n<strong>Literal text</strong>\n' };
      const safeCreated = await createDecision(adraiExe, mainRepository, safeFields.title, safeFields.summary, safeFields.body);
      head = safeCreated.commit;
      decisions.safeText = { adr: safeCreated.adr, ...safeFields, at: head };
    }
    let conflictSeed: ConflictSeed | undefined;
    const conflictSeedStarted = Date.now();
    if (options.seed === 'conflicts') {
      conflictSeed = await seedConflicts(adraiExe, mainRepository);
      baseHead = conflictSeed.baseHead;
      head = conflictSeed.head;
      console.log(`P705_CONFLICT_SEED_MS=${Date.now() - conflictSeedStarted}`);
    }
    for (const decision of Object.values(decisions)) {
      if (decision && 'at' in decision) decision.at = head;
    }
    const sentinels = {
      staged: { path: 'caller-staged.txt', bytes: 'staged caller bytes\n' },
      unstaged: { path: sourceFile, bytes: 'export const browserFixture = "unstaged caller bytes";\n' },
      untracked: { path: 'caller-untracked.txt', bytes: 'untracked caller bytes\n' },
    };
    writeFileSync(join(repository, sentinels.staged.path), sentinels.staged.bytes);
    await checked('git', ['add', '--', sentinels.staged.path], repository);
    writeFileSync(join(repository, sentinels.unstaged.path), sentinels.unstaged.bytes);
    writeFileSync(join(repository, sentinels.untracked.path), sentinels.untracked.bytes);
    const initialStage = await checked('git', ['ls-files', '-s', '--', sentinels.staged.path], repository);
    const initialStagedStatus = await checked('git', ['status', '--porcelain', '--', sentinels.staged.path], repository);
    expect(initialStagedStatus).toBe(`A  ${sentinels.staged.path}`);
    expect(await checked('git', ['ls-tree', '--name-only', 'HEAD', '--', sentinels.staged.path], repository)).toBe('');
    const assertSentinels = async () => {
      for (const sentinel of Object.values(sentinels)) {
        expect(readFileSync(join(repository, sentinel.path), 'utf8')).toBe(sentinel.bytes);
      }
      expect(await checked('git', ['ls-files', '-s', '--', sentinels.staged.path], repository)).toBe(initialStage);
      expect(await checked('git', ['status', '--porcelain', '--', sentinels.staged.path], repository)).toBe(initialStagedStatus);
      expect(await checked('git', ['ls-tree', '--name-only', 'HEAD', '--', sentinels.staged.path], repository)).toBe('');
      expect((await checked('git', ['status', '--porcelain', '--', sentinels.unstaged.path], repository))).toMatch(/^ M /);
      expect((await checked('git', ['status', '--porcelain', '--', sentinels.untracked.path], repository))).toMatch(/^\?\? /);
    };
    const holdRepositoryLock = async (): Promise<() => Promise<void>> => {
      if (lockChild) throw new Error('fixture lock already held');
      const commonDir = await checked('git', ['rev-parse', '--path-format=absolute', '--git-common-dir'], repository);
      const lockPath = join(commonDir, 'adrai.lock');
      const scriptPath = join(temporary, 'hold-lock.ps1');
      writeFileSync(scriptPath, [
        'param([string] $LockPath)',
        '$ErrorActionPreference = "Stop"',
        '[Console]::Out.WriteLine("P705_LOCK_PHASE=entry")',
        '$clock = [Diagnostics.Stopwatch]::StartNew()',
        '$stream = $null',
        '$attempts = 0',
        'while ($null -eq $stream) {',
        '  $attempts += 1',
        '  [Console]::Out.WriteLine("P705_LOCK_ATTEMPT=$attempts")',
        '  try {',
        '    $stream = [IO.FileStream]::new($LockPath, [IO.FileMode]::OpenOrCreate, [IO.FileAccess]::ReadWrite, [IO.FileShare]::Read)',
        '  } catch {',
        '    $failure = $_.Exception',
        '    $native = $null',
        '    for ($depth = 0; $depth -lt 3 -and $null -ne $failure; $depth++) {',
        '      if ($failure -is [IO.IOException] -or $failure -is [UnauthorizedAccessException]) {',
        '        $native = $failure.HResult -band 0xffff',
        '        break',
        '      }',
        '      if ($failure.GetType().FullName -ne "System.Management.Automation.MethodInvocationException") { break }',
        '      $failure = $failure.InnerException',
        '    }',
        '    $class = switch ($native) { 32 { "sharing" } 33 { "lock" } 2 { "path" } 3 { "path" } 5 { "access" } default { "other" } }',
        '    [Console]::Out.WriteLine("P705_LOCK_CLASS=$class")',
        '    if ($class -ne "sharing" -and $class -ne "lock") { exit 1 }',
        '    if ($clock.ElapsedMilliseconds -ge 3000) {',
        '      [Console]::Out.WriteLine("P705_LOCK_EXHAUSTED")',
        '      exit 1',
        '    }',
        '    Start-Sleep -Milliseconds 50',
        '  }',
        '}',
        '[Console]::Out.WriteLine("P705_LOCK_PHASE=opened")',
        'try {',
        '  $bytes = [Text.Encoding]::ASCII.GetBytes("pid=$PID`n")',
        '  $stream.SetLength(0); $stream.Write($bytes, 0, $bytes.Length); $stream.Flush($true)',
        '  [Console]::Out.WriteLine("P705_LOCK_PHASE=flushed")',
        '  [Console]::Out.WriteLine("P705_LOCK_READY")',
        '  [Console]::In.ReadLine() | Out-Null',
        '} finally { $stream.Dispose() }',
      ].join('\r\n'));
      const child = spawn('powershell.exe', ['-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', scriptPath, lockPath], { cwd: repository, stdio: 'pipe', windowsHide: true });
      lockChild = child;
      const readinessStarted = Date.now();
      let phase: 'spawn' | 'entry' | 'opened' | 'flushed' | 'ready' = 'spawn';
      let cause: 'timeout' | 'exit' | 'spawn-error' | 'not-live' = 'timeout';
      let openClass: 'none' | 'sharing' | 'lock' | 'path' | 'access' | 'other' = 'none';
      let attempts = 0;
      let exhausted = false;
      let exitCode: number | null = null;
      const ready = new Promise<void>((resolveReady, rejectReady) => {
        let line = '';
        let dropping = false;
        child.stdout.on('data', (chunk: Buffer) => {
          for (const character of chunk.toString('utf8')) {
            if (character === '\n') {
              const complete = line.replace(/\r$/, '');
              if (!dropping) {
                if (complete === 'P705_LOCK_PHASE=entry' && phase === 'spawn') phase = 'entry';
                else if (/^P705_LOCK_ATTEMPT=(?:[1-9]\d{0,2})$/.test(complete) && phase === 'entry') {
                  const next = Number(complete.slice('P705_LOCK_ATTEMPT='.length));
                  if (next === attempts + 1) attempts = next;
                }
                else if (/^P705_LOCK_CLASS=(?:sharing|lock|path|access|other)$/.test(complete) && phase === 'entry') {
                  openClass = complete.slice('P705_LOCK_CLASS='.length) as typeof openClass;
                }
                else if (complete === 'P705_LOCK_EXHAUSTED' && phase === 'entry') exhausted = true;
                else if (complete === 'P705_LOCK_PHASE=opened' && phase === 'entry') phase = 'opened';
                else if (complete === 'P705_LOCK_PHASE=flushed' && phase === 'opened') phase = 'flushed';
                else if (complete === 'P705_LOCK_READY' && phase === 'flushed') {
                  phase = 'ready';
                  resolveReady();
                }
              }
              line = '';
              dropping = false;
            } else if (!dropping) {
              if (line.length < 128) line += character;
              else { line = ''; dropping = true; }
            }
          }
        });
        child.once('error', () => {
          cause = 'spawn-error';
          rejectReady(new Error('fixture lock child failed before ready'));
        });
        child.once('exit', (code) => {
          cause = 'exit';
          exitCode = typeof code === 'number' && code >= 0 && code <= 255 ? code : null;
          rejectReady(new Error('fixture lock child exited before ready'));
        });
      });
      let timer: NodeJS.Timeout | undefined;
      try {
        await Promise.race([ready, new Promise<never>((_, reject) => { timer = setTimeout(() => { cause = 'timeout'; reject(new Error('fixture lock readiness timed out')); }, 5_000); })]);
        if (child.exitCode !== null || child.signalCode !== null) {
          cause = 'not-live';
          exitCode = typeof child.exitCode === 'number' && child.exitCode >= 0 && child.exitCode <= 255 ? child.exitCode : null;
          throw new Error('fixture lock child exited after ready');
        }
        console.log(`P705_LOCK_ACQUIRED=attempts:${attempts} class:${openClass} elapsed_ms:${Math.min(Date.now() - readinessStarted, 999999)}`);
      } catch (error) {
        console.log(`P705_LOCK_FAILURE=phase:${phase} cause:${cause} class:${openClass} attempts:${attempts} exhausted:${exhausted ? 1 : 0} elapsed_ms:${Math.min(Date.now() - readinessStarted, 999999)} exit:${exitCode ?? 'none'}`);
        await stopOwned(child);
        lockChild = undefined;
        throw error;
      } finally {
        if (timer) clearTimeout(timer);
      }
      let released = false;
      return async () => {
        if (released) return;
        released = true;
        child.stdin.end('\n');
        await stopOwned(child);
        lockChild = undefined;
      };
    };
    const serverStarted = Date.now();
    server = spawn(adraiExe, ['web', '--no-open'], { cwd: repository, stdio: 'pipe', windowsHide: true });
    console.log(`P705_WEB_SERVER_PID=${server.pid}`);
    const bootstrapUrl = await waitForReady(server);
    if (conflictSeed) console.log(`P705_CONFLICT_SERVER_READY_MS=${Date.now() - serverStarted}`);
    const origin = new URL(bootstrapUrl).origin;
    console.log(`P705_ORIGIN=${origin}`);
    writeFileSync(join(process.env.P705_OWNED_TEMP_ROOT, 'listener.json'), JSON.stringify({ port: Number(new URL(bootstrapUrl).port) }));
    const preflightStarted = Date.now();
    const conflictDetails = conflictSeed ? await inspectConflicts(bootstrapUrl, conflictSeed) : undefined;
    if (conflictDetails) {
      decisions.conflicted = conflictDetails.conflicted;
      decisions.conflictByAxis = conflictDetails.conflictByAxis;
      console.log(`P705_CONFLICT_PREFLIGHT_MS=${Date.now() - preflightStarted}`);
    }
    const fixture: P705Fixture = {
      bootstrapUrl, origin, repository, mainRepository, linkedRepository, adraiExe,
      baseHead, head, search: { domain: 'core', file: sourceFile, actor }, decisions, conflictHeads: conflictDetails?.conflictHeads, sentinels,
      git: (args, cwd = repository) => checked('git', args, cwd),
      adrai: (args, cwd = repository) => checked(adraiExe, args, cwd, 40_000),
      currentHead: (cwd = repository) => checked('git', ['rev-parse', 'HEAD'], cwd),
      assertSentinels, holdRepositoryLock,
      diagnoseExactArchive: async (page, oid, path) => {
        if (!oidPattern.test(oid)) throw new Error('archive diagnostic requires an exact commit OID');
        const archivePath = join(repository, '.adrai', 'cache', `${oid}.sqlite`);
        const archive = existsSync(archivePath) ? {
          exists: true, bytes: statSync(archivePath).size,
          sha256: createHash('sha256').update(readFileSync(archivePath)).digest('hex'),
          mtime_ms: statSync(archivePath).mtimeMs,
        } : { exists: false };
        await new Promise((resolveWait) => setTimeout(resolveWait, 200));
        const url = new URL(path, origin);
        if (url.origin !== origin || !/^\/api\/v1\/(search|relevant|repository|adrs\/)/.test(url.pathname)) {
          throw new Error('archive diagnostic requires a same-origin safe GET route');
        }
        url.searchParams.set('at', oid);
        const response = await page.request.get(url.toString());
        const body = await response.json();
        const safe = {
          status: response.status(), code: body.error?.code, as_of: body.metadata?.as_of?.oid,
          message_class: body.error?.message === 'validated exact archive is unavailable' ? 'validated exact archive is unavailable' : undefined,
        };
        let doctor: { status: number; code?: string; as_of?: string } | undefined;
        let archiveAfterSha256: string | undefined;
        if (response.status() >= 500) {
          const doctorResponse = await page.request.get(`${origin}/api/v1/doctor?at=${oid}`);
          const doctorBody = await doctorResponse.json();
          doctor = { status: doctorResponse.status(), code: doctorBody.error?.code, as_of: doctorBody.metadata?.as_of?.oid };
          if (existsSync(archivePath)) archiveAfterSha256 = createHash('sha256').update(readFileSync(archivePath)).digest('hex');
        }
        const facts = { oid, archive, response: safe, doctor, archive_after_sha256: archiveAfterSha256 };
        console.log(`P705_ARCHIVE_DIAGNOSTIC=${JSON.stringify(facts)}`);
        return facts;
      },
    };
    await assertSentinels();
    return await action(fixture);
  } finally {
    try {
      if (lockChild) await stopOwned(lockChild);
      if (server) await stopOwned(server);
    } finally {
      assertWithinOwnedRoot(temporary);
      assertNoReparseEntries(temporary);
      rmSync(temporary, { recursive: true, force: true });
    }
  }
}

export async function readJsonWithBusyRetry(page: Page, path: string, options: { expectedOid?: string; deadlineMs?: number } = {}): Promise<{ status: number; body: any }> {
  const deadline = Date.now() + (options.deadlineMs ?? 5_000);
  do {
    const response = await page.request.get(path);
    const body = await response.json();
    if (response.status() === 503 && body.error?.code === 'repository-busy' && Date.now() < deadline) {
      await new Promise((resolveWait) => setTimeout(resolveWait, 100));
      continue;
    }
    expect(response.status(), JSON.stringify({ status: response.status(), code: body.error?.code })).toBe(200);
    if (options.expectedOid) expect(body.metadata?.as_of?.oid).toBe(options.expectedOid);
    return { status: response.status(), body };
  } while (Date.now() < deadline);
  throw new Error('bounded repository-busy read deadline exhausted');
}

export async function waitForPrimaryInspection(page: Page, decision: DecisionRef, oid: string): Promise<void> {
  const pane = page.locator('#inspector-pane');
  await expect(pane.locator('h3')).toHaveText(decision.title);
  await expect(pane.locator('h3 + p.meta')).toHaveText(new RegExp(`^${decision.adr} · [^·]+ · ${oid}$`));
  await expect(pane.locator('h3 + p + p')).toHaveText(decision.summary);
  await expect(pane.locator('h4:has-text("Decision body") + pre.body-text')).toHaveText(decision.body.trimEnd());
  await expect(pane.locator('h4').filter({ hasText: /^Operation / }).first()).toBeVisible();
  await expect(pane.locator('.error')).toHaveCount(0);
}

export function safeResponseFacts(response: Response, body?: any): { method: string; path: string; status: number; as_of?: string; adr?: string; code?: string } {
  const url = new URL(response.url());
  return {
    method: response.request().method(), path: url.pathname, status: response.status(),
    as_of: body?.metadata?.as_of?.oid, adr: body?.data?.adr, code: body?.error?.code,
  };
}
