import { createHash } from 'node:crypto';
import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { lstat, mkdir, mkdtemp, readFile, readdir, rename, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { basename, dirname, join, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

export const webRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const requiredInputs = [
  'elm.json',
  'package.json',
  'package-lock.json',
  'tools/build.mjs',
  'tools/verify-assets.mjs',
  'static/bridge.js',
  'static/index.html',
  'static/app.css',
];

function hash(bytes) {
  return createHash('sha256').update(bytes).digest('hex');
}

async function sourceFiles(root, directory = join(root, 'src')) {
  const files = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const path = join(directory, entry.name);
    if (entry.isSymbolicLink()) throw new Error(`Asset input is a symbolic link: ${relative(root, path)}`);
    if (entry.isDirectory()) files.push(...await sourceFiles(root, path));
    else if (entry.isFile()) files.push(relative(root, path).replaceAll('\\', '/'));
    else throw new Error(`Asset input is not a regular file: ${relative(root, path)}`);
  }
  return files;
}

async function inputsFor(root) {
  const paths = [...requiredInputs, ...await sourceFiles(root)].sort();
  const result = {};
  for (const path of paths) {
    const absolute = join(root, path);
    const info = await lstat(absolute);
    if (!info.isFile()) throw new Error(`Missing regular asset input: ${path}`);
    result[path] = hash(await readFile(absolute));
  }
  return result;
}

export function nativeCompiler(root = webRoot) {
  return join(root, 'node_modules', '@elm_binaries', `${process.platform}_${process.arch}`, process.platform === 'win32' ? 'elm.exe' : 'elm');
}

function runCompiler(root, compilerPath, args, signal, onCompilerSpawn) {
  if (signal?.aborted) return Promise.reject(new Error('Elm compiler cancelled.'));
  return new Promise((resolve, reject) => {
    const child = spawn(compilerPath ?? nativeCompiler(root), args, {
      cwd: root, windowsHide: true, stdio: ['ignore', 'pipe', 'pipe'],
    });
    let stdout = '';
    let stderr = '';
    let failure;
    let cancelled = false;
    const stop = () => { cancelled = true; child.kill(); };
    const timer = setTimeout(stop, 120000);
    signal?.addEventListener('abort', stop, { once: true });
    onCompilerSpawn?.(child.pid, args);
    const append = (current, chunk) => {
      if (current.length + chunk.length > 4 * 1024 * 1024) {
        stop();
        failure = new Error('Elm compiler output exceeded 4 MiB.');
        return current;
      }
      return current + chunk;
    };
    child.stdout.setEncoding('utf8');
    child.stderr.setEncoding('utf8');
    child.stdout.on('data', (chunk) => { stdout = append(stdout, chunk); });
    child.stderr.on('data', (chunk) => { stderr = append(stderr, chunk); });
    child.on('error', (error) => { failure = error; });
    child.on('close', (code) => {
      clearTimeout(timer);
      signal?.removeEventListener('abort', stop);
      if (failure) reject(new Error(`Elm compiler failed: ${failure.message}`));
      else if (cancelled) reject(new Error('Elm compiler cancelled or timed out.'));
      else if (code !== 0) reject(new Error(`Elm compiler failed: ${(stderr || stdout || `exit ${code}`).trim()}`));
      else resolve(stdout.trim());
    });
  });
}

async function toolchain(root, compilerPath, signal, onCompilerSpawn) {
  const pkg = JSON.parse(requireTextSync(join(root, 'package.json')));
  const node = process.version.slice(1);
  const npm = pkg.packageManager;
  const elm = await runCompiler(root, compilerPath, ['--version'], signal, onCompilerSpawn);
  if (node !== pkg.engines.node || npm !== 'npm@11.12.1' || elm !== '0.19.2') {
    throw new Error(`Asset toolchain mismatch: Node ${node}, ${npm}, Elm ${elm}`);
  }
  const agent = process.env.npm_config_user_agent;
  if (agent && !agent.startsWith('npm/11.12.1 ')) {
    throw new Error(`Asset npm version mismatch: ${agent.split(' ')[0]}`);
  }
  return { node, npm, elm };
}

// Synchronous read keeps the version check before any compiler output is created.
import { readFileSync } from 'node:fs';
function requireTextSync(path) { return readFileSync(path, 'utf8'); }

async function atomicWrite(path, bytes, temporary, renameFile) {
  const stage = join(dirname(path), `.${basename(path)}.${basename(temporary)}.tmp`);
  await writeFile(stage, bytes);
  await renameFile(stage, path);
}

async function publish(root, bundle, provenance, temporary, renameFile) {
  const dist = join(root, 'dist');
  await mkdir(dist, { recursive: true });
  const appPath = join(dist, 'app.js');
  const provenancePath = join(dist, 'provenance.json');
  const previousApp = existsSync(appPath) ? await readFile(appPath) : null;
  const previousProvenance = existsSync(provenancePath) ? await readFile(provenancePath) : null;
  let appReplaced = false;
  try {
    await atomicWrite(appPath, bundle, temporary, renameFile);
    appReplaced = true;
    await atomicWrite(provenancePath, provenance, temporary, renameFile);
  } catch (error) {
    if (appReplaced) {
      if (previousApp === null) await rm(appPath, { force: true });
      else await atomicWrite(appPath, previousApp, temporary, renameFile);
      if (previousProvenance === null) await rm(provenancePath, { force: true });
      else await atomicWrite(provenancePath, previousProvenance, temporary, renameFile);
    }
    throw error;
  } finally {
    await rm(join(dist, `.app.js.${basename(temporary)}.tmp`), { force: true });
    await rm(join(dist, `.provenance.json.${basename(temporary)}.tmp`), { force: true });
  }
}

export async function buildAsset({ root = webRoot, compilerPath, write = true, signal, renameFile = rename, onCompilerSpawn } = {}) {
  const absoluteRoot = resolve(root);
  const temporary = await mkdtemp(join(tmpdir(), 'adrai-assets-'));
  try {
    const inputs = await inputsFor(absoluteRoot);
    const tools = await toolchain(absoluteRoot, compilerPath, signal, onCompilerSpawn);
    const compiledPath = join(temporary, 'elm.js');
    await runCompiler(absoluteRoot, compilerPath, ['make', 'src/Main.elm', '--optimize', `--output=${compiledPath}`], signal, onCompilerSpawn);
    const elm = await readFile(compiledPath);
    const bridge = await readFile(join(absoluteRoot, 'static', 'bridge.js'));
    if (JSON.stringify(await inputsFor(absoluteRoot)) !== JSON.stringify(inputs)) {
      throw new Error('Asset inputs changed during compilation; retry the build.');
    }
    const bundle = Buffer.concat([elm, Buffer.from('\n;\n'), bridge, Buffer.from('\n')]);
    const provenance = Buffer.from(`${JSON.stringify({ schema: 'adrai/assets/v1', inputs, toolchain: tools, output: { 'dist/app.js': hash(bundle) } }, null, 2)}\n`);
    if (write) await publish(absoluteRoot, bundle, provenance, temporary, renameFile);
    return { bundle, provenance };
  } finally {
    await rm(temporary, { recursive: true, force: true });
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  buildAsset().then(
    ({ bundle }) => console.log(`Built dist/app.js (${hash(bundle)})`),
    (error) => { console.error(error.message); process.exitCode = 1; },
  );
}
