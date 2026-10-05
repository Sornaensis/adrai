import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { cp, mkdtemp, readFile, rename, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import test from 'node:test';
import { performance } from 'node:perf_hooks';
import { buildAsset, nativeCompiler, webRoot } from '../tools/build.mjs';
import { verifyAssets } from '../tools/verify-assets.mjs';

const compilerPath = nativeCompiler(webRoot);
const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');
const supplied = process.env.ADRAI_FRONTEND_REMAINING_MS ?? '3600000';
if (!/^\d+$/.test(supplied) || !Number.isSafeInteger(Number(supplied)) || Number(supplied) < 1) {
  throw new Error('ADRAI_FRONTEND_REMAINING_MS must be a positive remaining execution and cleanup budget.');
}
const started = performance.now();
const cleanupReserve = 10000;
if (Number(supplied) <= cleanupReserve) throw new Error('No asset execution and cleanup allocation remains.');
const aggregateSignal = AbortSignal.timeout(Number(supplied) - cleanupReserve);
const assertCompletionWithinAllocation = () => assert.ok(
  performance.now() - started < Number(supplied),
  'Asset execution and cleanup exceeded the caller allocation',
);

test('optimized assets rebuild identically and copied source drift is rejected', async () => {
  const root = await mkdtemp(join(tmpdir(), 'adrai-asset-test-'));
  try {
    for (const name of ['src', 'static', 'tools', 'dist', 'elm.json', 'package.json', 'package-lock.json']) {
      await cp(join(webRoot, name), join(root, name), { recursive: true });
    }
    const first = await buildAsset({ root, compilerPath, signal: aggregateSignal });
    const second = await buildAsset({ root, compilerPath, signal: aggregateSignal });
    assert.equal(sha256(first.bundle), sha256(second.bundle));
    assert.deepEqual(first.provenance, second.provenance);
    await verifyAssets({ root, compilerPath, signal: aggregateSignal });

    for (const name of ['src/Main.elm', 'static/bridge.js', 'static/index.html']) {
      const path = join(root, name);
      const original = await readFile(path);
      try {
        await writeFile(path, Buffer.concat([original, Buffer.from('\n')]));
        await assert.rejects(verifyAssets({ root, compilerPath, signal: aggregateSignal }), /stale/);
      } finally {
        await writeFile(path, original);
      }
    }

    const appBeforeFailure = await readFile(join(root, 'dist', 'app.js'));
    const provenanceBeforeFailure = await readFile(join(root, 'dist', 'provenance.json'));
    const mainBeforeFailure = await readFile(join(root, 'src', 'Main.elm'));
    await writeFile(join(root, 'src', 'Main.elm'), 'this is not Elm');
    await assert.rejects(buildAsset({ root, compilerPath, signal: aggregateSignal }), /Elm compiler failed/);
    assert.deepEqual(await readFile(join(root, 'dist', 'app.js')), appBeforeFailure);
    assert.deepEqual(await readFile(join(root, 'dist', 'provenance.json')), provenanceBeforeFailure);

    await writeFile(join(root, 'src', 'Main.elm'), mainBeforeFailure);
    const bridgePath = join(root, 'static', 'bridge.js');
    const originalBridge = await readFile(bridgePath);
    await writeFile(bridgePath, Buffer.concat([originalBridge, Buffer.from('\n/* rollback probe */\n')]));
    const changed = await buildAsset({ root, compilerPath, write: false, signal: aggregateSignal });
    assert.notDeepEqual(changed.bundle, appBeforeFailure);
    assert.notDeepEqual(changed.provenance, provenanceBeforeFailure);
    let injected = false;
    await assert.rejects(buildAsset({
      root, compilerPath, signal: aggregateSignal,
      renameFile: (from, to) => {
        if (!injected && to.endsWith('provenance.json')) {
          injected = true;
          throw new Error('injected provenance rename failure');
        }
        return rename(from, to);
      },
    }), /injected provenance rename failure/);
    assert.equal(injected, true);
    assert.deepEqual(await readFile(join(root, 'dist', 'app.js')), appBeforeFailure);
    assert.deepEqual(await readFile(join(root, 'dist', 'provenance.json')), provenanceBeforeFailure);
  } finally {
    await rm(root, { recursive: true, force: true });
  }
  assertCompletionWithinAllocation();
});

test('cancelled native Elm compile closes before temporary cleanup', async () => {
  const controller = new AbortController();
  let compilerPid;
  await assert.rejects(buildAsset({
    write: false,
    signal: AbortSignal.any([aggregateSignal, controller.signal]),
    onCompilerSpawn: (pid, args) => {
      if (args[0] === 'make') {
        compilerPid = pid;
        controller.abort();
      }
    },
  }), /cancelled/);
  assert.ok(Number.isInteger(compilerPid));
  assert.throws(() => process.kill(compilerPid, 0), { code: 'ESRCH' });
  assertCompletionWithinAllocation();
});
