import { readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { buildAsset, webRoot } from './build.mjs';

export async function verifyAssets({ root = webRoot, compilerPath, signal } = {}) {
  const absoluteRoot = resolve(root);
  const expected = await buildAsset({ root: absoluteRoot, compilerPath, write: false, signal });
  for (const [path, bytes] of [['app.js', expected.bundle], ['provenance.json', expected.provenance]]) {
    const checked = await readFile(join(absoluteRoot, 'dist', path));
    if (!checked.equals(bytes)) throw new Error(`Checked-in dist/${path} is stale; run npm run build.`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  verifyAssets().then(
    () => console.log('Checked-in web assets match a fresh optimized build.'),
    (error) => { console.error(error.message); process.exitCode = 1; },
  );
}
