import { spawn } from 'node:child_process';
import { existsSync, mkdirSync, writeFileSync } from 'node:fs';
import { createServer } from 'node:http';
import { join } from 'node:path';

const mode = process.argv[2];
const root = process.env.P704_OWNED_TEMP_ROOT;
if (!root) throw new Error('missing owned temporary root');
const listenerPath = join(root, 'listener.json');

if (mode === 'listener') {
  mkdirSync(join(root, 'profile'));
  mkdirSync(join(root, 'repository'));
  writeFileSync(join(root, 'profile', 'owned-marker'), 'browser profile probe\n');
  writeFileSync(join(root, 'repository', 'owned-marker'), 'repository fixture probe\n');
  const server = createServer((_request, response) => { response.end('owned'); });
  server.listen(0, '127.0.0.1', () => {
    const address = server.address();
    writeFileSync(listenerPath, JSON.stringify({ port: address.port, pid: process.pid }));
  });
} else if (['timeout', 'early-success', 'early-error'].includes(mode)) {
  const child = spawn(process.execPath, [process.argv[1], 'listener'], {
    stdio: 'ignore', windowsHide: true, shell: false, detached: true,
  });
  child.once('error', (error) => { console.error(error.message); process.exit(1); });
  const deadline = Date.now() + 5000;
  const ready = setInterval(() => {
    if (existsSync(listenerPath)) {
      clearInterval(ready);
      console.log(`P704_PROBE_DESCENDANT=${child.pid}`);
      if (mode === 'early-success') process.exit(0);
      if (mode === 'early-error') process.exit(17);
    } else if (Date.now() >= deadline) {
      clearInterval(ready);
      process.exit(1);
    }
  }, 20);
} else {
  throw new Error(`unsupported ownership probe: ${mode}`);
}
