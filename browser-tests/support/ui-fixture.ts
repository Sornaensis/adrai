import { readFile } from 'node:fs/promises';
import { createServer } from 'node:http';
import { resolve } from 'node:path';
import type { BrowserContext, WebSocketRoute } from '@playwright/test';

const web = resolve(__dirname, '../../web');
const credential = 'browser-fixture-credential';
const initialHead = '1'.repeat(40);
const changedHead = '2'.repeat(40);
type Mutation = { method: string; path: string; body: Record<string, any>; authorized: boolean };

function deferred<T>() {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>(done => { resolve = done; });
  return { promise, resolve };
}

export async function startFixture(context: BrowserContext) {
  context.setDefaultTimeout(0);
  context.setDefaultNavigationTimeout(0);
  const document = await readFile(resolve(web, 'static/index.html'));
  const stylesheet = await readFile(resolve(web, 'static/app.css'));
  const bundle = await readFile(process.env.ADRAI_BROWSER_BUNDLE!);
  const cases = JSON.parse(await readFile(resolve(web, 'fixtures/api-v1.json'), 'utf8')).cases;
  const inspection = cases.rich_resolved.data;
  const state = { head: initialHead, generation: BigInt(cases.repository.metadata.generation), title: inspection.title, summary: inspection.summary, body: inspection.body };
  const sockets = new Set<WebSocketRoute>();
  const mutations: Mutation[] = [];
  const reads: Array<{ path: string; query: Record<string, string> }> = [];
  const errors: string[] = [];
  let held: { received: ReturnType<typeof deferred<Mutation>>; released: ReturnType<typeof deferred<void>> } | undefined;
  const holds: Array<ReturnType<typeof deferred<void>>> = [];

  function envelope(name: string) {
    const value = structuredClone(cases[name]);
    value.metadata.generation = String(state.generation);
    value.metadata.as_of = { kind: 'commit', oid: state.head };
    if ('as_of' in value.data) value.data.as_of = state.head;
    return value;
  }
  function event() {
    return { schema: 'adrai/events/v1', generation: String(state.generation), as_of: { kind: 'commit', oid: state.head }, event: { type: 'repository-invalidated', facts: ['head'] } };
  }
  await context.routeWebSocket('**/api/v1/events', socket => {
    sockets.add(socket);
    socket.onMessage(message => {
      const command = JSON.parse(String(message));
      if (command.type === 'authenticate') socket.send(JSON.stringify(event()));
    });
    socket.onClose(() => sockets.delete(socket));
  });

  const server = createServer(async (request, response) => {
    const url = new URL(request.url!, 'http://fixture');
    try {
    if (url.pathname === '/') { response.setHeader('Content-Type', 'text/html'); response.end(document); return; }
    if (url.pathname === '/app.js') { response.setHeader('Content-Type', 'application/javascript'); response.end(bundle); return; }
    if (url.pathname === '/app.css') { response.setHeader('Content-Type', 'text/css'); response.end(stylesheet); return; }
    if (url.pathname === '/favicon.ico') { response.statusCode = 204; response.end(); return; }
    response.setHeader('Content-Type', 'application/json');
    let value;
    if (!['GET', 'POST'].includes(request.method!)) {
      errors.push(`Unsupported ${request.method} ${url.pathname}`); response.statusCode = 405; response.end('{}'); return;
    }
    if (request.method === 'GET' && url.searchParams.has('at') && !['HEAD', state.head].includes(url.searchParams.get('at')!)) {
      errors.push(`Unsupported revision for ${url.pathname}`); response.statusCode = 404; response.end('{}'); return;
    }
    if (request.method === 'POST') {
      if (!['/api/v1/adrs', `/api/v1/adrs/${inspection.adr}/amend`].includes(url.pathname)) {
        errors.push(`Unsupported POST ${url.pathname}`); response.statusCode = 404; response.end('{}'); return;
      }
      const chunks = [];
      for await (const chunk of request) chunks.push(chunk);
      const body = JSON.parse(Buffer.concat(chunks).toString());
      const mutation = { method: request.method, path: url.pathname, body, authorized: request.headers.authorization === `Bearer ${credential}` };
      mutations.push(mutation);
      state.head = changedHead;
      state.generation += 1n;
      state.title = body.title;
      state.summary = body.summary;
      state.body = body.body;
      value = envelope(url.pathname.endsWith('/amend') ? 'mutation_amend' : 'mutation_create');
      value.data.commit = state.head;
      value.data.index_revision = state.head;
      const gate = held;
      held = undefined;
      if (gate) { gate.received.resolve(mutation); await gate.released.promise; }
    } else if (url.pathname === '/api/v1/repository') {
      reads.push({ path: url.pathname, query: Object.fromEntries(url.searchParams) });
      value = envelope('repository');
      value.data.head = state.head;
      value.data.repository_state.head = state.head;
      value.data.repository_state.token = 'R' + (state.head === initialHead ? 'a' : 'b').repeat(43);
    } else if (url.pathname === '/api/v1/search') {
      reads.push({ path: url.pathname, query: Object.fromEntries(url.searchParams) });
      value = envelope('search_blank');
      value.data.results = [{ ...value.data.results[0], adr: inspection.adr, id: inspection.adr, title: state.title, summary: state.summary, status: 'active', obsolete: false, resolution_required: false, resolved: true, state_token: inspection.state_token, domains: inspection.domains, applies_to: inspection.applies_to }];
    } else if (url.pathname === `/api/v1/adrs/${inspection.adr}` && ['collapsed', 'exploded'].includes(url.searchParams.get('view')!)) {
      value = envelope(url.searchParams.get('view') === 'exploded' ? 'exploded' : 'rich_resolved');
      if (url.searchParams.get('view') !== 'exploded') {
        value.data.title = state.title; value.data.summary = state.summary; value.data.body = state.body;
      } else {
        for (const operation of value.data.operations) for (const item of operation.items) if (item.type === 'decision') {
          item.title = state.title; item.summary = state.summary; item.body = state.body;
        }
      }
    } else { errors.push(`Unsupported ${request.method} ${url.pathname}`); response.statusCode = 404; response.end('{}'); return; }
    response.end(JSON.stringify(value));
    } catch (error) {
      errors.push(`Fixture request failed: ${request.method} ${url.pathname}`);
      response.statusCode = 500; response.end(JSON.stringify({ error: 'Fixture request failed' }));
    }
  });
  await new Promise<void>(done => server.listen(0, '127.0.0.1', done));
  const address = server.address();
  if (!address || typeof address === 'string') throw new Error('Fixture did not acquire a port.');
  const origin = `http://127.0.0.1:${address.port}`;
  return {
    origin, bootstrapUrl: `${origin}/?token=${credential}`, adr: inspection.adr, initialHead, changedHead, state, mutations, reads, errors,
    holdMutation() {
      const received = deferred<Mutation>(); const released = deferred<void>();
      held = { received, released }; holds.push(released);
      return { received: received.promise, release: () => released.resolve() };
    },
    externalChange() {
      state.head = changedHead; state.generation += 1n;
      for (const socket of sockets) socket.send(JSON.stringify(event()));
    },
    async close() {
      for (const hold of holds) hold.resolve();
      for (const socket of sockets) socket.close();
      const closed = new Promise<void>((done, reject) => server.close(error => error ? reject(error) : done()));
      server.closeAllConnections();
      await closed;
      if (server.listening) throw new Error('Fixture listener survived teardown.');
    },
  };
}

export type Fixture = Awaited<ReturnType<typeof startFixture>>;
