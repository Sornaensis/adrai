import { expect, test, type Page } from '@playwright/test';
import { readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import { withP705Server, waitForPrimaryInspection, type DecisionRef, type P705Fixture } from '../support/p705-server';

type SocketEvent = { kind: string; asOfKind: string; asOf: string; generation: string; facts: string[] };
type SocketConnection = { id: number; sent: string[]; events: SocketEvent[]; closed: boolean };
type SocketFacts = { sent: string[]; events: SocketEvent[]; closes: number; connections: SocketConnection[] };

const fullResyncFacts = [
  'repository-identity', 'head', 'index', 'sequencer', 'configuration', 'managed-source',
  'common-refs', 'packed-refs', 'reflogs', 'worktree-metadata', 'relevant-worktree-file',
];

function observeSockets(page: Page): SocketFacts {
  const facts: SocketFacts = { sent: [], events: [], closes: 0, connections: [] };
  page.on('websocket', (socket) => {
    const connection: SocketConnection = { id: facts.connections.length + 1, sent: [], events: [], closed: false };
    facts.connections.push(connection);
    socket.on('framesent', (frame) => {
      try {
        const message = JSON.parse(frame.payload.toString());
        if (message.type === 'authenticate') {
          facts.sent.push('authenticate');
          connection.sent.push('authenticate');
        } else if (message.type === 'active-files') {
          const interest = `active-files:${JSON.stringify(message.paths)}`;
          facts.sent.push(interest);
          connection.sent.push(interest);
        }
      } catch { /* Only typed frames are evidence. */ }
    });
    socket.on('framereceived', (frame) => {
      try {
        const message = JSON.parse(frame.payload.toString());
        if (message.schema === 'adrai/events/v1') {
          const event = {
            kind: message.event?.type ?? 'unknown',
            asOfKind: message.as_of?.kind ?? 'unknown',
            asOf: message.as_of?.oid ?? message.as_of?.reason ?? 'none',
            generation: message.generation,
            facts: Array.isArray(message.event?.facts) ? message.event.facts : [],
          };
          facts.events.push(event);
          connection.events.push(event);
        }
      } catch { /* Ignore non-event frames. */ }
    });
    socket.on('close', () => { facts.closes += 1; connection.closed = true; });
  });
  return facts;
}

async function captureNativeEventSocket(page: Page): Promise<void> {
  await page.addInitScript(() => {
    const NativeWebSocket = window.WebSocket;
    let active: WebSocket | null = null;
    Object.defineProperty(window, '__p705CloseActualSocket', {
      enumerable: false,
      value: () => {
        if (!active || active.readyState !== NativeWebSocket.OPEN) return false;
        active.close(4000, 'test-disconnect');
        return true;
      },
    });
    window.WebSocket = new Proxy(NativeWebSocket, {
      construct(target, args) {
        const socket = Reflect.construct(target, args) as WebSocket;
        if (new URL(socket.url).pathname === '/api/v1/events') active = socket;
        return socket;
      },
    });
  });
}

async function refreshHead(page: Page, head: string): Promise<void> {
  const deadline = Date.now() + 5_000;
  const timedOut = () => new Error(`Repository did not expose exact HEAD ${head} within the bounded busy window`);
  const remaining = () => {
    const milliseconds = deadline - Date.now();
    if (milliseconds <= 0) throw timedOut();
    return milliseconds;
  };
  const withinDeadline = async <T>(operation: Promise<T>): Promise<T> => {
    let timer!: ReturnType<typeof setTimeout>;
    const timeout = new Promise<never>((_, reject) => {
      timer = setTimeout(() => reject(timedOut()), remaining());
    });
    try {
      return await Promise.race([operation, timeout]);
    } finally {
      clearTimeout(timer);
    }
  };
  while (Date.now() < deadline) {
    const attemptBudget = remaining();
    const response = page.waitForResponse((candidate) =>
      new URL(candidate.url()).pathname === '/api/v1/repository' && candidate.request().method() === 'GET',
    { timeout: attemptBudget });
    const [observed] = await withinDeadline(Promise.all([
      response,
      page.getByRole('button', { name: 'Refresh repository' }).click({ timeout: attemptBudget }),
    ]));
    const body = await withinDeadline(observed.json());
    if (observed.status() === 200 && body.data?.head === head && body.metadata?.as_of?.oid === head) {
      await withinDeadline(expect(page.locator('.masthead')).toContainText(`HEAD ${head}`, { timeout: remaining() }));
      return;
    }
    expect(observed.status() === 503 && body.error?.code === 'repository-busy',
      `Unexpected repository response ${observed.status()} ${body.error?.code ?? 'none'}`).toBe(true);
    if (Date.now() < deadline) await withinDeadline(page.waitForTimeout(Math.min(150, remaining())));
  }
  throw timedOut();
}

async function selectDecision(page: Page, decision: DecisionRef, head: string): Promise<void> {
  const result = page.locator('#context-pane .result-list').getByRole('button', { name: decision.adr });
  for (let attempt = 0; attempt < 6; attempt += 1) {
    if (await result.isVisible() && (await page.locator('#context-pane').textContent())?.includes(`At ${head}`)) {
      await result.click();
      await waitForPrimaryInspection(page, decision, head);
      return;
    }
    const response = page.waitForResponse((candidate) =>
      new URL(candidate.url()).pathname === '/api/v1/search' && candidate.request().method() === 'GET', { timeout: 5_000 });
    await page.getByRole('button', { name: 'Load view' }).click();
    const observed = await response;
    const body = await observed.json();
    if (observed.status() === 200) {
      expect(body.metadata?.as_of?.oid).toBe(head);
      expect(body.data?.results?.some((hit: { adr: string }) => hit.adr === decision.adr)).toBe(true);
      await expect(page.locator('#context-pane')).toContainText(`At ${head}`);
      await expect(result).toBeVisible();
    } else {
      expect(body.error?.code).toBe('repository-busy');
    }
  }
  throw new Error(`ADR ${decision.adr} did not enter the exact ${head} result window`);
}

test.setTimeout(90_000);

test('B13 dirty draft survives real external ADR change and explicit review', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B13', seed: 'main' }, async (fixture: P705Fixture) => {
    const context = await browser.newContext();
    try {
      const page = await context.newPage();
      const socket = observeSockets(page);
      const writes: string[] = [];
      page.on('request', (request) => {
        if (request.method() === 'POST' && new URL(request.url()).pathname.startsWith('/api/v1/adrs')) writes.push('POST');
      });
      await page.goto(fixture.bootstrapUrl);
      const decision = fixture.decisions.primary;
      await selectDecision(page, decision, fixture.head);
      await page.getByLabel('Action').selectOption('amend');
      await expect(page.getByText(`Target: ${decision.adr}`)).toBeVisible();
      await page.getByLabel('Title').fill('Unsubmitted human revision');
      await page.getByLabel('Change summary').fill('Review the external revision');
      await page.getByLabel('Actor ID').fill('browser-live');
      const before = socket.events.filter((event) => event.kind === 'repository-invalidated').length;

      const external: DecisionRef = {
        ...decision,
        title: 'Externally revised decision',
        summary: 'External CLI changed the decision',
        body: 'The external CLI revision is authoritative.\n',
      };
      await fixture.adrai([
        'amend', decision.adr, '--title', external.title, '--summary', external.summary,
        '--body', external.body, '--change-summary', 'External concurrent amendment',
        '--actor', 'human:external-browser', '--json',
      ]);
      const changedHead = await fixture.currentHead();
      expect(changedHead).not.toBe(fixture.head);
      await expect.poll(() => socket.events.filter((event) => event.kind === 'repository-invalidated').length,
        { timeout: 12_000 }).toBeGreaterThan(before);
      await expect(page.getByText(/Draft is stale/)).toBeVisible();
      await expect(page.getByRole('button', { name: 'Submit amend' })).toBeDisabled();
      await expect(page.getByLabel('Title')).toHaveValue('Unsubmitted human revision');
      expect(writes).toHaveLength(0);

      await refreshHead(page, changedHead);
      await selectDecision(page, external, changedHead);
      await expect(page.locator('#actions-pane')).toContainText(`Title: ${decision.title}`);
      await expect(page.locator('#actions-pane')).toContainText(`Title: ${external.title}`);
      await expect(page.getByLabel('Title')).toHaveValue('Unsubmitted human revision');
      await page.getByRole('button', { name: 'Review heads and adopt current tokens' }).click();
      await expect(page.getByText('Current heads reviewed; fresh tokens adopted.')).toBeVisible();
      await expect(page.getByRole('button', { name: 'Submit amend' })).toBeEnabled();
      expect(writes).toHaveLength(0);
      await fixture.assertSentinels();
      console.log(`P705_B13=head:${changedHead} invalidations:${socket.events.length} writes:${writes.length}`);
    } finally {
      await context.close();
    }
  });
});

test('B14 real delayed reads and committed responses preserve current context and newer draft', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B14', seed: 'main' }, async (fixture: P705Fixture) => {
    const context = await browser.newContext();
    const page = await context.newPage();
    let releaseOldRead: () => void = () => {};
    let releaseCommit: () => void = () => {};
    try {
      const socket = observeSockets(page);
      const posts: string[] = [];
      page.on('request', (request) => {
        if (request.method() === 'POST' && new URL(request.url()).pathname.includes('/amend')) posts.push('amend');
      });
      await page.goto(fixture.bootstrapUrl);
      const decision = fixture.decisions.primary;
      await page.getByRole('button', { name: 'Search', exact: true }).click();
      await page.getByLabel('Retrieval mode').selectOption('fts');
      await page.getByLabel('Search terms').fill(decision.title);

      const oldReadGate = new Promise<void>((resolve) => { releaseOldRead = resolve; });
      let oldReadReady!: (facts: { status: number; body: any; path: string }) => void;
      const oldRead = new Promise<{ status: number; body: any; path: string }>((resolve) => { oldReadReady = resolve; });
      let oldReadDelivered!: () => void;
      const oldDelivered = new Promise<void>((resolve) => { oldReadDelivered = resolve; });
      const oldReadRoute = async (route: import('@playwright/test').Route) => {
        const requestUrl = new URL(route.request().url());
        if (requestUrl.searchParams.get('q') !== decision.title) {
          await route.continue();
          return;
        }
        const actual = await route.fetch();
        if (actual.status() === 503) {
          await route.fulfill({ response: actual });
          return;
        }
        const body = await actual.json();
        if (actual.status() !== 200) {
          console.log(`P705_B14_OLD_READ_ERROR=status:${actual.status()} code:${body.error?.code ?? 'none'} as_of:${body.metadata?.as_of?.oid ?? body.metadata?.as_of?.reason ?? 'none'}`);
          await route.fulfill({ response: actual });
          oldReadReady({ status: actual.status(), body, path: requestUrl.pathname + requestUrl.search });
          return;
        }
        oldReadReady({ status: actual.status(), body, path: requestUrl.pathname + requestUrl.search });
        await oldReadGate;
        await route.fulfill({ response: actual });
        oldReadDelivered();
      };
      await page.route('**/api/v1/search?*', oldReadRoute);
      await page.getByRole('button', { name: 'Load view' }).click();
      const held = await oldRead;
      if (held.status === 500) {
        releaseOldRead();
        await page.unrouteAll({ behavior: 'wait' });
        await fixture.diagnoseExactArchive(page, fixture.head, held.path);
      }
      expect(held.status).toBe(200);
      expect(held.body.metadata?.as_of?.oid).toBe(fixture.head);
      expect(held.body.data?.results?.some((hit: { adr: string }) => hit.adr === decision.adr)).toBe(true);

      await page.getByLabel('Search terms').fill('qxjrvnbkmtpl');
      await page.getByLabel('Domain filter').fill('p705nonexistent');
      let newerBusy = 0;
      const newerResponse = page.waitForResponse((response) => {
        const url = new URL(response.url());
        if (url.pathname !== '/api/v1/search' || url.searchParams.get('q') !== 'qxjrvnbkmtpl' || url.searchParams.get('domain') !== 'p705nonexistent') return false;
        if (response.status() === 503) newerBusy += 1;
        return response.status() !== 503 || newerBusy >= 3;
      }, { timeout: 12_000 });
      await page.getByRole('button', { name: 'Load view' }).click();
      const newer = await newerResponse;
      const newerBody = await newer.json();
      if (newer.status() === 500) {
        const url = new URL(newer.url());
        releaseOldRead();
        await fixture.diagnoseExactArchive(page, fixture.head, url.pathname + url.search);
      }
      expect(newer.status(), `newer search error ${newerBody.error?.code ?? 'none'} as_of ${newerBody.metadata?.as_of?.oid ?? newerBody.metadata?.as_of?.reason ?? 'none'}`).toBe(200);
      expect(newerBody.metadata?.as_of?.oid).toBe(fixture.head);
      expect(newerBody.data?.results).toHaveLength(0);
      releaseOldRead();
      await oldDelivered;
      await expect(page.locator('#context-pane')).toContainText('0 results in a bounded window');
      await page.unroute('**/api/v1/search?*', oldReadRoute);

      await page.getByLabel('Domain filter').fill('');
      await page.getByRole('button', { name: 'Browse', exact: true }).click();
      await selectDecision(page, decision, fixture.head);
      await page.getByLabel('Action').selectOption('amend');
      await page.getByLabel('Title').fill('Browser committed revision');
      await page.getByLabel('Change summary').fill('Real delayed commit response');
      await page.getByLabel('Actor ID').fill('browser-ordering');
      await expect(page.getByRole('button', { name: 'Submit amend' })).toBeEnabled();

      const commitGate = new Promise<void>((resolve) => { releaseCommit = resolve; });
      let committedReady!: (facts: { status: number; body: any }) => void;
      const committed = new Promise<{ status: number; body: any }>((resolve) => { committedReady = resolve; });
      const amendPath = `/api/v1/adrs/${decision.adr}/amend`;
      const commitRoute = async (route: import('@playwright/test').Route) => {
        const actual = await route.fetch();
        const body = await actual.json();
        committedReady({ status: actual.status(), body });
        await commitGate;
        await route.fulfill({ response: actual });
      };
      await page.route(`**${amendPath}`, commitRoute);
      const beforeInvalidation = socket.events.filter((event) => event.kind === 'repository-invalidated').length;
      await page.getByRole('button', { name: 'Submit amend' }).click();
      const confirmed = await committed;
      expect(confirmed.status).toBe(200);
      expect(confirmed.body.data?.committed).toBe(true);
      const committedOid: string = confirmed.body.data.commit;
      const operation: string = confirmed.body.data.operation;
      expect(committedOid).toMatch(/^[a-f0-9]{40}$/);
      expect(operation).toBeTruthy();
      expect(confirmed.body.metadata?.as_of?.oid).toBe(committedOid);
      await expect.poll(() => socket.events.filter((event) => event.kind === 'repository-invalidated').length,
        { timeout: 12_000 }).toBeGreaterThan(beforeInvalidation);
      await refreshHead(page, committedOid);
      await page.getByLabel('Title').fill('Newer unsent human draft');
      releaseCommit();
      await expect(page.locator('#actions-pane')).toContainText(`Committed ${operation} at ${committedOid}.`);
      await expect(page.getByLabel('Title')).toHaveValue('Newer unsent human draft');
      expect(posts).toHaveLength(1);
      await page.unroute(`**${amendPath}`, commitRoute);

      const revised = { ...decision, title: 'Browser committed revision', at: committedOid };
      await selectDecision(page, revised, committedOid);
      await page.getByRole('button', { name: 'Review heads and adopt current tokens' }).click();
      await page.getByLabel('Title').fill('Second human draft with lost response');
      await page.getByLabel('Change summary').fill('Actual commit with network ambiguity');
      await expect(page.getByRole('button', { name: 'Submit amend' })).toBeEnabled();
      let ambiguousReady!: (facts: { status: number; body: any }) => void;
      const ambiguous = new Promise<{ status: number; body: any }>((resolve) => { ambiguousReady = resolve; });
      const abortAfterRealCommit = async (route: import('@playwright/test').Route) => {
        const actual = await route.fetch();
        ambiguousReady({ status: actual.status(), body: await actual.json() });
        await route.abort('failed');
      };
      await page.route(`**${amendPath}`, abortAfterRealCommit);
      await page.getByRole('button', { name: 'Submit amend' }).click();
      const lost = await ambiguous;
      expect(lost.status).toBe(200);
      expect(lost.body.data?.committed).toBe(true);
      expect(await fixture.currentHead()).toBe(lost.body.data.commit);
      await expect(page.locator('#actions-pane')).toContainText('outcome is uncertain');
      await expect(page.getByLabel('Title')).toHaveValue('Second human draft with lost response');
      await page.waitForTimeout(3_000);
      expect(posts).toHaveLength(2);
      await expect(page.locator('#actions-pane')).toContainText('outcome is uncertain');
      await expect(page.getByLabel('Title')).toHaveValue('Second human draft with lost response');
      await page.unroute(`**${amendPath}`, abortAfterRealCommit);
      await fixture.assertSentinels();
      console.log(`P705_B14=old-read:${held.status} committed:${committedOid} operation:${operation} lost-response:${lost.body.data.commit} posts:${posts.length}`);
    } finally {
      releaseOldRead();
      releaseCommit();
      await page.unrouteAll({ behavior: 'ignoreErrors' });
      await context.close();
    }
  });
});

test('B15 real socket reconnect, interests, busy reads and observation failure recover', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B15', seed: 'main' }, async (fixture: P705Fixture) => {
    const context = await browser.newContext();
    try {
      const page = await context.newPage();
      await captureNativeEventSocket(page);
      const socket = observeSockets(page);
      await page.goto(fixture.bootstrapUrl);
      await expect(page.locator('#context-pane')).toContainText('Live connection: open');
      await expect.poll(() => socket.events.filter((event) => event.kind === 'repository-invalidated' && event.asOf === fixture.head &&
        JSON.stringify(event.facts) === JSON.stringify(fullResyncFacts)).length,
        { timeout: 10_000 }).toBeGreaterThan(0);
      const initialFullResync = socket.events.find((event) => event.kind === 'repository-invalidated' &&
        event.asOf === fixture.head && JSON.stringify(event.facts) === JSON.stringify(fullResyncFacts));
      expect(initialFullResync?.generation).toMatch(/^(0|[1-9][0-9]*)$/);

      await page.getByRole('button', { name: 'Relevant', exact: true }).click();
      await page.getByLabel('Repository-relative source file').fill(fixture.sentinels.unstaged.path);
      await page.getByLabel('Use worktree source').check();
      let relevantBusy = 0;
      const relevantResponse = page.waitForResponse((response) => {
        const url = new URL(response.url());
        if (url.pathname !== '/api/v1/relevant' || url.searchParams.get('worktree') !== 'true') return false;
        if (response.status() === 503) relevantBusy += 1;
        return response.status() !== 503 || relevantBusy >= 3;
      }, { timeout: 12_000 });
      await page.getByRole('button', { name: 'Load view' }).click();
      const relevant = await relevantResponse;
      const relevance = await relevant.json();
      if (relevant.status() === 500) {
        const url = new URL(relevant.url());
        await fixture.diagnoseExactArchive(page, fixture.head, url.pathname + url.search);
      }
      expect(relevant.status(), `relevant error ${relevance.error?.code ?? 'none'} as_of ${relevance.metadata?.as_of?.oid ?? relevance.metadata?.as_of?.reason ?? 'none'}`).toBe(200);
      expect(relevance.metadata?.as_of?.oid).toBe(fixture.head);
      expect(relevance.data?.file?.source).toBe('worktree');
      const activeInterest = `active-files:${JSON.stringify([fixture.sentinels.unstaged.path])}`;
      await expect.poll(() => socket.sent.filter((frame) => frame === activeInterest).length,
        { timeout: 8_000 }).toBeGreaterThan(0);

      const original = [...socket.connections].reverse().find((connection) =>
        connection.sent.includes('authenticate') && connection.sent.includes(activeInterest) &&
        connection.events.some((event) => event.kind === 'repository-invalidated' && event.asOf === fixture.head &&
          JSON.stringify(event.facts) === JSON.stringify(fullResyncFacts)));
      expect(original).toBeDefined();
      const closedNativeSocket = await page.evaluate(() =>
        (window as Window & { __p705CloseActualSocket?: () => boolean }).__p705CloseActualSocket?.() ?? false);
      expect(closedNativeSocket).toBe(true);
      await expect.poll(() => original!.closed, { timeout: 6_000 }).toBe(true);
      await expect(page.locator('#context-pane')).toContainText(/Live connection: closed|Live connection: connecting|Live connection: unavailable/);
      await expect.poll(() => socket.connections.some((connection) =>
        connection.id > original!.id && connection.sent.includes('authenticate') &&
        connection.events.some((event) => event.kind === 'repository-invalidated' && event.asOf === fixture.head &&
          JSON.stringify(event.facts) === JSON.stringify(fullResyncFacts) &&
          BigInt(event.generation) > BigInt(initialFullResync!.generation)) &&
        connection.sent.includes(activeInterest)),
      { timeout: 15_000 }).toBe(true);
      await expect(page.locator('#context-pane')).toContainText('Live connection: open');

      await page.getByLabel('Repository-relative source file').fill('seed.txt');
      await page.getByRole('button', { name: 'Load view' }).click();
      await expect.poll(() => socket.sent.some((frame) => frame === 'active-files:["seed.txt"]'),
        { timeout: 8_000 }).toBe(true);
      const beforeRelease = socket.sent.length;
      await page.getByRole('button', { name: 'Browse', exact: true }).click();
      await expect.poll(() => socket.sent.slice(beforeRelease).includes('active-files:[]'),
        { timeout: 8_000 }).toBe(true);

      const releaseLock = await fixture.holdRepositoryLock();
      try {
        const busyResponse = page.waitForResponse((response) =>
          new URL(response.url()).pathname === '/api/v1/repository' && response.status() === 503,
        { timeout: 8_000 });
        await page.getByRole('button', { name: 'Refresh repository' }).click();
        const busy = await busyResponse;
        const busyBody = await busy.json();
        expect(['repository-busy', 'repository-lock-unavailable']).toContain(busyBody.error?.code);
        expect(busyBody.error?.status).toBe(503);
        await expect(page.locator('#context-pane')).toContainText(/busy|stale/i);
      } finally {
        await releaseLock();
      }
      await refreshHead(page, fixture.head);

      const configPath = join(fixture.repository, '.adrai.toml');
      const originalConfig = readFileSync(configPath);
      const beforeFailureGeneration = socket.events.at(-1)?.generation ?? initialFullResync!.generation;
      const unavailable = (event: SocketEvent) => event.kind === 'repository-invalidated' &&
        event.asOfKind === 'unavailable' && event.asOf === 'repository-observation-failed' &&
        JSON.stringify(event.facts) === JSON.stringify(fullResyncFacts) &&
        BigInt(event.generation) > BigInt(beforeFailureGeneration);
      let restored = false;
      try {
        writeFileSync(configPath, 'invalid = [\n');
        await expect.poll(() => socket.events.some(unavailable), { timeout: 10_000 }).toBe(true);
        const failureEvent = socket.events.find(unavailable)!;
        await expect(page.locator('#context-pane')).toContainText('Live observation unavailable: repository-observation-failed');
        await expect(page.locator('#context-pane')).toContainText('Snapshot is loading or stale.');
        const beforeFailureReconnect = socket.connections.at(-1)!;
        expect(await page.evaluate(() =>
          (window as Window & { __p705CloseActualSocket?: () => boolean }).__p705CloseActualSocket?.() ?? false)).toBe(true);
        await expect.poll(() => beforeFailureReconnect.closed, { timeout: 6_000 }).toBe(true);
        await expect.poll(() => socket.connections.some((connection) => connection.id > beforeFailureReconnect.id &&
          connection.sent.includes('authenticate') && connection.sent.includes('active-files:[]') &&
          connection.events.some(unavailable)),
        { timeout: 15_000 }).toBe(true);
        const failedConnection = socket.connections.find((connection) => connection.id > beforeFailureReconnect.id &&
          connection.sent.includes('authenticate') && connection.sent.includes('active-files:[]') &&
          connection.events.some(unavailable))!;
        expect(failedConnection.events[0]).toMatchObject({ asOfKind: 'unavailable', asOf: 'repository-observation-failed' });
        expect(BigInt(failedConnection.events[0].generation)).toBeGreaterThan(BigInt(failureEvent.generation));
        await expect(page.locator('#context-pane')).toContainText('Live observation unavailable: repository-observation-failed');
        expect(await page.evaluate(() =>
          (window as Window & { __p705CloseActualSocket?: () => boolean }).__p705CloseActualSocket?.() ?? false)).toBe(true);
        await expect.poll(() => failedConnection.closed, { timeout: 6_000 }).toBe(true);
        writeFileSync(configPath, originalConfig);
        restored = true;
        await expect.poll(() => socket.connections.some((connection) => connection.id > failedConnection.id &&
          connection.sent.includes('authenticate') && connection.sent.includes('active-files:[]') &&
          connection.events[0]?.kind === 'repository-invalidated' &&
          connection.events[0]?.asOfKind === 'commit' && connection.events[0]?.asOf === fixture.head &&
          JSON.stringify(connection.events[0]?.facts) === JSON.stringify(fullResyncFacts) &&
          BigInt(connection.events[0]!.generation) > BigInt(failedConnection.events[0].generation)),
        { timeout: 15_000 }).toBe(true);
      } finally {
        if (!restored) writeFileSync(configPath, originalConfig);
      }
      await refreshHead(page, fixture.head);
      await expect(page.locator('#context-pane .error')).toHaveCount(0);
      await expect(page.locator('#context-pane .notice')).toHaveCount(0);
      await fixture.assertSentinels();
      console.log(`P705_B15=authenticated:${socket.sent.filter((frame) => frame === 'authenticate').length} resyncs:${socket.events.filter((event) => event.kind === 'repository-invalidated').length} unavailable:${socket.events.filter((event) => event.asOfKind === 'unavailable').length} closes:${socket.closes}`);
    } finally {
      await context.close();
    }
  });
});
