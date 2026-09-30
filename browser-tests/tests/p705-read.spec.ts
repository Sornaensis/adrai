import { expect, test, type Page, type Response } from '@playwright/test';
import { appendFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import { readJsonWithBusyRetry, withP705Server, waitForPrimaryInspection, type DecisionRef, type P705Fixture } from '../support/p705-server';

const oid = /^[a-f0-9]{40}$/;

function escapePattern(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

async function openExplorer(page: Page, fixture: P705Fixture): Promise<void> {
  await page.goto(fixture.bootstrapUrl);
  await expect(page.getByRole('heading', { name: 'ADRAI repository explorer' })).toBeVisible();
  await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.head}`);
}

async function loadView(page: Page, path: string, query: Record<string, string> = {}, timeoutMs = 20_000): Promise<{ response: Response; body: any }> {
  const arrived = page.waitForResponse(
    response => {
      const url = new URL(response.url());
      return url.pathname === path
        && Object.entries(query).every(([key, value]) => url.searchParams.get(key) === value)
        && response.request().method() === 'GET'
        && response.status() === 200;
    },
    { timeout: timeoutMs },
  );
  await page.getByRole('button', { name: 'Load view' }).click();
  const response = await arrived;
  const body = await response.json();
  expect(body.schema).toBe('adrai/api/v1');
  expect(body.metadata?.as_of?.oid ?? body.metadata?.as_of?.to).toMatch(oid);
  return { response, body };
}

async function chooseDecision(page: Page, decision: DecisionRef, revision: string): Promise<void> {
  await page.locator('#context-pane .result-list').getByRole('button', { name: new RegExp(escapePattern(decision.title)) }).first().click();
  await waitForPrimaryInspection(page, decision, revision);
}

test('B01 bootstrap, reload and new tab keep secrets out and snapshots readable', async ({ browser }) => {
  test.setTimeout(90_000);
  await withP705Server({ scenarioId: 'B01', seed: 'main' }, async fixture => {
    const context = await browser.newContext();
    const started = Date.now();
    let phase = 'bootstrap';
    let navigation = 0;
    const requests: Array<{ navigation: number; phase: string; elapsed_ms: number; path: string; status?: number; code?: string; as_of?: string; head?: string }> = [];
    const sockets: Array<{ navigation: number; phase: string; elapsed_ms: number; sent?: string; schema?: string; generation?: string; as_of_kind?: string; as_of?: string; kind?: string; facts?: string[] }> = [];
    const responseReads: Promise<void>[] = [];
    const requestNavigations = new WeakMap<object, number>();
    const safeOid = (value: unknown) => typeof value === 'string' && oid.test(value) ? value : undefined;
    const safeCode = (value: unknown) => typeof value === 'string' && /^[a-z0-9-]{1,64}$/.test(value) ? value : undefined;
    const validBusyFailure = (body: any): boolean => {
      const generation = body.metadata?.generation;
      const asOf = body.metadata?.as_of;
      const validGeneration = typeof generation === 'string' && /^(0|[1-9][0-9]*)$/.test(generation)
        && generation.length <= 20 && BigInt(generation) <= 18446744073709551615n;
      const validAsOf = (asOf?.kind === 'commit' && typeof asOf.oid === 'string')
        || (asOf?.kind === 'comparison' && typeof asOf.from === 'string' && typeof asOf.to === 'string')
        || (asOf?.kind === 'unavailable' && typeof asOf.reason === 'string');
      return body.schema === 'adrai/api/v1' && validGeneration && validAsOf
        && body.error?.category === 'service-failure' && body.error?.status === 503
        && body.error?.code === 'repository-busy' && typeof body.error?.message === 'string';
    };
    const validSearchWindow = (body: any): boolean => Number.isInteger(body.data?.limit)
      && Array.isArray(body.data?.results)
      && body.data.results.every((hit: any) => typeof hit.adr === 'string' && typeof hit.title === 'string'
        && typeof hit.summary === 'string' && typeof hit.status === 'string'
        && Array.isArray(hit.domains) && hit.domains.every((value: any) => typeof value === 'string')
        && Array.isArray(hit.applies_to) && hit.applies_to.every((value: any) => typeof value === 'string')
        && typeof hit.state_token === 'string' && (hit.score === null || typeof hit.score === 'number'));
    const recordRequest = (fact: (typeof requests)[number]) => { if (requests.length < 64) requests.push(fact); };
    const recordSocket = (fact: (typeof sockets)[number]) => { if (sockets.length < 64) sockets.push(fact); };
    let actionFailed = false;
    try {
      const page = await context.newPage();
      const token = new URL(fixture.bootstrapUrl).searchParams.get('token');
      expect(token).toBeTruthy();
      const assetPageUrls: string[] = [];
      const pageErrors: string[] = [];
      let authenticatedEvents = 0;
      let initialRepositoryRequestCount = 0;
      let reloadRepositoryRequestCount = 0;
      let initialSearchRequestCount = 0;
      let reloadSearchRequestCount = 0;
      const initialRepositoryRequestIds = new WeakMap<object, number>();
      const reloadRepositoryRequestIds = new WeakMap<object, number>();
      const initialSearchRequestIds = new WeakMap<object, number>();
      const reloadSearchRequestIds = new WeakMap<object, number>();
      const failedInitialRepositoryRequests = new Set<number>();
      const failedReloadRepositoryRequests = new Set<number>();
      const failedInitialSearchRequests = new Set<number>();
      const failedReloadSearchRequests = new Set<number>();
      const initialRepositoryReads: Array<{ request: number; status: number; parsed: boolean; schema?: string; code?: string; as_of?: string; head?: string; validFailure?: boolean }> = [];
      const reloadRepositoryReads: typeof initialRepositoryReads = [];
      const initialSearchReads: Array<{ request: number; status: number; parsed: boolean; schema?: string; code?: string; as_of?: string; data_as_of?: string; validWindow?: boolean; validFailure?: boolean }> = [];
      const reloadSearchReads: typeof initialSearchReads = [];
      page.on('request', request => {
        const path = new URL(request.url()).pathname;
        if (['/app.js', '/app.css'].includes(path)) assetPageUrls.push(page.url());
        if (path === '/api/v1/repository' || path === '/api/v1/search') {
          requestNavigations.set(request, navigation);
          if (navigation === 0 && path === '/api/v1/repository' && request.method() === 'GET') {
            initialRepositoryRequestCount += 1;
            initialRepositoryRequestIds.set(request, initialRepositoryRequestCount);
          }
          if (navigation === 1 && path === '/api/v1/repository' && request.method() === 'GET') {
            reloadRepositoryRequestCount += 1;
            reloadRepositoryRequestIds.set(request, reloadRepositoryRequestCount);
          }
          if (navigation === 0 && path === '/api/v1/search' && request.method() === 'GET') {
            initialSearchRequestCount += 1;
            initialSearchRequestIds.set(request, initialSearchRequestCount);
          }
          if (navigation === 1 && path === '/api/v1/search' && request.method() === 'GET') {
            reloadSearchRequestCount += 1;
            reloadSearchRequestIds.set(request, reloadSearchRequestCount);
          }
          recordRequest({ navigation, phase: 'request', elapsed_ms: Date.now() - started, path });
        }
      });
      page.on('requestfailed', request => {
        const path = new URL(request.url()).pathname;
        if (path === '/api/v1/repository' || path === '/api/v1/search') {
          if (requestNavigations.get(request) === 0 && path === '/api/v1/repository') {
            failedInitialRepositoryRequests.add(initialRepositoryRequestIds.get(request) ?? 0);
          }
          if (requestNavigations.get(request) === 1 && path === '/api/v1/repository') {
            failedReloadRepositoryRequests.add(reloadRepositoryRequestIds.get(request) ?? 0);
          }
          if (requestNavigations.get(request) === 0 && path === '/api/v1/search') {
            failedInitialSearchRequests.add(initialSearchRequestIds.get(request) ?? 0);
          }
          if (requestNavigations.get(request) === 1 && path === '/api/v1/search') {
            failedReloadSearchRequests.add(reloadSearchRequestIds.get(request) ?? 0);
          }
          recordRequest({ navigation: requestNavigations.get(request) ?? navigation, phase: 'request-failed', elapsed_ms: Date.now() - started, path });
        }
      });
      page.on('response', response => {
        const path = new URL(response.url()).pathname;
        if (path !== '/api/v1/repository' && path !== '/api/v1/search') return;
        const fact: (typeof requests)[number] = { navigation: requestNavigations.get(response.request()) ?? navigation, phase: 'response', elapsed_ms: Date.now() - started, path, status: response.status() };
        recordRequest(fact);
        const initialRead = fact.navigation === 0 && path === '/api/v1/repository'
          ? { request: initialRepositoryRequestIds.get(response.request()) ?? 0, status: response.status(), parsed: false } as (typeof initialRepositoryReads)[number]
          : undefined;
        const reloadRead = fact.navigation === 1 && path === '/api/v1/repository'
          ? { request: reloadRepositoryRequestIds.get(response.request()) ?? 0, status: response.status(), parsed: false } as (typeof reloadRepositoryReads)[number]
          : undefined;
        const initialSearchRead = fact.navigation === 0 && path === '/api/v1/search'
          ? { request: initialSearchRequestIds.get(response.request()) ?? 0, status: response.status(), parsed: false } as (typeof initialSearchReads)[number]
          : undefined;
        const reloadSearchRead = fact.navigation === 1 && path === '/api/v1/search'
          ? { request: reloadSearchRequestIds.get(response.request()) ?? 0, status: response.status(), parsed: false } as (typeof reloadSearchReads)[number]
          : undefined;
        if (initialRead) initialRepositoryReads.push(initialRead);
        if (reloadRead) reloadRepositoryReads.push(reloadRead);
        if (initialSearchRead) initialSearchReads.push(initialSearchRead);
        if (reloadSearchRead) reloadSearchReads.push(reloadSearchRead);
        const pageRepositoryRead = initialRead ?? reloadRead;
        const pageSearchRead = initialSearchRead ?? reloadSearchRead;
        const read = response.json().then(body => {
          fact.code = safeCode(body.error?.code);
          fact.as_of = safeOid(body.metadata?.as_of?.oid);
          if (path === '/api/v1/repository') fact.head = safeOid(body.data?.head);
          if (pageRepositoryRead) {
            pageRepositoryRead.schema = body.schema === 'adrai/api/v1' ? body.schema : undefined;
            pageRepositoryRead.code = fact.code;
            pageRepositoryRead.as_of = fact.as_of;
            pageRepositoryRead.head = fact.head;
            pageRepositoryRead.validFailure = validBusyFailure(body);
            pageRepositoryRead.parsed = true;
          }
          if (pageSearchRead) {
            pageSearchRead.schema = body.schema === 'adrai/api/v1' ? body.schema : undefined;
            pageSearchRead.code = fact.code;
            pageSearchRead.as_of = fact.as_of;
            pageSearchRead.data_as_of = safeOid(body.data?.as_of);
            pageSearchRead.validWindow = validSearchWindow(body);
            pageSearchRead.validFailure = validBusyFailure(body);
            pageSearchRead.parsed = true;
          }
        }).catch(() => {
          fact.code = 'unreadable';
          if (pageRepositoryRead) { pageRepositoryRead.code = 'unreadable'; pageRepositoryRead.parsed = true; }
          if (pageSearchRead) { pageSearchRead.code = 'unreadable'; pageSearchRead.parsed = true; }
        });
        if (responseReads.length < 64) responseReads.push(read);
      });
      page.on('pageerror', error => pageErrors.push(error.message));
      page.on('websocket', socket => {
        const socketNavigation = navigation;
        recordSocket({ navigation: socketNavigation, phase: 'created', elapsed_ms: Date.now() - started });
        socket.on('framesent', frame => {
          try {
            const body = JSON.parse(frame.payload.toString());
            recordSocket({ navigation: socketNavigation, phase: 'sent', elapsed_ms: Date.now() - started,
              sent: body.type === 'authenticate' || body.type === 'active-files' ? body.type : 'other' });
          } catch { recordSocket({ navigation: socketNavigation, phase: 'sent-unreadable', elapsed_ms: Date.now() - started }); }
        });
        socket.on('framereceived', frame => {
          try {
            const body = JSON.parse(frame.payload.toString());
            recordSocket({ navigation: socketNavigation, phase: 'received', elapsed_ms: Date.now() - started,
              schema: body.schema === 'adrai/events/v1' ? body.schema : 'other',
              generation: typeof body.generation === 'string' && /^\d{1,20}$/.test(body.generation) ? body.generation : undefined,
              as_of_kind: safeCode(body.as_of?.kind), as_of: safeOid(body.as_of?.oid), kind: safeCode(body.event?.type),
              facts: Array.isArray(body.event?.facts) ? body.event.facts.slice(0, 32).map(safeCode).filter((fact: string | undefined): fact is string => !!fact) : undefined });
            if (body.schema === 'adrai/events/v1') authenticatedEvents += 1;
          } catch { recordSocket({ navigation: socketNavigation, phase: 'received-unreadable', elapsed_ms: Date.now() - started }); }
        });
        socket.on('close', () => recordSocket({ navigation: socketNavigation, phase: 'closed', elapsed_ms: Date.now() - started }));
        socket.on('socketerror', () => recordSocket({ navigation: socketNavigation, phase: 'error', elapsed_ms: Date.now() - started }));
      });
      const pageRepositoryWave = async (which: 'initial' | 'reload', firstRequest: number): Promise<'ready' | 'exhausted'> => {
        const count = () => which === 'initial' ? initialRepositoryRequestCount : reloadRepositoryRequestCount;
        const failed = which === 'initial' ? failedInitialRepositoryRequests : failedReloadRepositoryRequests;
        const observed = which === 'initial' ? initialRepositoryReads : reloadRepositoryReads;
        const outcome = () => {
          if (count() > firstRequest + 2) return `unexpected-extra-${which}-get`;
          if ([...failed].some(request => request >= firstRequest)) return `${which}-request-failed`;
          const reads = observed.filter(read => read.request >= firstRequest);
          for (const read of reads.filter(read => read.parsed)) {
            if (read.status === 200) {
              if (read.schema !== 'adrai/api/v1' || read.head !== fixture.head || read.as_of !== fixture.head) {
                return `invalid-${which}-repository-snapshot`;
              }
            } else if (read.status !== 503 || read.code !== 'repository-busy' || !read.validFailure) {
              return `invalid-${which}-repository-error`;
            }
          }
          if (reads.length !== count() - firstRequest + 1 || reads.some(read => !read.parsed)) return 'waiting';
          if (reads.some(read => read.status === 200)) return 'ready';
          if (reads.length === 3) return 'exhausted';
          return 'waiting';
        };
        await expect.poll(outcome, { timeout: 10_000 }).not.toBe('waiting');
        const result = outcome();
        if (result !== 'ready' && result !== 'exhausted') throw new Error(result);
        return result;
      };
      const pageSearchWave = async (which: 'initial' | 'reload', firstRequest: number, maximumRequests = 3): Promise<'ready' | 'exhausted'> => {
        const count = () => which === 'initial' ? initialSearchRequestCount : reloadSearchRequestCount;
        const failed = which === 'initial' ? failedInitialSearchRequests : failedReloadSearchRequests;
        const observed = which === 'initial' ? initialSearchReads : reloadSearchReads;
        const outcome = () => {
          if (count() > firstRequest + maximumRequests - 1) return `unexpected-extra-${which}-search-get`;
          if ([...failed].some(request => request >= firstRequest)) return `${which}-search-request-failed`;
          const reads = observed.filter(read => read.request >= firstRequest);
          for (const read of reads.filter(read => read.parsed)) {
            if (read.status === 200) {
              if (read.schema !== 'adrai/api/v1' || read.as_of !== fixture.head
                || read.data_as_of !== fixture.head || !read.validWindow) return `invalid-${which}-search-window`;
            } else if (read.status !== 503 || read.code !== 'repository-busy' || !read.validFailure) {
              return `invalid-${which}-search-error`;
            }
          }
          if (reads.length !== count() - firstRequest + 1 || reads.some(read => !read.parsed)) return 'waiting';
          if (reads.some(read => read.status === 200)) return 'ready';
          if (reads.length === maximumRequests) return 'exhausted';
          return 'waiting';
        };
        await expect.poll(outcome, { timeout: 10_000 }).not.toBe('waiting');
        const result = outcome();
        if (result !== 'ready' && result !== 'exhausted') throw new Error(result);
        return result;
      };
      await page.goto(fixture.bootstrapUrl);
      await expect(page.getByRole('heading', { name: 'ADRAI repository explorer' })).toBeVisible();
      let initialRepositoryState = await pageRepositoryWave('initial', 1);
      let initialSearchWaveStart = 1;
      for (let refresh = 0; initialRepositoryState === 'exhausted' && refresh < 3; refresh += 1) {
        expect(initialRepositoryRequestCount).toBe(3 * (refresh + 1));
        const nextRequest = initialRepositoryRequestCount + 1;
        initialSearchWaveStart = initialSearchRequestCount + 1;
        await page.getByRole('button', { name: 'Refresh repository' }).click();
        initialRepositoryState = await pageRepositoryWave('initial', nextRequest);
      }
      expect(initialRepositoryState).toBe('ready');
      await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.head}`);
      phase = 'initial-visible';
      await expect(page.locator('#context-pane')).toBeVisible();
      await expect(page.locator('#inspector-pane')).toBeVisible();
      await expect(page.locator('#actions-pane')).toBeVisible();
      expect(page.url()).toBe(`${fixture.origin}/`);
      expect(assetPageUrls.length).toBeGreaterThanOrEqual(2);
      expect(assetPageUrls.every(url => !url.includes('token='))).toBe(true);
      const searchBeforeResync = initialSearchRequestCount;
      await expect.poll(() => authenticatedEvents, { timeout: 20_000 }).toBeGreaterThan(0);
      const fullResyncFacts = [
        'repository-identity', 'head', 'index', 'sequencer', 'configuration', 'managed-source',
        'common-refs', 'packed-refs', 'reflogs', 'worktree-metadata', 'relevant-worktree-file',
      ];
      await expect.poll(() => sockets.some(event => event.navigation === 0 && event.phase === 'received'
        && event.schema === 'adrai/events/v1' && event.generation !== undefined
        && event.as_of_kind === 'commit' && event.as_of === fixture.head
        && event.kind === 'repository-invalidated'
        && JSON.stringify(event.facts) === JSON.stringify(fullResyncFacts)), { timeout: 20_000 }).toBe(true);
      if (initialSearchRequestCount > searchBeforeResync) initialSearchWaveStart = searchBeforeResync + 1;
      let initialSearchState = await pageSearchWave('initial', initialSearchWaveStart);
      for (let load = 0; initialSearchState === 'exhausted' && load < 3; load += 1) {
        const nextRequest = initialSearchRequestCount + 1;
        await page.getByRole('button', { name: 'Load view' }).click();
        initialSearchState = await pageSearchWave('initial', nextRequest, 1);
      }
      expect(initialSearchState).toBe('ready');
      await expect(page.locator('#context-pane p.meta').filter({ hasText: `At ${fixture.head} ·` })).toBeVisible();
      await expect(page.getByText('Snapshot is loading or stale.')).toHaveCount(0);
      expect((await context.cookies(fixture.origin)).some(cookie => cookie.httpOnly)).toBe(true);
      expect(await page.locator('body').innerText()).not.toContain(token!);
      expect(pageErrors.join(' ')).not.toContain(token!);
      expect(await page.evaluate(() => [
        ...Object.keys(localStorage).map(key => `${key}=${localStorage.getItem(key)}`),
        ...Object.keys(sessionStorage).map(key => `${key}=${sessionStorage.getItem(key)}`),
      ].join(' '))).not.toContain(token!);

      let reloadSockets = 0;
      page.on('websocket', () => { reloadSockets += 1; });
      phase = 'reload';
      navigation += 1;
      await page.reload();
      await expect(page.getByText(/Read-only session\. Reopen the process bootstrap URL/)).toBeVisible();
      let reloadRepositoryState = await pageRepositoryWave('reload', 1);
      let reloadSearchWaveStart = 1;
      for (let refresh = 0; reloadRepositoryState === 'exhausted' && refresh < 3; refresh += 1) {
        expect(reloadRepositoryRequestCount).toBe(3 * (refresh + 1));
        const nextRequest = reloadRepositoryRequestCount + 1;
        reloadSearchWaveStart = reloadSearchRequestCount + 1;
        await page.getByRole('button', { name: 'Refresh repository' }).click();
        reloadRepositoryState = await pageRepositoryWave('reload', nextRequest);
      }
      expect(reloadRepositoryState).toBe('ready');
      let reloadSearchState = await pageSearchWave('reload', reloadSearchWaveStart);
      for (let load = 0; reloadSearchState === 'exhausted' && load < 3; load += 1) {
        const nextRequest = reloadSearchRequestCount + 1;
        await page.getByRole('button', { name: 'Load view' }).click();
        reloadSearchState = await pageSearchWave('reload', nextRequest, 1);
      }
      expect(reloadSearchState).toBe('ready');
      await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.head}`);
      await expect(page.locator('#context-pane p.meta').filter({ hasText: `At ${fixture.head} ·` })).toBeVisible();
      await expect(page.getByText('Snapshot is loading or stale.')).toHaveCount(0);
      phase = 'reload-visible';
      await expect(page.getByRole('button', { name: 'Submit create' })).toBeDisabled();
      await page.waitForTimeout(750);
      expect(reloadSockets).toBe(0);
      expect(page.url()).toBe(`${fixture.origin}/`);

      const newTab = await context.newPage();
      phase = 'new-tab';
      navigation += 1;
      let newTabSockets = 0;
      let repositoryRequestCount = 0;
      let searchRequestCount = 0;
      const repositoryRequestIds = new WeakMap<object, number>();
      const searchRequestIds = new WeakMap<object, number>();
      const failedRepositoryRequests = new Set<number>();
      const failedSearchRequests = new Set<number>();
      const repositoryReads: Array<{ request: number; status: number; parsed: boolean; schema?: string; code?: string; as_of?: string; head?: string; validFailure?: boolean }> = [];
      const searchReads: Array<{ request: number; status: number; parsed: boolean; schema?: string; code?: string; as_of?: string; data_as_of?: string; validWindow?: boolean; validFailure?: boolean }> = [];
      newTab.on('websocket', () => { newTabSockets += 1; });
      newTab.on('request', request => {
        const path = new URL(request.url()).pathname;
        if (path === '/api/v1/repository' || path === '/api/v1/search') {
          if (path === '/api/v1/repository' && request.method() === 'GET') {
            repositoryRequestCount += 1;
            repositoryRequestIds.set(request, repositoryRequestCount);
          }
          if (path === '/api/v1/search' && request.method() === 'GET') {
            searchRequestCount += 1;
            searchRequestIds.set(request, searchRequestCount);
          }
          recordRequest({ navigation: 2, phase: 'request', elapsed_ms: Date.now() - started, path });
        }
      });
      newTab.on('requestfailed', request => {
        const path = new URL(request.url()).pathname;
        if (path === '/api/v1/repository' || path === '/api/v1/search') {
          if (path === '/api/v1/repository') failedRepositoryRequests.add(repositoryRequestIds.get(request) ?? 0);
          if (path === '/api/v1/search') failedSearchRequests.add(searchRequestIds.get(request) ?? 0);
          recordRequest({ navigation: 2, phase: 'request-failed', elapsed_ms: Date.now() - started, path });
        }
      });
      newTab.on('response', response => {
        const path = new URL(response.url()).pathname;
        if (path !== '/api/v1/repository' && path !== '/api/v1/search') return;
        const fact: (typeof requests)[number] = { navigation: 2, phase: 'response', elapsed_ms: Date.now() - started, path, status: response.status() };
        recordRequest(fact);
        const repositoryRead = path === '/api/v1/repository'
          ? { request: repositoryRequestIds.get(response.request()) ?? 0, status: response.status(), parsed: false } as (typeof repositoryReads)[number]
          : undefined;
        const searchRead = path === '/api/v1/search'
          ? { request: searchRequestIds.get(response.request()) ?? 0, status: response.status(), parsed: false } as (typeof searchReads)[number]
          : undefined;
        if (repositoryRead) repositoryReads.push(repositoryRead);
        if (searchRead) searchReads.push(searchRead);
        const read = response.json().then(body => {
          fact.code = safeCode(body.error?.code);
          fact.as_of = safeOid(body.metadata?.as_of?.oid);
          if (path === '/api/v1/repository') fact.head = safeOid(body.data?.head);
          if (repositoryRead) {
            repositoryRead.schema = body.schema === 'adrai/api/v1' ? body.schema : undefined;
            repositoryRead.code = fact.code;
            repositoryRead.as_of = fact.as_of;
            repositoryRead.head = fact.head;
            repositoryRead.validFailure = validBusyFailure(body);
            repositoryRead.parsed = true;
          }
          if (searchRead) {
            searchRead.schema = body.schema === 'adrai/api/v1' ? body.schema : undefined;
            searchRead.code = fact.code;
            searchRead.as_of = fact.as_of;
            searchRead.data_as_of = safeOid(body.data?.as_of);
            searchRead.validWindow = validSearchWindow(body);
            searchRead.validFailure = validBusyFailure(body);
            searchRead.parsed = true;
          }
        }).catch(() => {
          fact.code = 'unreadable';
          if (repositoryRead) { repositoryRead.code = 'unreadable'; repositoryRead.parsed = true; }
          if (searchRead) { searchRead.code = 'unreadable'; searchRead.parsed = true; }
        });
        if (responseReads.length < 64) responseReads.push(read);
      });
      const repositoryWave = async (firstRequest: number): Promise<'ready' | 'exhausted'> => {
        const outcome = () => {
          if (repositoryRequestCount > firstRequest + 2) return 'unexpected-extra-get';
          if ([...failedRepositoryRequests].some(request => request >= firstRequest)) return 'repository-request-failed';
          const reads = repositoryReads.filter(read => read.request >= firstRequest);
          for (const read of reads.filter(read => read.parsed)) {
            if (read.status === 200) {
              if (read.schema !== 'adrai/api/v1' || read.head !== fixture.head || read.as_of !== fixture.head) {
                return 'invalid-repository-snapshot';
              }
            } else if (read.status !== 503 || read.code !== 'repository-busy' || !read.validFailure) {
              return 'invalid-repository-error';
            }
          }
          if (reads.length !== repositoryRequestCount - firstRequest + 1 || reads.some(read => !read.parsed)) return 'waiting';
          if (reads.some(read => read.parsed && read.status === 200)) return 'ready';
          if (reads.length === 3 && reads.every(read => read.parsed)) return 'exhausted';
          return 'waiting';
        };
        await expect.poll(outcome, { timeout: 10_000 }).not.toBe('waiting');
        const result = outcome();
        if (result !== 'ready' && result !== 'exhausted') throw new Error(result);
        return result;
      };
      const searchWave = async (firstRequest: number, maximumRequests = 3): Promise<'ready' | 'exhausted'> => {
        const outcome = () => {
          if (searchRequestCount > firstRequest + maximumRequests - 1) return 'unexpected-extra-search-get';
          if ([...failedSearchRequests].some(request => request >= firstRequest)) return 'search-request-failed';
          const reads = searchReads.filter(read => read.request >= firstRequest);
          for (const read of reads.filter(read => read.parsed)) {
            if (read.status === 200) {
              if (read.schema !== 'adrai/api/v1' || read.as_of !== fixture.head
                || read.data_as_of !== fixture.head || !read.validWindow) return 'invalid-search-window';
            } else if (read.status !== 503 || read.code !== 'repository-busy' || !read.validFailure) {
              return 'invalid-search-error';
            }
          }
          if (reads.length !== searchRequestCount - firstRequest + 1 || reads.some(read => !read.parsed)) return 'waiting';
          if (reads.some(read => read.status === 200)) return 'ready';
          if (reads.length === maximumRequests) return 'exhausted';
          return 'waiting';
        };
        await expect.poll(outcome, { timeout: 10_000 }).not.toBe('waiting');
        const result = outcome();
        if (result !== 'ready' && result !== 'exhausted') throw new Error(result);
        return result;
      };
      await newTab.goto(fixture.origin);
      await expect(newTab.getByText(/Read-only session\. Reopen the process bootstrap URL/)).toBeVisible();
      let repositoryState = await repositoryWave(1);
      let searchState = await searchWave(1);
      for (let refresh = 0; repositoryState === 'exhausted' && refresh < 3; refresh += 1) {
        expect(repositoryRequestCount).toBe(3 * (refresh + 1));
        const nextRepositoryRequest = repositoryRequestCount + 1;
        const nextSearchRequest = searchRequestCount + 1;
        await newTab.getByRole('button', { name: 'Refresh repository' }).click();
        [repositoryState, searchState] = await Promise.all([
          repositoryWave(nextRepositoryRequest), searchWave(nextSearchRequest),
        ]);
      }
      expect(repositoryState).toBe('ready');
      const completedRepositoryRequests = repositoryRequestCount;
      for (let load = 0; searchState === 'exhausted' && load < 3; load += 1) {
        const nextSearchRequest = searchRequestCount + 1;
        await newTab.getByRole('button', { name: 'Load view' }).click();
        searchState = await searchWave(nextSearchRequest, 1);
      }
      expect(searchState).toBe('ready');
      const completedSearchRequests = searchRequestCount;
      await expect(newTab.locator('.masthead')).toContainText(`HEAD ${fixture.head}`);
      await expect(newTab.locator('#context-pane')).toBeVisible();
      await expect(newTab.locator('#inspector-pane')).toBeVisible();
      await expect(newTab.locator('#actions-pane')).toBeVisible();
      await expect(newTab.locator('#context-pane p.meta').filter({ hasText: `At ${fixture.head} ·` })).toBeVisible();
      await expect(newTab.getByText('Snapshot is loading or stale.')).toHaveCount(0);
      phase = 'new-tab-visible';
      await newTab.waitForTimeout(750);
      expect(repositoryRequestCount).toBe(completedRepositoryRequests);
      expect(searchRequestCount).toBe(completedSearchRequests);
      expect(newTabSockets).toBe(0);
      expect(await newTab.locator('body').innerText()).not.toContain(token!);
      await fixture.assertSentinels();
    } catch (error) {
      actionFailed = true;
      throw error;
    } finally {
      try {
        await Promise.race([Promise.allSettled(responseReads), new Promise(resolve => setTimeout(resolve, 500))]);
        if (process.env.P705_EVIDENCE_DIR) writeFileSync(join(process.env.P705_EVIDENCE_DIR, 'b01-safe-diagnostic.json'), JSON.stringify({
          phase, elapsed_ms: Date.now() - started, authenticated_events: sockets.filter(event => event.phase === 'received' && event.schema === 'adrai/events/v1').length,
          requests, sockets,
        }));
      } catch (error) {
        if (!actionFailed) throw error;
      } finally {
        await context.close();
      }
    }
  });
});

test('B02 linked authority and historical selection retain exact read-only context', async ({ browser }) => {
  test.setTimeout(45_000);
  await withP705Server({ scenarioId: 'B02', seed: 'linked' }, async fixture => {
    expect(fixture.linkedRepository).toBeTruthy();
    expect(fixture.repository).toBe(fixture.linkedRepository);
    expect(fixture.baseHead).toMatch(oid);
    expect(fixture.head).toMatch(oid);
    expect(fixture.baseHead).not.toBe(fixture.head);
    const context = await browser.newContext();
    try {
      const page = await context.newPage();
      const repositoryResponse = page.waitForResponse(response => new URL(response.url()).pathname === '/api/v1/repository' && response.status() === 200);
      await openExplorer(page, fixture);
      const repository = await (await repositoryResponse).json();
      expect(repository.data.head).toBe(fixture.head);
      expect(repository.data.head_ref).toBeTruthy();
      await expect(page.locator('.masthead')).toContainText(repository.data.head_ref);
      expect(await fixture.currentHead()).toBe(fixture.head);
      expect(await fixture.currentHead(fixture.mainRepository)).toBe(fixture.baseHead);

      await page.getByLabel('Revision (HEAD or exact commit)').fill(fixture.baseHead);
      await loadView(page, '/api/v1/search', { at: fixture.baseHead });
      await expect(page.locator('#context-pane')).toContainText(`At ${fixture.baseHead}`);
      await expect(page.getByText(/Historical inspection is read-only/)).toBeVisible();
      await expect(page.getByLabel('Action')).toBeDisabled();
      await expect(page.getByRole('button', { name: 'Review repository and adopt current basis' })).toBeDisabled();

      await page.getByLabel('Revision (HEAD or exact commit)').fill('HEAD');
      await loadView(page, '/api/v1/search', { at: 'HEAD' });
      await expect(page.getByText(/Historical inspection is read-only/)).toHaveCount(0);
      await chooseDecision(page, fixture.decisions.primary, fixture.head);
      await expect(page.getByLabel('Action')).toBeEnabled();
      await fixture.assertSentinels();
    } finally {
      await context.close();
    }
  });
});

test('B03 bounded browse pages and live search modes preserve one exact window', async ({ browser }) => {
  test.setTimeout(90_000);
  await withP705Server({ scenarioId: 'B03', seed: 'paging' }, async fixture => {
    const context = await browser.newContext();
    try {
      const page = await context.newPage();
      await openExplorer(page, fixture);
      let searchRequests = 0;
      const started = Date.now();
      let nextSearchId = 0;
      const requestIds = new WeakMap<object, string>();
      const searchFacts: Array<{ id: string; phase: string; elapsed_ms: number; limit: string | null; mode: string | null; at: string | null; status?: number; code?: string; as_of?: string; results?: number; window_limit?: number }> = [];
      const eventFacts: Array<{ elapsed_ms: number; generation: string; as_of?: string; kind: string; facts: string[] }> = [];
      page.on('request', request => {
        if (new URL(request.url()).pathname === '/api/v1/search' && request.method() === 'GET') {
          const url = new URL(request.url());
          const id = `read-${++nextSearchId}`;
          requestIds.set(request, id);
          searchRequests += 1;
          searchFacts.push({ id, phase: 'request', elapsed_ms: Date.now() - started, limit: url.searchParams.get('limit'), mode: url.searchParams.get('mode'), at: url.searchParams.get('at') });
        }
      });
      page.on('requestfailed', request => {
        if (new URL(request.url()).pathname === '/api/v1/search') {
          const url = new URL(request.url());
          searchFacts.push({ id: requestIds.get(request) ?? 'unknown', phase: 'request-failed', elapsed_ms: Date.now() - started, limit: url.searchParams.get('limit'), mode: url.searchParams.get('mode'), at: url.searchParams.get('at') });
        }
      });
      page.on('response', response => {
        if (new URL(response.url()).pathname !== '/api/v1/search') return;
        const url = new URL(response.url());
        const fact: (typeof searchFacts)[number] = {
          id: requestIds.get(response.request()) ?? 'unknown', phase: 'response', elapsed_ms: Date.now() - started,
          limit: url.searchParams.get('limit'), mode: url.searchParams.get('mode'), at: url.searchParams.get('at'), status: response.status(),
        };
        searchFacts.push(fact);
        void response.json().then(body => {
          fact.code = body.error?.code;
          fact.as_of = body.metadata?.as_of?.oid;
          fact.results = Array.isArray(body.data?.results) ? body.data.results.length : undefined;
          fact.window_limit = body.data?.limit;
        }).catch(() => { fact.code = 'undecodable'; });
      });
      page.on('websocket', socket => socket.on('framereceived', frame => {
        try {
          const body = JSON.parse(frame.payload.toString());
          if (body.schema === 'adrai/events/v1') {
            eventFacts.push({ elapsed_ms: Date.now() - started, generation: body.generation, as_of: body.as_of?.oid, kind: body.event?.type, facts: body.event?.facts ?? [] });
          }
        } catch { /* Ignore non-event frames without logging credentials. */ }
      }));
      const diagnose = async () => {
        const pane = page.locator('#context-pane');
        const windowText = await pane.locator('p.meta').filter({ hasText: /^At [a-f0-9]{40}/ }).first().textContent().catch(() => null);
        const match = windowText?.match(/^At ([a-f0-9]{40}) · (\d+) results/);
        const pagerText = await pane.locator('.pager span').first().textContent().catch(() => null);
        console.log(`P705_B03_SAFE_DIAGNOSTIC=${JSON.stringify({
          elapsed_ms: Date.now() - started, search: searchFacts, events: eventFacts,
          pane: { at: match?.[1], advertised_results: match ? Number(match[2]) : undefined,
            rendered_rows: await pane.locator('.result-list > li').count(),
            limit_input: await page.getByLabel('Window size (1–1000)').inputValue(),
            pager: pagerText, full_notice: await page.getByText('Result window full; more may exist.').isVisible(),
            stale_notice: await pane.getByText('Snapshot is loading or stale.').isVisible(), error_count: await pane.locator('.error').count() },
        })}`);
      };
      await page.getByLabel('Window size (1–1000)').fill('1000');
      let browse: { response: Response; body: any };
      try {
        browse = await loadView(page, '/api/v1/search', { q: '', limit: '1000' }, 35_000);
      } catch (error) {
        await page.waitForTimeout(200);
        await diagnose();
        throw error;
      }
      const browseUrl = new URL(browse.response.url());
      expect(browseUrl.searchParams.get('q')).toBe('');
      expect(browseUrl.searchParams.get('limit')).toBe('1000');
      expect(browse.body.data.results).toHaveLength(1000);
      expect(new Set(browse.body.data.results.map((hit: { adr: string }) => hit.adr)).size).toBe(1000);
      expect(browse.body.metadata.as_of.oid).toBe(fixture.head);
      try {
        await expect(page.locator('#context-pane p.meta').filter({ hasText: `At ${fixture.head} · 1000 results` })).toBeVisible({ timeout: 15_000 });
        await expect(page.getByText('Result window full; more may exist.')).toBeVisible();
      } catch (error) {
        await diagnose();
        throw error;
      }
      await expect(page.locator('#context-pane .pager')).toContainText('Page 1 of 10');
      const beforePaging = searchRequests;
      const rendered = new Set<string>();
      for (let index = 0; index < 10; index += 1) {
        await expect(page.locator('#context-pane .pager')).toContainText(`Page ${index + 1} of 10`);
        const names = await page.locator('#context-pane .result-list button').allTextContents();
        expect(names).toHaveLength(100);
        for (const name of names) {
          expect(rendered.has(name)).toBe(false);
          rendered.add(name);
        }
        if (index < 9) await page.getByRole('button', { name: 'Next page' }).click();
      }
      expect(rendered.size).toBe(1000);
      expect(searchRequests).toBe(beforePaging);

      await page.getByRole('button', { name: 'Search', exact: true }).click();
      await page.getByLabel('Window size (1–1000)').fill('20');
      await page.getByLabel('Search terms').fill(fixture.decisions.primary.title);
      for (const mode of ['hybrid', 'fts', 'vector']) {
        await page.getByLabel('Retrieval mode').selectOption(mode);
        if (mode === 'vector') {
          await page.getByLabel('Result view').selectOption('exploded');
          await page.getByLabel('Domain filter').fill(fixture.search.domain);
          await page.getByLabel('File scope filter').fill(fixture.search.file);
          await page.getByLabel('Actor (kind:identifier)').fill(fixture.search.actor);
          await page.getByLabel('Since (Unix milliseconds)').fill('0');
          await page.getByLabel('Until (Unix milliseconds)').fill('9999999999999');
          await page.getByLabel('Include obsolete').check();
          await page.getByLabel('Shallow history').check();
        }
        const result = await loadView(page, '/api/v1/search', mode === 'vector'
          ? { q: fixture.decisions.primary.title, mode, view: 'exploded', domain: fixture.search.domain, file: fixture.search.file, actor: fixture.search.actor }
          : { q: fixture.decisions.primary.title, mode });
        const url = new URL(result.response.url());
        expect(url.searchParams.get('mode')).toBe(mode);
        expect(url.searchParams.get('q')).toBe(fixture.decisions.primary.title);
        expect(result.body.data.results.length).toBeGreaterThan(0);
        await expect(page.locator('#context-pane .result-list button').first()).toBeVisible();
        expect(result.body.data.results.some((hit: { matches?: { fields?: string[] }; score?: number | null }) =>
          (hit.matches?.fields ?? []).length > 0 || typeof hit.score === 'number')).toBe(true);
        if (mode === 'vector') {
          for (const [key, value] of Object.entries({ view: 'exploded', domain: fixture.search.domain, file: fixture.search.file, actor: fixture.search.actor, since: '0', until: '9999999999999', include_obsolete: 'true', shallow: 'true' })) {
            expect(url.searchParams.get(key)).toBe(value);
          }
        }
      }
      await page.getByLabel('Result view').selectOption('collapsed');
      await fixture.assertSentinels();
    } finally {
      await context.close();
    }
  });
});

test('B04 committed and worktree relevance replace live file interests', async ({ browser }) => {
  test.setTimeout(90_000);
  for (const seed of ['main', 'linked'] as const) {
    await withP705Server({ scenarioId: 'B04', seed }, async fixture => {
      const context = await browser.newContext();
      try {
        const page = await context.newPage();
        const interests: string[][] = [];
        const invalidations: Array<{ facts: string[]; generation: string }> = [];
        page.on('websocket', socket => socket.on('framesent', frame => {
          try {
            const payload = JSON.parse(frame.payload.toString());
            if (payload.type === 'active-files') interests.push(payload.paths);
          } catch { /* Authentication frame has a different shape. */ }
        }));
        page.on('websocket', socket => socket.on('framereceived', frame => {
          try {
            const payload = JSON.parse(frame.payload.toString());
            if (payload.schema === 'adrai/events/v1' && payload.event?.type === 'repository-invalidated') {
              invalidations.push({ facts: payload.event.facts ?? [], generation: payload.generation });
            }
          } catch { /* A non-event frame is not invalidation evidence. */ }
        }));
        await openExplorer(page, fixture);
        const expected = fixture.decisions.relevance;
        expect(expected, `Missing real source-matching decision in ${seed} fixture`).toBeTruthy();
        await page.getByRole('button', { name: 'Relevant' }).click();
        const source = fixture.search.file;
        await page.getByLabel('Repository-relative source file').fill(source);
        const committed = await loadView(page, '/api/v1/relevant', { file: source, worktree: 'false' });
        expect(committed.body.data.file.path).toBe(source);
        expect(committed.body.data.file.source).toBe('revision');
        expect(committed.body.metadata.as_of.oid).toBe(fixture.head);
        const committedHit = committed.body.data.results.find((hit: { adr: string }) => hit.adr === expected!.adr);
        expect(committedHit).toBeTruthy();
        expect(committedHit.scope_match).not.toBe('none');
        expect(committedHit.evidence.length).toBeGreaterThan(0);
        await expect(page.locator('#context-pane')).toContainText(`Source: ${source} · revision`);
        const committedRow = page.locator('#context-pane .result-list > li').filter({ hasText: expected!.adr });
        await expect(committedRow).toContainText(`Declared applicability: ${committedHit.scope_match}`);
        await expect(committedRow).toContainText(`Semantic evidence: ${committedHit.confidence}`);
        await expect(committedRow).toContainText(committedHit.evidence[0].file_excerpt);

        await page.getByLabel('Use worktree source').check();
        const worktree = await loadView(page, '/api/v1/relevant', { file: source, worktree: 'true' });
        expect(worktree.body.data.file.path).toBe(source);
        expect(worktree.body.data.file.source).toBe('worktree');
        expect(worktree.body.metadata.as_of.oid).toBe(fixture.head);
        expect(worktree.body.data.file.digest).not.toBe(committed.body.data.file.digest);
        const worktreeHit = worktree.body.data.results.find((hit: { adr: string }) => hit.adr === expected!.adr);
        expect(worktreeHit).toBeTruthy();
        expect(worktreeHit.scope_match).not.toBe('none');
        expect(worktreeHit.evidence.length).toBeGreaterThan(0);
        await expect(page.locator('#context-pane')).toContainText(`Source: ${source} · worktree`);
        const worktreeRow = page.locator('#context-pane .result-list > li').filter({ hasText: expected!.adr });
        await expect(worktreeRow).toContainText(`Declared applicability: ${worktreeHit.scope_match}`);
        await expect(worktreeRow).toContainText(`Semantic evidence: ${worktreeHit.confidence}`);
        await expect(worktreeRow).toContainText(worktreeHit.evidence[0].file_excerpt);
        await expect.poll(() => interests.some(paths => paths.length === 1 && paths[0] === source)).toBe(true);

        let quiet = false;
        for (let check = 0; check < 6; check += 1) {
          const count = invalidations.length;
          await page.waitForTimeout(500);
          if (invalidations.length === count) { quiet = true; break; }
        }
        expect(quiet, `Prior ${seed} interest events did not settle`).toBe(true);
        const beforeEdit = invalidations.length;
        try {
          appendFileSync(join(fixture.repository, source), '\n// external relevant source edit\n');
          await expect.poll(() => invalidations.slice(beforeEdit).some(event => event.facts.includes('relevant-worktree-file')), { timeout: 10_000 }).toBe(true);
        } finally {
          writeFileSync(join(fixture.repository, source), fixture.sentinels.unstaged.bytes);
        }

        const replacement = 'seed.txt';
        await page.getByLabel('Repository-relative source file').fill(replacement);
        await loadView(page, '/api/v1/relevant', { file: replacement, worktree: 'true' });
        await expect.poll(() => interests.some(paths => paths.length === 1 && paths[0] === replacement)).toBe(true);
        const beforeBrowse = interests.length;
        await page.getByRole('button', { name: 'Browse', exact: true }).click();
        await expect.poll(() => interests.slice(beforeBrowse).some(paths => paths.length === 0)).toBe(true);
        await fixture.assertSentinels();
      } finally {
        await context.close();
      }
    });
  }
});

test('B05 primary inspection, conflict candidates and read navigation stay accessible', async ({ browser }) => {
  test.setTimeout(90_000);
  await withP705Server({ scenarioId: 'B05', seed: 'conflicts' }, async fixture => {
    const context = await browser.newContext();
    try {
      const page = await context.newPage();
      await openExplorer(page, fixture);
      const primary = fixture.decisions.primary;
      const primaryPath = `/api/v1/adrs/${encodeURIComponent(primary.adr)}?at=${fixture.head}`;
      const primaryCollapsed = (await readJsonWithBusyRetry(page, `${fixture.origin}${primaryPath}&view=collapsed`, { expectedOid: fixture.head })).body.data;
      const primaryExploded = (await readJsonWithBusyRetry(page, `${fixture.origin}${primaryPath}&view=exploded`, { expectedOid: fixture.head })).body.data;
      expect(primaryCollapsed.title).toBe(primary.title);
      expect(primaryCollapsed.summary).toBe(primary.summary);
      expect(primaryCollapsed.body).toBe(primary.body.endsWith('\n') ? primary.body.slice(0, -1) : primary.body);
      expect(primaryExploded.operations.length).toBeGreaterThan(0);
      const primaryProvenance = Object.values(primaryCollapsed.provenance ?? {}).filter(Boolean) as Array<{
        actor: string; claimed_at: string; basis: string; operation: string;
        introductions: string[]; original_commits: string[];
      }>;
      expect(primaryProvenance.length).toBeGreaterThan(0);
      type FullProvenance = { actor: string; operation: string; placements: Array<{ classification: string; commit: string; subject: string }> };
      const operationProvenance = primaryExploded.operations.map((operation: { provenance?: FullProvenance }) => operation.provenance).filter(Boolean) as FullProvenance[];
      expect(operationProvenance.some(value => value.placements.length > 0)).toBe(true);
      await chooseDecision(page, fixture.decisions.primary, fixture.head);
      const inspector = page.locator('#inspector-pane');
      await expect(inspector.locator('h3')).toHaveText(primaryCollapsed.title);
      await expect(inspector.locator('h3 + p + p')).toHaveText(primaryCollapsed.summary);
      expect(await inspector.locator('h4:has-text("Decision body") + pre.body-text').textContent()).toBe(primaryCollapsed.body);
      for (const path of primaryCollapsed.source_paths) await expect(inspector).toContainText(path);
      for (const provenance of primaryProvenance) {
        await expect(inspector).toContainText(`Actor: ${provenance.actor}`);
        await expect(inspector).toContainText(`Claimed: ${provenance.claimed_at}`);
        await expect(inspector).toContainText(`Basis: ${provenance.basis}`);
        await expect(inspector).toContainText(`Operation: ${provenance.operation}`);
        await expect(inspector).toContainText(`Introductions: ${provenance.introductions.join(', ')}`);
        await expect(inspector).toContainText(`Original operation commits: ${provenance.original_commits.join(', ')}`);
      }
      for (const operation of primaryExploded.operations) {
        await expect(inspector.getByRole('heading', { name: `Operation ${operation.operation}` })).toBeVisible();
      }
      for (const provenance of operationProvenance) {
        await expect(inspector).toContainText(`Actor: ${provenance.actor}`);
        await expect(inspector).toContainText(`Operation: ${provenance.operation}`);
        for (const placement of provenance.placements) {
          await expect(inspector).toContainText(`${placement.classification} · ${placement.commit} · ${placement.subject}`);
        }
      }
      expect(await page.locator('#inspector-pane script').count()).toBe(0);

      const safeText = fixture.decisions.safeText;
      expect(safeText).toBeTruthy();
      await page.getByRole('button', { name: 'Search', exact: true }).click();
      await page.getByLabel('Search terms').fill(safeText!.title);
      await loadView(page, '/api/v1/search', { q: safeText!.title });
      const safePath = `/api/v1/adrs/${encodeURIComponent(safeText!.adr)}?at=${fixture.head}&view=collapsed`;
      const safeCollapsed = (await readJsonWithBusyRetry(page, `${fixture.origin}${safePath}`, { expectedOid: fixture.head })).body.data;
      expect(safeCollapsed.body).toBe(safeText!.body.endsWith('\n') ? safeText!.body.slice(0, -1) : safeText!.body);
      await chooseDecision(page, safeText!, fixture.head);
      expect(await page.locator('#inspector-pane h4:has-text("Decision body") + pre.body-text').textContent()).toBe(safeCollapsed.body);
      expect(safeText!.body).toContain('<script');
      expect(await page.locator('#inspector-pane script').count()).toBe(0);

      await page.getByRole('button', { name: 'Conflicts' }).click();
      const conflicts = await loadView(page, '/api/v1/conflicts');
      expect(conflicts.body.data.conflicts.length).toBeGreaterThan(0);
      const conflicted = fixture.decisions.conflicted;
      expect(conflicted).toBeTruthy();
      expect(fixture.conflictHeads).toBeTruthy();
      const conflictPath = `/api/v1/adrs/${encodeURIComponent(conflicted!.adr)}?at=${fixture.head}`;
      const conflictCollapsed = (await readJsonWithBusyRetry(page, `${fixture.origin}${conflictPath}&view=collapsed`, { expectedOid: fixture.head })).body.data;
      const conflictExploded = (await readJsonWithBusyRetry(page, `${fixture.origin}${conflictPath}&view=exploded`, { expectedOid: fixture.head })).body.data;
      expect(conflictCollapsed.resolution_required).toBe(true);
      const items = new Map<string, { body?: string }>();
      for (const operation of conflictExploded.operations) {
        for (const item of operation.items) items.set(item.item, item);
      }
      await page.locator('#context-pane .result-list').getByRole('button', { name: conflicted!.adr }).click();
      await waitForPrimaryInspection(page, conflicted!, fixture.head);
      expect(conflictCollapsed.candidate_records.length).toBe(fixture.conflictHeads!.decision.length);
      for (const candidate of conflictCollapsed.candidate_records) {
        const body = items.get(candidate.record)?.body;
        expect(body, `Missing real exploded body for candidate ${candidate.record}`).toBeTruthy();
        const rendered = page.locator('#inspector-pane h4:has-text("Decision heads")').locator('..').locator('article.candidate').filter({ hasText: candidate.record });
        await expect(rendered).toContainText(candidate.title);
        expect(await rendered.locator('pre.body-text').textContent()).toBe(body);
      }
      for (const [axis, heads] of Object.entries({ Decision: fixture.conflictHeads!.decision, Scope: fixture.conflictHeads!.scope, Domain: fixture.conflictHeads!.domain, Status: fixture.conflictHeads!.status })) {
        const heading = page.locator('#inspector-pane').getByRole('heading', { name: `${axis} heads` });
        await expect(heading).toBeVisible();
        await expect(heading.locator('..').locator('article.candidate')).toHaveCount(heads.length);
      }
      await expect(page.locator('#inspector-pane .candidate pre.body-text').first()).not.toBeEmpty();
      await expect(page.locator('#inspector-pane')).toContainText('Review required:');

      await page.getByRole('button', { name: 'History' }).click();
      await page.getByLabel('Window size (1–1000)').fill('1');
      const history = await loadView(page, '/api/v1/history', { limit: '1' });
      expect(history.body.data.truncated).toBe(true);
      await expect(page.getByText('History window truncated by the server.')).toBeVisible();
      await expect(page.locator('#context-pane .result-list button').first()).toBeVisible();

      await page.getByRole('button', { name: 'Compare' }).click();
      await page.getByLabel('From revision').fill(fixture.baseHead);
      await page.getByLabel('To revision').fill(fixture.head);
      const comparison = await loadView(page, '/api/v1/compare', { from: fixture.baseHead, to: fixture.head });
      expect(comparison.body.data.entries.length).toBeGreaterThan(0);
      await expect(page.locator('#context-pane')).toContainText(`${fixture.baseHead} → ${fixture.head}`);
      await expect(page.locator('#context-pane')).toContainText('Before:');
      await expect(page.locator('#context-pane')).toContainText('After:');

      await page.getByRole('button', { name: 'Doctor' }).click();
      const doctor = await loadView(page, '/api/v1/doctor');
      expect(typeof doctor.body.data.ok).toBe('boolean');
      await expect(page.locator('#context-pane')).toContainText(doctor.body.data.issues.length ? doctor.body.data.issues[0].code : 'No issues reported.');
      await page.keyboard.press('Tab');
      expect(await page.evaluate(() => document.activeElement?.tagName)).toMatch(/^(BUTTON|INPUT|SELECT)$/);
      await fixture.assertSentinels();
    } finally {
      await context.close();
    }
  });
});
