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
  test.setTimeout(45_000);
  await withP705Server({ scenarioId: 'B01', seed: 'main' }, async fixture => {
    const context = await browser.newContext();
    try {
      const page = await context.newPage();
      const token = new URL(fixture.bootstrapUrl).searchParams.get('token');
      expect(token).toBeTruthy();
      const assetPageUrls: string[] = [];
      const pageErrors: string[] = [];
      let authenticatedEvents = 0;
      page.on('request', request => {
        if (['/app.js', '/app.css'].includes(new URL(request.url()).pathname)) assetPageUrls.push(page.url());
      });
      page.on('pageerror', error => pageErrors.push(error.message));
      page.on('websocket', socket => {
        socket.on('framereceived', frame => {
          try {
            const body = JSON.parse(frame.payload.toString());
            if (body.schema === 'adrai/events/v1') authenticatedEvents += 1;
          } catch { /* A non-JSON frame is not authenticated event evidence. */ }
        });
      });
      await openExplorer(page, fixture);
      await expect(page.locator('#context-pane')).toBeVisible();
      await expect(page.locator('#inspector-pane')).toBeVisible();
      await expect(page.locator('#actions-pane')).toBeVisible();
      expect(page.url()).toBe(`${fixture.origin}/`);
      expect(assetPageUrls.length).toBeGreaterThanOrEqual(2);
      expect(assetPageUrls.every(url => !url.includes('token='))).toBe(true);
      await expect.poll(() => authenticatedEvents, { timeout: 10_000 }).toBeGreaterThan(0);
      expect((await context.cookies(fixture.origin)).some(cookie => cookie.httpOnly)).toBe(true);
      expect(await page.locator('body').innerText()).not.toContain(token!);
      expect(pageErrors.join(' ')).not.toContain(token!);
      expect(await page.evaluate(() => [
        ...Object.keys(localStorage).map(key => `${key}=${localStorage.getItem(key)}`),
        ...Object.keys(sessionStorage).map(key => `${key}=${sessionStorage.getItem(key)}`),
      ].join(' '))).not.toContain(token!);

      let reloadSockets = 0;
      page.on('websocket', () => { reloadSockets += 1; });
      await page.reload();
      await expect(page.getByText(/Read-only session\. Reopen the process bootstrap URL/)).toBeVisible();
      await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.head}`);
      await expect(page.getByRole('button', { name: 'Submit create' })).toBeDisabled();
      await page.waitForTimeout(750);
      expect(reloadSockets).toBe(0);
      expect(page.url()).toBe(`${fixture.origin}/`);

      const newTab = await context.newPage();
      let newTabSockets = 0;
      newTab.on('websocket', () => { newTabSockets += 1; });
      await newTab.goto(fixture.origin);
      await expect(newTab.getByText(/Read-only session\. Reopen the process bootstrap URL/)).toBeVisible();
      await expect(newTab.locator('.masthead')).toContainText(`HEAD ${fixture.head}`);
      await newTab.waitForTimeout(750);
      expect(newTabSockets).toBe(0);
      expect(await newTab.locator('body').innerText()).not.toContain(token!);
      await fixture.assertSentinels();
    } finally {
      await context.close();
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
