import { expect, test, type Page, type Request, type Response } from '@playwright/test';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { checked, withBrowserServer } from '../support/p704-server';

const scenarios = [
  'bootstrap removes the secret before assets and renders three panes',
  'a checked create is observed by the real server and inspected',
  'an external HEAD change makes a dirty draft require review',
  'cleaned-URL reload reads snapshots but requires credential re-entry',
] as const;

async function refreshRepositoryTo(page: Page, expectedHead?: string): Promise<string> {
  const deadline = Date.now() + 20_000;
  for (let attempt = 0; attempt < 32 && Date.now() < deadline; attempt += 1) {
    const next = page.waitForResponse((response) => new URL(response.url()).pathname === '/api/v1/repository' && response.request().method() === 'GET', { timeout: 7_000 });
    await page.getByRole('button', { name: 'Refresh repository' }).click();
    const response = await next;
    const result = await response.json();
    if (response.status() === 503 && result.error?.code === 'repository-busy') {
      await page.waitForTimeout(250);
      continue;
    }
    expect(response.status(), `repository code=${result.error?.code ?? 'none'}`).toBe(200);
    const head = result.data?.head;
    expect(head).toMatch(/^[a-f0-9]{40}$/);
    expect(result.metadata?.as_of?.oid).toBe(head);
    if (expectedHead && head !== expectedHead) {
      await page.waitForTimeout(250);
      continue;
    }
    try {
      await expect(page.locator('.masthead')).toContainText(`HEAD ${head}`, { timeout: 2_000 });
      return head;
    } catch { /* A later invalidation may discard this checked response. */ }
    await page.waitForTimeout(250);
  }
  throw new Error(`Repository did not expose the expected exact HEAD ${expectedHead ?? ''} within the bounded checked reads`);
}

async function loadDecisionAt(page: Page, head: string, adr: string): Promise<void> {
  const decision = page.getByRole('button', { name: /Runtime browser decision/ });
  const deadline = Date.now() + 20_000;
  for (let attempt = 0; attempt < 32 && Date.now() < deadline; attempt += 1) {
    const began = Date.now();
    const matchesSearch = (url: string, method: string) => new URL(url).pathname === '/api/v1/search' && method === 'GET';
    const started = page.waitForRequest((request) => matchesSearch(request.url(), request.method()), { timeout: 10_000 });
    const next = page.waitForResponse((response) => matchesSearch(response.url(), response.request().method()), { timeout: 30_000 });
    await Promise.all([page.getByRole('button', { name: 'Load view' }).click({ timeout: 10_000 }), started]);
    console.log(`P704_SEARCH_START attempt=${attempt + 1} at=${head}`);
    const response = await next.catch((error) => {
      console.log(`P704_SEARCH_WAIT_FAILED attempt=${attempt + 1} elapsed_ms=${Date.now() - began} at=${head}`);
      throw error;
    });
    const result = await response.json();
    if (response.status() === 503 && result.error?.code === 'repository-busy') {
      await page.waitForTimeout(250);
      continue;
    }
    expect(response.status(), `search code=${result.error?.code ?? 'none'}`).toBe(200);
    expect(result.metadata?.as_of?.oid).toBe(head);
    expect(result.data?.as_of).toBe(head);
    expect(result.data?.results?.some((item: { adr: string }) => item.adr === adr)).toBe(true);
    try {
      await expect(decision).toBeVisible({ timeout: 1_500 });
      await expect(page.locator('#context-pane')).toContainText(`At ${head}`);
      return;
    } catch { /* A later invalidation may discard this checked response. */ }
    await page.waitForTimeout(250);
  }
  throw new Error(`The checked ${head} search window did not render ADR ${adr}`);
}

test.setTimeout(150_000);

test('P7-04 real repository explorer smoke', async ({ browser }) => {
  expect(scenarios.length).toBeGreaterThan(0);
  await withBrowserServer(async ({ bootstrapUrl, origin, repository }) => {
    const context = await browser.newContext({ viewport: { width: 1440, height: 900 } });
    try {
      const page = await context.newPage();
      const token = new URL(bootstrapUrl).searchParams.get('token');
      expect(token).toBeTruthy();
      let invalidations = 0;
      let socketsOpened = 0;
      let socketFrames = 0;
      const socketCloses: string[] = [];
      page.on('websocket', (socket) => {
        socketsOpened += 1;
        socket.on('framereceived', (frame) => {
          socketFrames += 1;
          try {
            const envelope = JSON.parse(frame.payload.toString());
            if (envelope.schema === 'adrai/events/v1' && envelope.event?.type === 'repository-invalidated') invalidations += 1;
          } catch { /* A non-event frame is not evidence of an invalidation. */ }
        });
        socket.on('close', () => { socketCloses.push(socket.url()); });
      });
      const assetRequests: string[] = [];
      let postStarts = 0;
      let postFailures = 0;
      const postRequests: Request[] = [];
      const postResponses: Response[] = [];
      let inspectionStarts = 0;
      const inspectionResponses: Array<{ status: number; code: string; asOf: string; adr: string }> = [];
      page.on('request', (request) => {
        const path = new URL(request.url()).pathname;
        if (path === '/app.js' || path === '/app.css') {
          assetRequests.push(page.url());
        }
        if (path === '/api/v1/adrs' && request.method() === 'POST') {
          postStarts += 1;
          postRequests.push(request);
          console.log(`P704_POST_START=${postStarts}`);
        }
        if (path.startsWith('/api/v1/adrs/') && request.method() === 'GET') inspectionStarts += 1;
      });
      page.on('requestfailed', (request) => {
        if (new URL(request.url()).pathname === '/api/v1/adrs' && request.method() === 'POST') {
          postFailures += 1;
          console.log(`P704_POST_FAILURE=${postFailures}`);
        }
      });
      page.on('response', (response) => {
        const url = new URL(response.url());
        const path = url.pathname;
        if (path === '/api/v1/repository' || path === '/api/v1/search' || (path === '/api/v1/adrs' && response.request().method() === 'POST')) {
          console.log(`P704_HTTP=${response.request().method()} ${path} ${response.status()}`);
        }
        if (path === '/api/v1/adrs' && response.request().method() === 'POST') postResponses.push(response);
        if (path.startsWith('/api/v1/adrs/') && response.request().method() === 'GET') {
          void response.json().then((body) => {
            inspectionResponses.push({ status: response.status(), code: body.error?.code ?? 'none', asOf: body.metadata?.as_of?.oid ?? 'none', adr: body.data?.adr ?? 'none' });
            console.log(`P704_INSPECTION=${path.slice('/api/v1/adrs/'.length)} at=${url.searchParams.get('at') ?? 'none'} view=${url.searchParams.get('view') ?? 'none'} status=${response.status()} as_of=${body.metadata?.as_of?.oid ?? 'none'} adr=${body.data?.adr ?? 'none'} code=${body.error?.code ?? 'none'} invalidations=${invalidations}`);
          }).catch(() => console.log(`P704_INSPECTION=${path.slice('/api/v1/adrs/'.length)} invalid-response`));
        }
      });
      await page.goto(bootstrapUrl);
      await expect(page.getByRole('heading', { name: 'ADRAI repository explorer' })).toBeVisible();
      await expect(page.locator('#context-pane')).toBeVisible();
      await expect(page.locator('#inspector-pane')).toBeVisible();
      await expect(page.locator('#actions-pane')).toBeVisible();
      expect(page.url() === origin + '/', 'Bootstrap credential must be removed from the browser URL').toBe(true);
      expect(assetRequests.length).toBeGreaterThanOrEqual(2);
      expect(assetRequests.every((url) => !url.includes('token='))).toBe(true);
      expect((await page.locator('body').innerText()).includes(token!), 'Bootstrap credential must not appear in rendered content').toBe(false);
      const bootstrapHead = await refreshRepositoryTo(page);
      await expect.poll(() => invalidations, { timeout: 30_000, message: 'No authenticated initial invalidation within the socket liveness guard' }).toBeGreaterThan(0).catch((error) => {
        console.log(`P704_INITIAL_SOCKET_FAILED opened=${socketsOpened} frames=${socketFrames} closes=${socketCloses.length}`);
        throw error;
      });

      const visualDirectory = mkdtempSync(join(process.env.P704_EVIDENCE_DIR ?? tmpdir(), 'visual-'));
      const desktop = join(visualDirectory, 'desktop.png');
      const narrow = join(visualDirectory, 'narrow.png');
      await page.screenshot({ path: desktop, fullPage: true });
      await page.setViewportSize({ width: 390, height: 844 });
      await page.screenshot({ path: narrow, fullPage: true });
      await page.setViewportSize({ width: 1440, height: 900 });
      console.log(`P704_VISUAL_DESKTOP=${desktop}`);
      console.log(`P704_VISUAL_NARROW=${narrow}`);

      await page.getByLabel('Title').fill('Runtime browser decision');
      await page.getByLabel('Summary').fill('A checked decision from the browser');
      await page.getByLabel('Body').fill('Use the repository-bound explorer.\n');
      await page.getByLabel('Domains, one per line').fill('core');
      await page.getByLabel('Scopes, one per line').fill('src/**');
      await page.getByLabel('Actor ID').fill('browser-smoke');
      const originalHead = await refreshRepositoryTo(page, bootstrapHead);
      console.log(`P704_INITIAL_REVIEW_ENABLED=${await page.getByRole('button', { name: 'Review repository and adopt current basis' }).isEnabled()}`);
      await page.getByRole('button', { name: 'Review repository and adopt current basis' }).click();
      const submit = page.getByRole('button', { name: 'Submit create' });
      const status = submit.locator('xpath=following-sibling::p[1]');
      console.log(`P704_AFTER_REVIEW enabled=${await submit.isEnabled()} adopted=${await status.getByText('Current repository basis adopted.', { exact: true }).isVisible()}`);
      await expect.poll(() => status.getByText('Current repository basis adopted.', { exact: true }).isVisible(), { timeout: 10_000 }).toBe(true);
      let committed: any;
      let permittedPostStarts = 0;
      let provenBusyRetries = 0;
      for (let attempt = 0; attempt < 4; attempt += 1) {
        expect(postStarts, 'Unexpected create POST before the checked submit').toBe(permittedPostStarts);
        const began = Date.now();
        console.log(`P704_BEFORE_SUBMIT attempt=${attempt + 1} enabled=${await submit.isEnabled()} adopted=${await status.getByText('Current repository basis adopted.', { exact: true }).isVisible()}`);
        const clickCompleted = await submit.click({ timeout: 10_000 }).then(() => true, () => false);
        if (!clickCompleted) throw new Error(`Create click did not complete; enabled=${await submit.isEnabled()} adopted=${await status.getByText('Current repository basis adopted.', { exact: true }).isVisible()} starts=${postStarts}`);
        await expect.poll(() => postStarts >= permittedPostStarts + 1, { timeout: 10_000, message: 'Create click completed without a POST request in the request-start guard' }).toBe(true);
        permittedPostStarts += 1;
        expect(postStarts, 'The checked click must start exactly one create POST').toBe(permittedPostStarts);
        const startedRequest = postRequests[permittedPostStarts - 1];
        console.log(`P704_AFTER_CLICK attempt=${attempt + 1} starts=${postStarts} elapsed_ms=${Date.now() - began} submitting=${await status.getByText('Submitting checked operation…', { exact: true }).isVisible()}`);
        const responseForStart = () => postResponses.find((item) => item.request() === startedRequest);
        await expect.poll(() => Boolean(responseForStart()), { timeout: 7_000 }).toBe(true).catch(() => undefined);
        if (!responseForStart()) {
          const observedHead = await checked('git', ['rev-parse', 'HEAD'], repository, 10_000);
          console.log(`P704_POST_PENDING elapsed_ms=${Date.now() - began} head_advanced=${observedHead !== originalHead} starts=${postStarts} failures=${postFailures} submitting=${await status.getByText('Submitting checked operation…', { exact: true }).isVisible()}`);
        }
        const remaining = Math.max(1, 60_000 - (Date.now() - began));
        await expect.poll(() => Boolean(responseForStart()), { timeout: remaining, message: 'Create POST started but no response arrived within the response guard' }).toBe(true).catch(async (error) => {
          console.log(`P704_POST_WAIT_FAILED attempt=${attempt + 1} starts=${postStarts} failures=${postFailures} elapsed_ms=${Date.now() - began} enabled=${await submit.isEnabled()} submitting=${await status.getByText('Submitting checked operation…', { exact: true }).isVisible()}`);
          throw error;
        });
        const response = responseForStart()!;
        expect(response.request(), 'Create response must belong to the checked POST').toBe(startedRequest);
        expect(postStarts, 'No additional create POST may start while awaiting a result').toBe(permittedPostStarts);
        const result = await response.json();
        console.log(`P704_POST_RESULT=${response.status()} code=${result.error?.code ?? 'none'} committed=${result.data?.committed ?? 'none'} elapsed_ms=${Date.now() - began} adr=${result.data?.adr ?? 'none'} commit=${result.data?.commit ?? 'none'} operation=${result.data?.operation ?? 'none'}`);
        if (response.status() === 503 && result.error?.code === 'repository-busy' && attempt < 3) {
          expect(result.error.status).toBe(503);
          expect(result.data).toBeUndefined();
          expect(await checked('git', ['rev-parse', 'HEAD'], repository, 10_000)).toBe(originalHead);
          provenBusyRetries += 1;
          await refreshRepositoryTo(page, originalHead);
          await expect(page.getByRole('button', { name: 'Review repository and adopt current basis' })).toBeEnabled();
          await page.getByRole('button', { name: 'Review repository and adopt current basis' }).click();
          continue;
        }
        expect(response.status(), `create code=${result.error?.code ?? 'none'}`).toBe(200);
        committed = result;
        break;
      }
      expect(committed.data.committed).toBe(true);
      expect(postStarts, 'One committed create after only proven precommit busy retries').toBe(provenBusyRetries + 1);
      expect(committed.data.commit).toMatch(/^[a-f0-9]{40}$/);
      expect(committed.data.operation).toBeTruthy();
      console.log(`P704_CREATE_INDEXED=${committed.data.indexed} P704_CREATE_WARNING=${Boolean(committed.data.publication_warning ?? committed.data.index_error)}`);
      await expect(page.getByText(/Committed .* at [a-f0-9]{40}/)).toBeVisible();
      const decision = page.getByRole('button', { name: /Runtime browser decision/ });
      let autoResynced = false;
      try {
        await expect(decision).toBeVisible({ timeout: 3_000 });
        await expect(page.locator('#context-pane')).toContainText(`At ${committed.data.commit}`, { timeout: 1_000 });
        autoResynced = true;
      } catch { /* A bounded busy read leaves the view visibly stale for manual recovery. */ }
      console.log(`P704_AUTO_RESYNC_ACCEPTED=${autoResynced}`);
      if (!autoResynced) {
        await refreshRepositoryTo(page, committed.data.commit);
        await loadDecisionAt(page, committed.data.commit, committed.data.adr);
      }
      console.log(`P704_INSPECTION_CLICK=${committed.data.adr} at=${committed.data.commit} invalidations=${invalidations}`);
      const inspectionDeadline = Date.now() + 50_000;
      let inspectionReady = false;
      for (let attempt = 0; attempt < 4 && Date.now() < inspectionDeadline; attempt += 1) {
        if (!(await decision.isVisible())) {
          await refreshRepositoryTo(page, committed.data.commit);
          await loadDecisionAt(page, committed.data.commit, committed.data.adr);
        }
        const before = inspectionResponses.length;
        const startsBefore = inspectionStarts;
        await decision.click();
        try {
          await expect(page.locator('#inspector-pane h3')).toHaveText('Runtime browser decision', { timeout: 12_000 });
          inspectionReady = true;
          break;
        } catch (failure) {
          const observed = inspectionResponses.slice(before);
          console.log(`P704_INSPECTION_RETRY attempt=${attempt + 1} starts=${inspectionStarts - startsBefore} responses=${observed.length} busy=${observed.filter((item) => item.status === 503 && item.code === 'repository-busy').length} invalidations=${invalidations}`);
          const invalidStatus = observed.some((item) => item.status !== 200 && !(item.status === 503 && item.code === 'repository-busy'));
          const wrongBasis = observed.some((item) => item.status === 200 && (item.asOf !== committed.data.commit || item.adr !== committed.data.adr));
          if (inspectionStarts === startsBefore || observed.length === 0 || invalidStatus || wrongBasis || attempt === 3) throw failure;
          await page.waitForTimeout(250);
        }
      }
      expect(inspectionReady, 'Exact committed decision inspection did not become ready').toBe(true);
      await expect(page.locator('#inspector-pane h3 + p + p')).toHaveText('A checked decision from the browser');
      await expect(page.locator('#inspector-pane h4:has-text("Decision body") + pre.body-text')).toHaveText('Use the repository-bound explorer.');
      await expect(page.locator('#inspector-pane h4').filter({ hasText: /^Operation / }).first()).toBeVisible();
      await expect(page.locator('#inspector-pane .error')).toHaveCount(0);
      console.log('P704_PRIMARY_INSPECTION_READY=true');

      await page.screenshot({ path: desktop, fullPage: true });
      await page.setViewportSize({ width: 390, height: 844 });
      await expect(page.locator('#context-pane')).toBeVisible();
      await expect(page.locator('#inspector-pane')).toBeVisible();
      await expect(page.locator('#actions-pane')).toBeVisible();
      await page.screenshot({ path: narrow, fullPage: true });
      console.log(`P704_VISUAL_DESKTOP=${desktop}`);
      console.log(`P704_VISUAL_NARROW=${narrow}`);

      await page.getByLabel('Action').selectOption('amend');
      await expect(page.getByText(`Target: ${committed.data.adr}`)).toBeVisible();
      await expect(page.getByLabel('Change summary')).toBeVisible();
      await page.getByLabel('Change summary').fill('Review after external HEAD change');
      const beforeExternalChange = invalidations;
      await checked('git', ['commit', '--allow-empty', '-m', 'external browser smoke change'], repository, 15_000);
      const externalHead = await checked('git', ['rev-parse', 'HEAD'], repository, 15_000);
      await expect.poll(() => invalidations, { timeout: 15_000, message: `External HEAD change did not reach authenticated socket; closes=${socketCloses.length}` }).toBeGreaterThan(beforeExternalChange);
      await expect(page.getByText(/Draft is stale/)).toBeVisible({ timeout: 10_000 });
      await refreshRepositoryTo(page, externalHead);
      await loadDecisionAt(page, externalHead, committed.data.adr);
      await page.getByRole('button', { name: /Runtime browser decision/ }).click();
      await page.getByRole('button', { name: 'Review heads and adopt current tokens' }).click();
      await expect(page.getByText('Current heads reviewed; fresh tokens adopted.')).toBeVisible();

      let unauthenticatedSockets = 0;
      page.on('websocket', () => { unauthenticatedSockets += 1; });
      await page.reload();
      await expect(page.getByRole('heading', { name: 'ADRAI repository explorer' })).toBeVisible();
      await expect(page.getByText(/Read-only session\. Reopen the process bootstrap URL/)).toBeVisible();
      await expect(page.getByRole('button', { name: 'Submit create' })).toBeDisabled();
      expect(unauthenticatedSockets).toBe(0);
      expect(page.url() === origin + '/', 'Reloaded URL must remain free of the bootstrap credential').toBe(true);
      expect((await page.locator('body').innerText()).includes(token!), 'Reloaded content must not expose the bootstrap credential').toBe(false);
      expect(postStarts, 'No duplicate create POST may start later in the browser flow').toBe(provenBusyRetries + 1);
    } finally {
      await context.close();
    }
  });
});
