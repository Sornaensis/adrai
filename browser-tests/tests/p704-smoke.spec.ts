import { expect, test, type Page } from '@playwright/test';
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
  for (let attempt = 0; attempt < 8; attempt += 1) {
    const next = page.waitForResponse((response) => new URL(response.url()).pathname === '/api/v1/repository' && response.request().method() === 'GET', { timeout: 7_000 });
    await page.getByRole('button', { name: 'Refresh repository' }).click();
    const response = await next;
    const result = await response.json();
    if (response.status() === 503 && result.error?.code === 'repository-busy') continue;
    expect(response.status(), JSON.stringify(result.error ?? {})).toBe(200);
    const head = result.data?.head;
    expect(head).toMatch(/^[a-f0-9]{40}$/);
    expect(result.metadata?.as_of?.oid).toBe(head);
    if (expectedHead && head !== expectedHead) continue;
    await expect(page.locator('.masthead')).toContainText(`HEAD ${head}`);
    return head;
  }
  throw new Error(`Repository did not expose the expected exact HEAD ${expectedHead ?? ''} within eight checked reads`);
}

async function loadDecisionAt(page: Page, head: string, adr: string): Promise<void> {
  const decision = page.getByRole('button', { name: /Runtime browser decision/ });
  for (let attempt = 0; attempt < 8; attempt += 1) {
    const next = page.waitForResponse((response) => new URL(response.url()).pathname === '/api/v1/search' && response.request().method() === 'GET', { timeout: 7_000 });
    await page.getByRole('button', { name: 'Load view' }).click();
    const response = await next;
    const result = await response.json();
    if (response.status() === 503 && result.error?.code === 'repository-busy') continue;
    expect(response.status(), JSON.stringify(result.error ?? {})).toBe(200);
    expect(result.metadata?.as_of?.oid).toBe(head);
    expect(result.data?.as_of).toBe(head);
    expect(result.data?.results?.some((item: { adr: string }) => item.adr === adr)).toBe(true);
    try {
      await expect(decision).toBeVisible({ timeout: 1_500 });
      await expect(page.locator('#context-pane')).toContainText(`At ${head}`);
      return;
    } catch { /* A later invalidation may discard this checked response. */ }
  }
  throw new Error(`The checked ${head} search window did not render ADR ${adr}`);
}

test.setTimeout(120_000);

test('P7-04 real repository explorer smoke', async ({ browser }) => {
  expect(scenarios.length).toBeGreaterThan(0);
  await withBrowserServer(async ({ bootstrapUrl, origin, repository }) => {
    const context = await browser.newContext({ viewport: { width: 1440, height: 900 } });
    try {
      const page = await context.newPage();
      const token = new URL(bootstrapUrl).searchParams.get('token');
      expect(token).toBeTruthy();
      let invalidations = 0;
      const socketCloses: string[] = [];
      page.on('websocket', (socket) => {
        socket.on('framereceived', (frame) => {
          try {
            const envelope = JSON.parse(frame.payload.toString());
            if (envelope.schema === 'adrai/events/v1' && envelope.event?.type === 'repository-invalidated') invalidations += 1;
          } catch { /* A non-event frame is not evidence of an invalidation. */ }
        });
        socket.on('close', () => { socketCloses.push(socket.url()); });
      });
      const assetRequests: string[] = [];
      page.on('request', (request) => {
        if (new URL(request.url()).pathname === '/app.js' || new URL(request.url()).pathname === '/app.css') {
          assetRequests.push(page.url());
        }
      });
      page.on('response', (response) => {
        const url = new URL(response.url());
        const path = url.pathname;
        if (path === '/api/v1/repository' || path === '/api/v1/search' || (path === '/api/v1/adrs' && response.request().method() === 'POST')) {
          console.log(`P704_HTTP=${response.request().method()} ${path} ${response.status()}`);
        }
        if (path.startsWith('/api/v1/adrs/') && response.request().method() === 'GET') {
          void response.json().then((body) => {
            console.log(`P704_INSPECTION=${path.slice('/api/v1/adrs/'.length)} at=${url.searchParams.get('at') ?? 'none'} view=${url.searchParams.get('view') ?? 'none'} status=${response.status()} as_of=${body.metadata?.as_of?.oid ?? 'none'} adr=${body.data?.adr ?? 'none'} code=${body.error?.code ?? 'none'} invalidations=${invalidations}`);
          }).catch(() => console.log(`P704_INSPECTION=${path.slice('/api/v1/adrs/'.length)} invalid-response`));
        }
      });
      await page.goto(bootstrapUrl);
      await expect(page.getByRole('heading', { name: 'ADRAI repository explorer' })).toBeVisible();
      await expect(page.locator('#context-pane')).toBeVisible();
      await expect(page.locator('#inspector-pane')).toBeVisible();
      await expect(page.locator('#actions-pane')).toBeVisible();
      expect(page.url()).toBe(origin + '/');
      expect(assetRequests.length).toBeGreaterThanOrEqual(2);
      expect(assetRequests.every((url) => !url.includes('token='))).toBe(true);
      expect(await page.locator('body').innerText()).not.toContain(token!);
      await expect(page.locator('.masthead')).toContainText(/HEAD [a-f0-9]{40}/, { timeout: 15_000 });
      await expect.poll(() => invalidations, { timeout: 15_000, message: `No authenticated initial invalidation; closes=${socketCloses.length}` }).toBeGreaterThan(0);

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
      const originalHead = await refreshRepositoryTo(page);
      console.log(`P704_INITIAL_REVIEW_ENABLED=${await page.getByRole('button', { name: 'Review repository and adopt current basis' }).isEnabled()}`);
      await page.getByRole('button', { name: 'Review repository and adopt current basis' }).click();
      let committed: any;
      for (let attempt = 0; attempt < 4; attempt += 1) {
        const committedResponse = page.waitForResponse((response) => response.url().endsWith('/api/v1/adrs') && response.request().method() === 'POST', { timeout: 7_000 });
        await page.getByRole('button', { name: 'Submit create' }).click();
        const response = await committedResponse;
        const result = await response.json();
        const lockOwner = result.error?.message?.match(/\s(\d+)$/)?.[1] ?? 'none';
        console.log(`P704_POST_RESULT=${response.status()} code=${result.error?.code ?? 'none'} committed=${result.data?.committed ?? 'none'} lock_owner=${lockOwner}`);
        if (response.status() === 503 && result.error?.code === 'repository-lock-unavailable' && attempt < 3) {
          expect(result.error.status).toBe(503);
          expect(result.data).toBeUndefined();
          await refreshRepositoryTo(page, originalHead);
          await expect(page.getByRole('button', { name: 'Review repository and adopt current basis' })).toBeEnabled();
          await page.getByRole('button', { name: 'Review repository and adopt current basis' }).click();
          continue;
        }
        expect(response.status(), JSON.stringify(result)).toBe(200);
        committed = result;
        break;
      }
      expect(committed.data.committed).toBe(true);
      expect(committed.data.commit).toMatch(/^[a-f0-9]{40}$/);
      expect(committed.data.operation).toBeTruthy();
      console.log(`P704_CREATE_INDEXED=${committed.data.indexed} P704_CREATE_WARNING=${committed.data.publication_warning ?? committed.data.index_error ?? ''}`);
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
      await page.getByRole('button', { name: /Runtime browser decision/ }).click();
      try {
        await expect(page.locator('#inspector-pane h3')).toHaveText('Runtime browser decision');
        await expect(page.locator('#inspector-pane h3 + p + p')).toHaveText('A checked decision from the browser');
        await expect(page.locator('#inspector-pane h4:has-text("Decision body") + pre.body-text')).toHaveText('Use the repository-bound explorer.');
        await expect(page.locator('#inspector-pane h4').filter({ hasText: /^Operation / }).first()).toBeVisible();
        await expect(page.locator('#inspector-pane .error')).toHaveCount(0);
      } catch (failure) {
        console.log(`P704_INSPECTION_PLACEHOLDER=${await page.getByText('Select an ADR to inspect its decision, candidates, and operation provenance.').isVisible()} invalidations=${invalidations}`);
        throw failure;
      }
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
      expect(page.url()).toBe(origin + '/');
      expect(await page.locator('body').innerText()).not.toContain(token!);
    } finally {
      await context.close();
    }
  });
});
