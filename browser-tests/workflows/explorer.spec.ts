import { expect, test, type Browser, type Page } from '@playwright/test';
import { startFixture, type Fixture } from '../support/ui-fixture';

async function withExplorer(browser: Browser, run: (page: Page, fixture: Fixture) => Promise<void>) {
  const context = await browser.newContext();
  let fixture: Fixture | undefined;
  try {
    fixture = await startFixture(context);
    const page = await context.newPage();
    await page.goto(fixture.bootstrapUrl);
    await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.initialHead}`);
    await run(page, fixture);
    expect(fixture.errors).toEqual([]);
  } finally {
    try { await context.close(); } finally { await fixture?.close(); }
  }
}

async function inspect(page: Page, fixture: Fixture) {
  await page.locator('#context-pane .result-list button').first().click();
  await expect(page.locator('#inspector-pane')).toContainText('decision.amend');
  await page.getByLabel('Action', { exact: true }).selectOption('amend');
  await expect(page.getByRole('button', { name: 'Review heads and adopt current tokens' })).toBeEnabled();
}

test('bootstrap, browse and both inspection views use production UI', async ({ browser }) => {
  const context = await browser.newContext({ viewport: { width: 390, height: 844 } });
  let fixture: Awaited<ReturnType<typeof startFixture>> | undefined;
  try {
    fixture = await startFixture(context);
    const page = await context.newPage();
    const assetUrls: string[] = [];
    page.on('request', request => { if (['/app.js', '/app.css'].includes(new URL(request.url()).pathname)) assetUrls.push(page.url()); });
    await page.goto(fixture.bootstrapUrl);
    await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.initialHead}`);
    expect(page.url()).toBe(fixture.origin + '/');
    expect(assetUrls).toHaveLength(2);
    expect(assetUrls.every(url => url === fixture.origin + '/')).toBe(true);
    await expect(page.locator('#context-pane')).toContainText(fixture.state.title);
    await page.locator('#context-pane .result-list button').first().click();
    await expect(page.locator('#inspector-pane')).toContainText(fixture.state.body);
    await expect(page.locator('#inspector-pane')).toContainText('decision.amend');
    await page.getByLabel('Action', { exact: true }).selectOption('amend');
    await expect(page.getByRole('button', { name: 'Review heads and adopt current tokens' })).toBeEnabled();
    await page.reload();
    await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.initialHead}`);
    await expect(page.getByRole('button', { name: 'Submit create', exact: true })).toBeDisabled();
    expect(fixture.errors).toEqual([]);
  } finally {
    try { await context.close(); } finally { await fixture?.close(); }
  }
});

test('search sends the visible query and renders its returned decision', async ({ browser }) => {
  await withExplorer(browser, async (page, fixture) => {
    await page.getByRole('button', { name: 'Search', exact: true }).click();
    await page.getByLabel('Search terms', { exact: true }).fill('architecture café');
    const response = page.waitForResponse(response => new URL(response.url()).searchParams.get('q') === 'architecture café');
    await page.getByRole('button', { name: 'Load view', exact: true }).click();
    await response;
    expect(fixture.reads.some(read => read.path === '/api/v1/search' && read.query.q === 'architecture café')).toBe(true);
    await expect(page.locator('#context-pane')).toContainText(fixture.state.title);
    await inspect(page, fixture);
    await expect(page.locator('#inspector-pane .body-text').first()).toHaveText(fixture.state.body);
  });
});

test('busy inspection recovery clears the stale banner and preserves the draft', async ({ browser }) => {
  await withExplorer(browser, async (page, fixture) => {
    let releaseRetry!: () => void;
    const retryReleased = new Promise<void>(resolve => { releaseRetry = resolve; });
    let receivedRetry!: () => void;
    const retryReceived = new Promise<void>(resolve => { receivedRetry = resolve; });
    let collapsedRequests = 0;
    await page.route(`**/api/v1/adrs/${fixture.adr}?*`, async route => {
      if (new URL(route.request().url()).searchParams.get('view') !== 'collapsed') {
        await route.continue();
      } else if (++collapsedRequests === 1) {
        await route.fulfill({ status: 503, json: {
          schema: 'adrai/api/v1',
          metadata: { generation: String(fixture.state.generation), as_of: { kind: 'commit', oid: fixture.initialHead } },
          error: { category: 'service', status: 503, code: 'repository-busy', message: 'repository is busy' },
        } });
      } else {
        receivedRetry();
        await retryReleased;
        await route.continue();
      }
    });
    try {
      await page.getByLabel('Title', { exact: true }).fill('Draft across inspection recovery');
      await page.locator('#context-pane .result-list button').first().click();
      await retryReceived;
      await expect(page.locator('#inspector-pane')).toContainText('decision.amend');
      await expect(page.getByText('Snapshot is loading or stale.', { exact: true })).toBeVisible();
      releaseRetry();
      await expect(page.locator('#action option[value="amend"]')).toBeEnabled();
      await expect(page.locator('#inspector-pane .body-text').first()).toHaveText(fixture.state.body);
      expect(await page.getByText('Snapshot is loading or stale.', { exact: true }).count()).toBe(0);
      await expect(page.getByLabel('Title', { exact: true })).toHaveValue('Draft across inspection recovery');
      expect(fixture.mutations).toEqual([]);
    } finally {
      releaseRetry();
    }
  });
});

test('create posts reviewed fields and refreshes the visible repository and result', async ({ browser }) => {
  await withExplorer(browser, async (page, fixture) => {
    await page.getByLabel('Title', { exact: true }).fill('Browser-created decision');
    await page.getByLabel('Summary', { exact: true }).fill('A checked create');
    await page.getByLabel('Body', { exact: true }).fill('  Use a deterministic browser fixture.  ');
    await page.getByLabel('Domains, one per line').fill('ui\n testing ');
    await page.getByLabel('Scopes, one per line').fill('browser-tests/**');
    await page.getByLabel('Actor ID', { exact: true }).fill('browser-test');
    await page.getByRole('button', { name: 'Review repository and adopt current basis' }).click();
    await page.getByRole('button', { name: 'Submit create', exact: true }).click();
    await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.changedHead}`);
    expect(fixture.mutations).toEqual([{
      method: 'POST', path: '/api/v1/adrs', authorized: true,
      body: { title: 'Browser-created decision', summary: 'A checked create', body: 'Use a deterministic browser fixture.\n', domains: ['ui', 'testing'], scopes: ['browser-tests/**'], actor: { kind: 'human', id: 'browser-test' }, repository_state: { kind: 'repository', token: 'R' + 'a'.repeat(43), head: fixture.initialHead, head_ref: 'refs/heads/main' } },
    }]);
    await expect(page.locator('#context-pane')).toContainText('Browser-created decision');
    await page.locator('#context-pane .result-list button').first().click();
    await expect(page.locator('#inspector-pane .body-text').first()).toHaveText('Use a deterministic browser fixture.\n');
  });
});

test('an earlier amend response preserves edits made while it is pending', async ({ browser }) => {
  await withExplorer(browser, async (page, fixture) => {
    await inspect(page, fixture);
    await page.getByLabel('Title', { exact: true }).fill('Submitted amendment');
    await page.getByLabel('Summary', { exact: true }).fill('Amended summary');
    await page.getByLabel('Body', { exact: true }).fill('Submitted body');
    await page.getByLabel('Change summary', { exact: true }).fill('Update the decision');
    await page.getByLabel('Actor ID', { exact: true }).fill('browser-test');
    await page.getByRole('button', { name: 'Review heads and adopt current tokens' }).click();
    const held = fixture.holdMutation();
    await page.getByRole('button', { name: 'Submit amend', exact: true }).click();
    const mutation = await held.received;
    expect(mutation).toEqual({ method: 'POST', path: `/api/v1/adrs/${fixture.adr}/amend`, authorized: true, body: { title: 'Submitted amendment', summary: 'Amended summary', body: 'Submitted body\n', change_summary: 'Update the decision', actor: { kind: 'human', id: 'browser-test' }, state_token: 'SYZEeC9tWl0ulonggnO5MTo', repository_state: { kind: 'repository', token: 'R' + 'a'.repeat(43), head: fixture.initialHead, head_ref: 'refs/heads/main' } } });
    await expect(page.getByRole('button', { name: 'Submit amend', exact: true })).toBeDisabled();
    await page.getByLabel('Title', { exact: true }).fill('Newer local draft');
    await page.getByLabel('Body', { exact: true }).fill('Newer local body');
    held.release();
    await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.changedHead}`);
    await expect(page.locator('#actions-pane')).toContainText(`at ${fixture.changedHead}.`);
    await expect(page.locator('#inspector-pane .body-text').first()).toHaveText('Submitted body\n');
    await expect(page.getByLabel('Title', { exact: true })).toHaveValue('Newer local draft');
    await expect(page.getByLabel('Body', { exact: true })).toHaveValue('Newer local body');
    expect(fixture.mutations).toHaveLength(1);
  });
});

test('external invalidation keeps the draft and requires deliberate basis review', async ({ browser }) => {
  await withExplorer(browser, async (page, fixture) => {
    await page.getByLabel('Title', { exact: true }).fill('Draft across external change');
    await page.getByLabel('Body', { exact: true }).fill('Draft body');
    await page.getByLabel('Actor ID', { exact: true }).fill('browser-test');
    await expect(page.locator('#context-pane')).toContainText('Live connection: open');
    await page.getByRole('button', { name: 'Review repository and adopt current basis' }).click();
    await expect(page.getByRole('button', { name: 'Submit create', exact: true })).toBeEnabled();
    fixture.externalChange();
    await expect(page.locator('.masthead')).toContainText(`HEAD ${fixture.changedHead}`);
    await expect(page.getByRole('button', { name: 'Submit create', exact: true })).toBeDisabled();
    await expect(page.getByText('Draft is stale. Refresh the repository, inspect current heads and candidates, then adopt the new tokens.', { exact: true })).toBeVisible();
    await expect(page.getByLabel('Title', { exact: true })).toHaveValue('Draft across external change');
    await expect(page.getByLabel('Body', { exact: true })).toHaveValue('Draft body');
    await page.getByRole('button', { name: 'Review repository and adopt current basis' }).click();
    const held = fixture.holdMutation();
    await page.getByRole('button', { name: 'Submit create', exact: true }).click();
    const mutation = await held.received;
    expect(mutation.body.repository_state).toEqual({ kind: 'repository', token: 'R' + 'b'.repeat(43), head: fixture.changedHead, head_ref: 'refs/heads/main' });
    expect(mutation.body.title).toBe('Draft across external change');
    held.release();
    await expect(page.locator('#context-pane')).toContainText('Draft across external change');
  });
});
