import { expect, test, type Browser, type Page } from '@playwright/test';
import {
  waitForPrimaryInspection,
  withP705Server,
  type DecisionRef,
  type P705Fixture,
} from '../support/p705-server';

type Json = Record<string, any>;
type Action = 'create' | 'amend' | 'scope' | 'domain' | 'obsolete' | 'reactivate';

const oid = /^[a-f0-9]{40}$/;
test.setTimeout(45_000);

async function inBrowser<T>(browser: Browser, fixture: P705Fixture, run: (page: Page) => Promise<T>): Promise<T> {
  const context = await browser.newContext({ viewport: { width: 1440, height: 900 } });
  try {
    const page = await context.newPage();
    const secret = new URL(fixture.bootstrapUrl).searchParams.get('token');
    expect(secret).toBeTruthy();
    const errors: string[] = [];
    page.on('pageerror', (error) => { errors.push(error.message); });
    page.on('console', (message) => { if (message.type() === 'error') errors.push(message.text()); });
    await page.goto(fixture.bootstrapUrl);
    await expect(page.getByRole('heading', { name: 'ADRAI repository explorer' })).toBeVisible();
    expect(page.url()).toBe(`${fixture.origin}/`);
    expect(await page.locator('body').innerText()).not.toContain(secret!);
    const result = await run(page);
    const exposed = await page.evaluate(() => ({
      url: location.href,
      body: document.body.innerText,
      history: JSON.stringify(history.state),
      local: Object.entries(localStorage),
      session: Object.entries(sessionStorage),
    }));
    expect(JSON.stringify(exposed) + errors.join('\n')).not.toContain(secret!);
    return result;
  } finally {
    await context.close();
  }
}

async function refresh(page: Page, expectedHead: string): Promise<Json> {
  for (let attempt = 0; attempt < 5; attempt += 1) {
    const responsePromise = page.waitForResponse((response) =>
      new URL(response.url()).pathname === '/api/v1/repository' && response.request().method() === 'GET',
    );
    await page.getByRole('button', { name: 'Refresh repository' }).click();
    const response = await responsePromise;
    const body: Json = await response.json();
    if (response.status() === 503 && body.error?.code === 'repository-busy') {
      await page.waitForTimeout(100);
      continue;
    }
    expect(response.status(), body.error?.code).toBe(200);
    expect(body.data?.head).toMatch(oid);
    expect(body.metadata?.as_of?.oid).toBe(body.data.head);
    if (body.data.head !== expectedHead) continue;
    await expect(page.locator('.masthead')).toContainText(`HEAD ${expectedHead}`);
    return body.data;
  }
  throw new Error(`Repository never displayed expected HEAD ${expectedHead}`);
}

async function selectDecision(page: Page, decision: DecisionRef, head: string, includeObsolete = false): Promise<void> {
  if (includeObsolete) await page.locator('#context-pane').getByLabel('Include obsolete').check();
  for (let attempt = 0; attempt < 5; attempt += 1) {
    const searchPromise = page.waitForResponse((response) =>
      new URL(response.url()).pathname === '/api/v1/search' && response.request().method() === 'GET',
    );
    await page.getByRole('button', { name: 'Load view' }).click();
    const search = await searchPromise;
    const body: Json = await search.json();
    if (search.status() === 503 && body.error?.code === 'repository-busy') {
      await page.waitForTimeout(100);
      continue;
    }
    expect(search.status(), body.error?.code).toBe(200);
    if (body.metadata?.as_of?.oid !== head) continue;
    expect(body.data?.results?.some((item: Json) => item.adr === decision.adr)).toBe(true);
    const choice = page.locator('#context-pane button').filter({ hasText: decision.adr }).first();
    await expect(choice).toBeVisible();
    const views = ['collapsed', 'exploded'].map((view) =>
      page.waitForResponse((response) => {
        const url = new URL(response.url());
        return url.pathname === `/api/v1/adrs/${decision.adr}`
          && url.searchParams.get('at') === head
          && url.searchParams.get('view') === view
          && response.status() === 200;
      }),
    );
    await choice.click();
    await Promise.all(views);
    await waitForPrimaryInspection(page, decision, head);
    return;
  }
  throw new Error(`ADR ${decision.adr} was not selected at ${head}`);
}

async function readInspection(page: Page, decision: DecisionRef, head: string, view: 'collapsed' | 'exploded'): Promise<Json> {
  for (let attempt = 0; attempt < 5; attempt += 1) {
    const response = await page.request.get(`${new URL(`/api/v1/adrs/${decision.adr}`, page.url())}?at=${head}&view=${view}`);
    const body: Json = await response.json();
    if (response.status() === 503 && body.error?.code === 'repository-busy') {
      await page.waitForTimeout(100);
      continue;
    }
    expect(response.status(), body.error?.code).toBe(200);
    expect(body.metadata?.as_of?.oid).toBe(head);
    expect(body.data?.adr).toBe(decision.adr);
    expect(body.data?.as_of).toBe(head);
    expect(body.data?.view).toBe(view);
    return body.data;
  }
  throw new Error(`Inspection ${view} stayed busy for ${decision.adr} at ${head}`);
}

async function reviewExisting(page: Page, fixture: P705Fixture, decision: DecisionRef, head: string, includeObsolete = false): Promise<Json> {
  const repository = await refresh(page, head);
  expect(repository.repository_state?.head).toBe(head);
  expect(repository.repository_state?.token).toBeTruthy();
  await selectDecision(page, decision, head, includeObsolete);
  const collapsed = await readInspection(page, decision, head, 'collapsed');
  const exploded = await readInspection(page, decision, head, 'exploded');
  expect(collapsed.state_token).toBe(exploded.state_token);
  expect(collapsed.title).toBeTruthy();
  expect(collapsed.summary).toBeTruthy();
  expect(collapsed.body).toBeTruthy();
  expect(exploded.operations.length).toBeGreaterThan(0);
  expect(await fixture.currentHead()).toBe(head);
  return { repository, collapsed, exploded };
}

async function submit(page: Page, fixture: P705Fixture, action: Action, decision: DecisionRef | undefined, basis: Json): Promise<Json> {
  const path = action === 'create' ? '/api/v1/adrs' : `/api/v1/adrs/${decision!.adr}/${action}`;
  const responsePromise = page.waitForResponse((response) =>
    new URL(response.url()).pathname === path && response.request().method() === 'POST',
  );
  await page.getByRole('button', { name: `Submit ${action}` }).click();
  const response = await responsePromise;
  const body: Json = await response.json();
  expect(response.status(), body.error?.code).toBe(200);
  expect(body.data?.committed).toBe(true);
  expect(body.data?.operation).toMatch(/^O[0-9A-Z]+$/);
  expect(body.data?.commit).toMatch(oid);
  expect(body.data?.adr).toBe(decision?.adr ?? body.data.adr);
  const sent: Json = response.request().postDataJSON();
  expect(sent.repository_state?.head).toBe(basis.repository.head);
  expect(sent.repository_state?.head_ref).toBe(basis.repository.head_ref);
  expect(sent.repository_state?.token).toBe(basis.repository.repository_state.token);
  if (decision) expect(sent.state_token).toBe(basis.collapsed.state_token);
  else expect(sent.state_token).toBeUndefined();
  expect(await fixture.currentHead()).toBe(body.data.commit);
  await fixture.assertSentinels();
  await expect(page.locator('#actions-pane')).toContainText(`Committed ${body.data.operation} at ${body.data.commit}`);
  if (body.data.publication_warning) await expect(page.locator('#actions-pane')).toContainText('Publication warning:');
  if (!body.data.indexed) await expect(page.locator('#actions-pane')).toContainText('Index warning:');
  console.log(`P705_MUTATION action=${action} status=${response.status()} operation=${body.data.operation} commit=${body.data.commit} indexed=${body.data.indexed} publication_warning=${Boolean(body.data.publication_warning)}`);
  return { ...body.data, request: sent };
}

async function chooseAction(page: Page, action: Exclude<Action, 'create'>): Promise<void> {
  await page.getByLabel('Action').selectOption(action);
  await expect(page.getByRole('button', { name: 'Review heads and adopt current tokens' })).toBeEnabled();
}

async function adoptExisting(page: Page, head: string): Promise<void> {
  await expect(page.locator('#actions-pane')).toContainText(`HEAD ${head}`);
  await page.getByRole('button', { name: 'Review heads and adopt current tokens' }).click();
  await expect(page.locator('#actions-pane')).toContainText('Current heads reviewed; fresh tokens adopted.');
}

test('B06 checked create commits exactly what the reviewed UI submitted', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B06', seed: 'main' }, async (fixture) => inBrowser(browser, fixture, async (page) => {
    const before = await reviewExisting(page, fixture, fixture.decisions.primary, fixture.head);
    await page.getByLabel('Action').selectOption('create');
    await page.getByLabel('Title').fill('B06 browser created decision');
    await page.getByLabel('Summary', { exact: true }).fill('Created through the checked browser form');
    await page.getByLabel('Body').fill('Use a browser verified transaction.\n');
    await page.getByLabel('Domains, one per line').fill('runtime');
    await page.getByLabel('Scopes, one per line').fill('src/**');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await page.getByRole('button', { name: 'Review repository and adopt current basis' }).click();
    const result = await submit(page, fixture, 'create', undefined, before);
    expect(result.request).toMatchObject({ title: 'B06 browser created decision', summary: 'Created through the checked browser form', domains: ['runtime'], scopes: ['src/**'] });
    const created: DecisionRef = { adr: result.adr, title: 'B06 browser created decision', summary: 'Created through the checked browser form', body: 'Use a browser verified transaction.\n', at: result.commit };
    await refresh(page, result.commit);
    await selectDecision(page, created, result.commit);
    const after = await readInspection(page, created, result.commit, 'collapsed');
    expect(after.title).toBe(created.title);
    expect(after.summary).toBe(created.summary);
    expect(after.body.trimEnd()).toBe(created.body.trimEnd());
    expect(after.domains).toContain('runtime');
    expect(after.applies_to).toContain('src/**');
    const exploded = await readInspection(page, created, result.commit, 'exploded');
    expect(JSON.stringify(exploded.operations)).toContain(result.operation);
  }));
});

test('B07 checked amend replaces reviewed decision content', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B07', seed: 'main' }, async (fixture) => inBrowser(browser, fixture, async (page) => {
    const decision = fixture.decisions.primary;
    const before = await reviewExisting(page, fixture, decision, fixture.head);
    expect(before.collapsed.title).toBe(decision.title);
    expect(before.collapsed.summary).toBe(decision.summary);
    expect(before.collapsed.body.trimEnd()).toBe(decision.body.trimEnd());
    await chooseAction(page, 'amend');
    await page.getByLabel('Title').fill('B07 reviewed amendment');
    await page.getByLabel('Summary', { exact: true }).fill('The inspected decision has changed');
    await page.getByLabel('Body').fill('Replace the reviewed body.\n');
    await page.getByLabel('Change summary').fill('Review and replace the seeded decision');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await adoptExisting(page, fixture.head);
    const result = await submit(page, fixture, 'amend', decision, before);
    expect(result.request).toMatchObject({ change_summary: 'Review and replace the seeded decision', title: 'B07 reviewed amendment' });
    const after = await readInspection(page, decision, result.commit, 'collapsed');
    expect(after.title).toBe('B07 reviewed amendment');
    expect(after.summary).toBe('The inspected decision has changed');
    expect(after.body.trimEnd()).toBe('Replace the reviewed body.');
    const exploded = await readInspection(page, decision, result.commit, 'exploded');
    expect(JSON.stringify(exploded.operations)).toContain(result.operation);
  }));
});

test('B08 checked scope delta updates the reviewed applicability', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B08', seed: 'main' }, async (fixture) => inBrowser(browser, fixture, async (page) => {
    const decision = fixture.decisions.primary;
    const before = await reviewExisting(page, fixture, decision, fixture.head);
    await chooseAction(page, 'scope');
    await page.getByLabel('Reason').fill('Include browser fixtures');
    await page.getByLabel('Change mode').selectOption('delta');
    await page.getByLabel('Add, one per line').fill('browser-tests/**');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await adoptExisting(page, fixture.head);
    const result = await submit(page, fixture, 'scope', decision, before);
    expect(result.request).toMatchObject({ mode: 'delta', add: ['browser-tests/**'], remove: [] });
    expect(result.mode).toBe('expand');
    expect(result.applies_to).toContain('browser-tests/**');
    const after = await readInspection(page, decision, result.commit, 'collapsed');
    expect(after.applies_to).toContain('browser-tests/**');
    expect(after.scope_heads).not.toEqual(before.collapsed.scope_heads);
  }));
});

test('B09 checked domain delta updates reviewed domains', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B09', seed: 'main' }, async (fixture) => inBrowser(browser, fixture, async (page) => {
    const decision = fixture.decisions.primary;
    const before = await reviewExisting(page, fixture, decision, fixture.head);
    await chooseAction(page, 'domain');
    await page.getByLabel('Reason').fill('Classify browser runtime');
    await page.getByLabel('Change mode').selectOption('delta');
    await page.getByLabel('Add, one per line').fill('browser');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await adoptExisting(page, fixture.head);
    const result = await submit(page, fixture, 'domain', decision, before);
    expect(result.request).toMatchObject({ mode: 'delta', add: ['browser'], remove: [] });
    expect(result.mode).toBe('expand');
    expect(result.domains).toContain('browser');
    const after = await readInspection(page, decision, result.commit, 'collapsed');
    expect(after.domains).toContain('browser');
    expect(after.domain_heads).not.toEqual(before.collapsed.domain_heads);
  }));
});

test('B10 checked obsolete changes the reviewed status', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B10', seed: 'main' }, async (fixture) => inBrowser(browser, fixture, async (page) => {
    const decision = fixture.decisions.primary;
    const before = await reviewExisting(page, fixture, decision, fixture.head);
    expect(before.collapsed.status).toBe('active');
    await chooseAction(page, 'obsolete');
    await page.getByLabel('Reason').fill('Superseded after browser review');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await adoptExisting(page, fixture.head);
    const result = await submit(page, fixture, 'obsolete', decision, before);
    expect(result.request).toMatchObject({ reason: 'Superseded after browser review', resolve: false });
    expect(result.obsolete).toBe(true);
    const after = await readInspection(page, decision, result.commit, 'collapsed');
    expect(after.status).toBe('obsolete');
    expect(after.status_heads).not.toEqual(before.collapsed.status_heads);
  }));
});

test('B11 checked reactivate changes the reviewed obsolete status', async ({ browser }) => {
  await withP705Server({ scenarioId: 'B11', seed: 'main' }, async (fixture) => inBrowser(browser, fixture, async (page) => {
    const decision = fixture.decisions.primary;
    const initial = await reviewExisting(page, fixture, decision, fixture.head);
    await chooseAction(page, 'obsolete');
    await page.getByLabel('Reason').fill('Prepare a real obsolete predecessor');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await adoptExisting(page, fixture.head);
    const obsolete = await submit(page, fixture, 'obsolete', decision, initial);
    const before = await reviewExisting(page, fixture, decision, obsolete.commit, true);
    expect(before.collapsed.status).toBe('obsolete');
    await chooseAction(page, 'reactivate');
    await page.getByLabel('Reason').fill('Restore after browser review');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await adoptExisting(page, obsolete.commit);
    const result = await submit(page, fixture, 'reactivate', decision, before);
    expect(result.request).toMatchObject({ reason: 'Restore after browser review', resolve: false });
    expect(result.obsolete).toBe(false);
    const after = await readInspection(page, decision, result.commit, 'collapsed');
    expect(after.status).toBe('active');
    expect(after.status_heads).not.toEqual(before.collapsed.status_heads);
  }));
});

test('B12 simultaneous conflict candidates require explicit axis reconciliation', async ({ browser }) => {
  test.setTimeout(90_000);
  await withP705Server({ scenarioId: 'B12', seed: 'conflicts' }, async (fixture) => inBrowser(browser, fixture, async (page) => {
    const postedRoutes: string[] = [];
    page.on('request', (request) => {
      if (request.method() === 'POST') postedRoutes.push(new URL(request.url()).pathname);
    });
    const decision = fixture.decisions.conflicted!;
    let head = fixture.head;
    const basis = await reviewExisting(page, fixture, decision, head);
    expect(basis.collapsed.resolution_required).toBe(true);
    for (const axis of ['decision', 'scope', 'domain', 'status'] as const) {
      const candidates: string[] = fixture.conflictHeads![axis];
      expect(candidates.length).toBeGreaterThan(1);
      if (axis === 'decision') {
        expect(basis.collapsed.candidate_records.map((item: Json) => item.record).sort()).toEqual([...candidates].sort());
      }
      const heading = axis === 'decision' ? 'Decision heads' : `${axis[0].toUpperCase()}${axis.slice(1)} heads`;
      await expect(page.locator('#inspector-pane').getByRole('heading', { name: heading })).toBeVisible();
      const axisView = page.locator('#inspector-pane h4').filter({ hasText: heading }).locator('..');
      for (const candidate of candidates) {
        expect(JSON.stringify(basis.collapsed) + JSON.stringify(basis.exploded)).toContain(candidate);
        const card = axisView.locator('article.candidate').filter({ hasText: candidate });
        await expect(card).toBeVisible();
        await expect(card.locator('strong')).not.toBeEmpty();
        await expect(card.locator('p').first()).not.toBeEmpty();
        const record = basis.collapsed.candidate_records?.find((item: Json) => item.record === candidate);
        const scope = basis.collapsed.candidate_scopes?.find((item: Json) => item.connection === candidate);
        const domain = basis.collapsed.candidate_domains?.find((item: Json) => item.connection === candidate);
        const operationItem = basis.exploded.operations?.flatMap((operation: Json) => operation.items).find((item: Json) => item.item === candidate);
        if (axis === 'decision') {
          expect(record).toBeDefined();
          expect(typeof operationItem?.body).toBe('string');
          expect(operationItem.body.trim()).not.toBe('');
          await expect(card).toContainText(record.title);
          await expect(card).toContainText(record.summary);
          await expect(card.locator('pre.body-text')).toHaveText(operationItem.body.trimEnd());
        }
        if (scope) await expect(card).toContainText(scope.applies_to.join(', '));
        if (domain) await expect(card).toContainText(domain.domains.join(', '));
        if (axis === 'status' && operationItem?.metadata?.state) await expect(card).toContainText(operationItem.metadata.state);
      }
    }
    await chooseAction(page, 'amend');
    await page.getByLabel('Title').fill('Simultaneous heads need separate review');
    await page.getByLabel('Change summary').fill('Inspect all simultaneous candidates');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await expect(page.locator('#actions-pane')).toContainText('Original reviewed state');
    for (const candidate of Object.values(fixture.conflictHeads!).flat()) {
      await expect(page.locator('#actions-pane')).toContainText(candidate);
    }
    await expect(page.getByRole('button', { name: 'Submit amend' })).toBeDisabled();

    // Each mutation resolves its own axis only. The four-axis ADR remains a
    // display/review proof; the supported routes act on isolated-axis ADRs.
    const isolated = fixture.decisions.conflictByAxis!;
    const decisionBasis = await reviewExisting(page, fixture, isolated.decision, head);
    expect(decisionBasis.collapsed.record_heads).toHaveLength(2);
    await chooseAction(page, 'amend');
    await page.getByLabel('Title').fill('B12 reviewed decision resolution');
    await page.getByLabel('Summary', { exact: true }).fill('A chosen decision across both heads');
    await page.getByLabel('Body').fill('Resolve the decision after reviewing both candidates.\n');
    await page.getByLabel('Change summary').fill('Reconcile decision heads');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await expect(page.getByRole('button', { name: 'Submit amend' })).toBeDisabled();
    await adoptExisting(page, head);
    const amended = await submit(page, fixture, 'amend', isolated.decision, decisionBasis);
    expect(amended.request.change_summary).toBe('Reconcile decision heads');
    head = amended.commit;
    const resolvedDecision = await readInspection(page, isolated.decision, head, 'collapsed');
    expect(resolvedDecision.record_heads).toHaveLength(1);
    expect(resolvedDecision.title).toBe('B12 reviewed decision resolution');

    const scopeBasis = await reviewExisting(page, fixture, isolated.scope, head);
    expect(scopeBasis.collapsed.scope_heads).toHaveLength(2);
    await chooseAction(page, 'scope');
    await page.getByLabel('Reason').fill('Reconcile reviewed scope heads');
    await page.getByLabel('Change mode').selectOption('reviewed');
    await page.getByLabel('Reviewed set, one per line').fill('src/**\nbrowser-tests/**');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await expect(page.getByRole('button', { name: 'Submit scope' })).toBeDisabled();
    await adoptExisting(page, head);
    const scoped = await submit(page, fixture, 'scope', isolated.scope, scopeBasis);
    expect(scoped.request).toMatchObject({ mode: 'reviewed', patterns: ['src/**', 'browser-tests/**'] });
    head = scoped.commit;
    const resolvedScope = await readInspection(page, isolated.scope, head, 'collapsed');
    expect(resolvedScope.scope_heads).toHaveLength(1);
    expect(resolvedScope.applies_to).toContain('browser-tests/**');

    const domainBasis = await reviewExisting(page, fixture, isolated.domain, head);
    expect(domainBasis.collapsed.domain_heads).toHaveLength(2);
    await chooseAction(page, 'domain');
    await page.getByLabel('Reason').fill('Reconcile reviewed domain heads');
    await page.getByLabel('Change mode').selectOption('reviewed');
    await page.getByLabel('Reviewed set, one per line').fill('runtime\nbrowser');
    await page.getByLabel('Actor ID').fill('p705-browser');
    await expect(page.getByRole('button', { name: 'Submit domain' })).toBeDisabled();
    await adoptExisting(page, head);
    const domained = await submit(page, fixture, 'domain', isolated.domain, domainBasis);
    expect(domained.request).toMatchObject({ mode: 'reviewed', domains: ['runtime', 'browser'] });
    head = domained.commit;
    const resolvedDomain = await readInspection(page, isolated.domain, head, 'collapsed');
    expect(resolvedDomain.domain_heads).toHaveLength(1);
    expect(resolvedDomain.domains).toContain('browser');

    const statusBasis = await reviewExisting(page, fixture, isolated.status, head);
    expect(statusBasis.collapsed.status_heads).toHaveLength(2);
    await chooseAction(page, 'obsolete');
    await page.getByLabel('Reason').fill('Resolve simultaneous status heads');
    await page.getByLabel('Resolve status conflict').check();
    await page.getByLabel('Actor ID').fill('p705-browser');
    await expect(page.getByRole('button', { name: 'Submit obsolete' })).toBeDisabled();
    await adoptExisting(page, head);
    const obsoleted = await submit(page, fixture, 'obsolete', isolated.status, statusBasis);
    expect(obsoleted.request.resolve).toBe(true);
    expect(obsoleted.resolved_status_conflict).toBe(true);
    const final = await readInspection(page, isolated.status, obsoleted.commit, 'collapsed');
    expect(final.status_heads).toHaveLength(1);
    expect(final.resolution_required).toBe(false);
    expect(postedRoutes).toEqual([
      `/api/v1/adrs/${isolated.decision.adr}/amend`,
      `/api/v1/adrs/${isolated.scope.adr}/scope`,
      `/api/v1/adrs/${isolated.domain.adr}/domain`,
      `/api/v1/adrs/${isolated.status.adr}/obsolete`,
    ]);
  }));
});
