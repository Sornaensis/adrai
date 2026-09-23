const { writeFileSync } = require('node:fs');
const { join, basename } = require('node:path');

const knownSpecs = new Set(['p705-read.spec.ts', 'p705-mutations.spec.ts', 'p705-live.spec.ts']);
const knownFailureSources = new Set([...knownSpecs, 'p705-server.ts']);

function safeFailureLocation(test, result) {
  const spec = basename(test.location.file);
  if (result.status === 'passed' || !knownSpecs.has(spec)) return null;
  for (const error of result.errors ?? []) {
    const location = error.location;
    const locationFile = basename(location?.file ?? '');
    if (knownFailureSources.has(locationFile) &&
        Number.isInteger(location.line) && location.line > 0 && location.line <= 100000 &&
        Number.isInteger(location.column) && location.column > 0 && location.column <= 10000) {
      return { file: locationFile, line: location.line, column: location.column };
    }
    const stack = typeof error.stack === 'string' ? error.stack.slice(0, 65536) : '';
    const frame = /(?:^|[\\/])(p705-(?:read|mutations|live)\.spec\.ts|p705-server\.ts):([1-9]\d{0,5}):([1-9]\d{0,4})(?=$|[)\s])/gm;
    for (const match of stack.matchAll(frame)) {
      if (!knownFailureSources.has(match[1])) continue;
      const line = Number(match[2]);
      const column = Number(match[3]);
      if (line <= 100000 && column <= 10000) return { file: match[1], line, column };
    }
  }
  return null;
}

const safeFact = /^(?:P705_CONFLICT_FIXTURE=head=[a-f0-9]{40} four_axes=2,2,2,2 expected_adr_conflicts=5 nonconflict_integrity_errors=0|P705_CONFLICT_(?:SEED|SERVER_READY|PREFLIGHT)_MS=\d{1,6}|P705_WINDOW_FIXTURE=basis=[a-f0-9]{40} head=[a-f0-9]{40} documents=4004 decisions=1001 compiler_errors=0|P705_MUTATION action=(?:create|amend|scope|domain|obsolete|reactivate) status=200 operation=O[A-Z0-9]{26} commit=[a-f0-9]{40} indexed=(?:true|false) publication_warning=(?:true|false)|P705_B13=head:[a-f0-9]{40} invalidations:\d{1,4} writes:\d{1,4}|P705_B14_OLD_READ_ERROR=status:[1-5]\d\d code:[a-z0-9-]{1,64} as_of:(?:[a-f0-9]{40}|[a-z0-9-]{1,64})|P705_B14=old-read:[1-5]\d\d committed:[a-f0-9]{40} operation:O[A-Z0-9]{26} lost-response:[a-f0-9]{40} posts:\d{1,4}|P705_B15=authenticated:\d{1,4} resyncs:\d{1,4} unavailable:\d{1,4} closes:\d{1,4}|P705_LOCK_ACQUIRED=attempts:[1-9]\d{0,2} class:(?:none|sharing|lock) elapsed_ms:\d{1,6}|P705_LOCK_FAILURE=phase:(?:spawn|entry|opened|flushed|ready) cause:(?:timeout|exit|spawn-error|not-live) class:(?:none|sharing|lock|path|access|other) attempts:\d{1,3} exhausted:[01] elapsed_ms:\d{1,6} exit:(?:none|\d{1,3}))$/;
const safeConflictFailure = /^P705_CONFLICT_(?:EMITTER_STAGE=(?:root|discovery|head|snapshot|typed|create|switch|amend|scope|domain|obsolete|merge|ancestry|topology|unknown)|SEED_FAILURE=(?:executable|initial-head|emitter|emitter-json|emitter-shape|head|ancestry|doctor))$/;

class P705Reporter {
  constructor() {
    this.cases = [];
    this.facts = [];
    this.streams = new Map();
    this.globalErrors = 0;
  }

  onStdOut(chunk, test) {
    this.acceptSafeLines(chunk, test?.id ?? 'global');
  }

  onStdErr(chunk, test) {
    this.acceptSafeLines(chunk, test?.id ?? 'global-stderr');
  }

  acceptSafeLines(chunk, key) {
    const state = this.streams.get(key) ?? { line: '', dropping: false };
    for (const character of chunk.toString('utf8')) {
      if (character === '\n') {
        const line = state.line.replace(/\r$/, '');
        if (!state.dropping && this.facts.length < 512 && (safeFact.test(line) || safeConflictFailure.test(line))) this.facts.push(line);
        state.line = '';
        state.dropping = false;
      } else if (!state.dropping) {
        if (state.line.length < 512) state.line += character;
        else { state.line = ''; state.dropping = true; }
      }
    }
    this.streams.set(key, state);
  }

  onError() {
    this.globalErrors += 1;
  }

  onTestEnd(test, result) {
    const match = /^B(0[1-9]|1[0-5]) /.exec(test.title);
    this.cases.push({
      id: match?.[0].trim() ?? null,
      title: test.title,
      spec: basename(test.location.file),
      status: result.status,
      expected_status: test.expectedStatus,
      retry: result.retry,
      duration_ms: result.duration,
      failure_location: safeFailureLocation(test, result),
    });
  }

  onEnd(result) {
    const directory = process.env.P705_EVIDENCE_DIR;
    if (!directory) throw new Error('P7-05 reporter requires owned evidence directory');
    writeFileSync(join(directory, 'execution.json'), JSON.stringify({
      schema: 'adrai/p705-browser-execution/v1',
      status: result.status,
      global_errors: this.globalErrors,
      cases: this.cases,
      safe_facts: this.facts,
    }), { encoding: 'utf8', flag: 'wx' });
  }

  printsToStdio() { return false; }
}

module.exports = P705Reporter;
