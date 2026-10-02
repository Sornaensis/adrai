import { defineConfig } from '@playwright/test';
import { join } from 'node:path';

if (!process.env.ADRAI_BROWSER_OUTPUT) throw new Error('Run npm test to keep browser output outside the repository.');

export default defineConfig({
  testDir: './workflows',
  outputDir: join(process.env.ADRAI_BROWSER_OUTPUT, 'playwright'),
  reporter: 'list',
  workers: 1,
  timeout: 0,
  expect: { timeout: 0 },
  use: { actionTimeout: 0, navigationTimeout: 0, screenshot: 'off', trace: 'off' },
});
