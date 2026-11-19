// Browser tests: `*.browser.test.ts`, mounted in a real Chromium (vitest browser mode, playwright
// provider) because the Node suite has no layout engine — a pinned header or a frozen column is only
// right or wrong once the browser has placed it. `npm run test:browser`. Plan:
// docs/todo/GUI_TESTING_PLAN.md (Decisions 1, 3, 4).
//
// It takes the app's own config — the Vue plugin with `rangeFillTransform`, the feijoa alias, the
// logger — WITHOUT `server`. That block proxies /api and /ws to the backend on CECELIA_PORT ?? 8080
// (the maintainer's live one) and pins port 5173 with `strictPort` (his dev server's). A browser test
// must reach neither; `src/test/browser/vitestBrowserConfig.test.ts` keeps this file honest.
//
// The browser is the SYSTEM Chromium the page scripts already use (`scripts/lib/chromium.mjs`,
// CECELIA_CHROMIUM picks it) — no browser download.
import { defineConfig } from 'vitest/config'
import { playwright } from '@vitest/browser-playwright'
import appConfig from './vite.config.ts'
import { findChromium } from '../scripts/lib/chromium.mjs'

const { server: _devServerOnly, test: _nodeTestOnly, ...shared } = appConfig

export default defineConfig({
  ...shared,
  test: {
    css: true,
    include: ['src/**/*.browser.test.ts'],
    setupFiles: ['src/test/browser/setup.ts'],
    browser: {
      enabled: true,
      headless: true,
      provider: playwright({ launchOptions: { executablePath: findChromium() } }),
      instances: [{ browser: 'chromium' }],
      // fixed, so geometry assertions don't depend on the runner's screen
      viewport: { width: 1280, height: 800 },
    },
  },
})
