// Types for chromium.mjs, so frontend/vitest.browser.config.ts (type-checked by vue-tsc) can import
// `findChromium` from the one place the repo picks its system Chromium.
export const repo: string
export function commit(): string
export const chromium: typeof import('playwright-core').chromium
export function findChromium(): string
