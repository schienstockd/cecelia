// defineConfig comes from vitest/config (not vite) so the `test` block below typechecks; the Vite
// build behaviour is identical.
import { existsSync } from 'node:fs'
import { fileURLToPath, URL } from 'node:url'
import { defineConfig } from 'vitest/config'
import vue from '@vitejs/plugin-vue'
import { createLogger } from 'vite'
import { backendDownNoise } from './src/utils/devProxyNoise'

// Sibling checkout of `~/cc-workspace/feijoa` gets hot-reload from the local source; otherwise
// Vite resolves `feijoa` via node_modules (github:schienstockd/feijoa#main). Note the FOUR levels:
// frontend → cecelia-feijoa → cecelia → cc-workspace → feijoa (the cecelia repo has an inner
// cecelia-feijoa/ dir, so feijoa is three "../" hops away, not two).
// See docs/todo/SKETCH_ENGINE_PLAN.md.
const feijoaSibling = fileURLToPath(new URL('../../../feijoa/src/lib/index.ts', import.meta.url))
const feijoaAlias: Record<string, string> = existsSync(feijoaSibling) ? { feijoa: feijoaSibling } : {}

// A BACKEND RESTART IS NOT AN ERROR HERE — see `src/utils/devProxyNoise.ts` for why, and why the
// filter sits on the LOGGER rather than on a proxy `error` listener (the first attempt did the latter
// and could not work: Vite attaches its own logging listener after ours, and both fire).
//
// Wrapping the logger and not replacing it: `createLogger()` is Vite's own, so `info`/`warn`/`warnOnce`/
// `clearScreen`/`hasErrorLogged` keep their real behaviour and only the one message class changes.
// Vite's proxy handler still answers the request (502 for /api, socket end for /ws), so a `catch` in the
// client runs when it should — that was never the logging's job.
const logger = createLogger()
const printError = logger.error.bind(logger)
logger.error = (msg, opts) => {
  const quiet = backendDownNoise(msg, opts?.error as NodeJS.ErrnoException | undefined)
  if (quiet === null) return printError(msg, opts)
  logger.info(quiet, { timestamp: true })
}

export default defineConfig({
  customLogger: logger,
  plugins: [vue()],
  resolve: {
    alias: feijoaAlias,
    // feijoa marks vue as peer; keep a single Vue instance across the two entry points.
    dedupe: ['vue'],
  },
  // Vitest stubs CSS imports to '' by default, which would make the design-token guard in
  // utils/cssTokens.test.ts read an empty style.css and pass vacuously. No test mounts components,
  // so processing CSS costs nothing else.
  test: { css: true },
  server: {
    proxy: {
      '/ws': { target: 'ws://localhost:8080', ws: true, changeOrigin: false },
      '/api': { target: 'http://localhost:8080', changeOrigin: false },
    },
  },
})
