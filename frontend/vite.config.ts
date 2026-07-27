// defineConfig comes from vitest/config (not vite) so the `test` block below typechecks; the Vite
// build behaviour is identical.
import { existsSync } from 'node:fs'
import { fileURLToPath, URL } from 'node:url'
import { defineConfig } from 'vitest/config'
import vue from '@vitejs/plugin-vue'

// Sibling checkout of `~/cc-workspace/feijoa` gets hot-reload from the local source; otherwise
// Vite resolves `feijoa` via node_modules (github:schienstockd/feijoa#main). Note the FOUR levels:
// frontend → cecelia-pineapple → cecelia → cc-workspace → feijoa (the cecelia repo has an inner
// cecelia-pineapple/ dir, so feijoa is three "../" hops away, not two).
// See docs/todo/SKETCH_ENGINE_PLAN.md.
const feijoaSibling = fileURLToPath(new URL('../../../feijoa/src/lib/index.ts', import.meta.url))
const feijoaAlias: Record<string, string> = existsSync(feijoaSibling) ? { feijoa: feijoaSibling } : {}

export default defineConfig({
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
      '/ws': {
        target: 'ws://localhost:8080',
        ws: true,
        changeOrigin: false,
      },
      '/api': {
        target: 'http://localhost:8080',
        changeOrigin: false,
      },
    },
  },
})
