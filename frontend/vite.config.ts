// defineConfig comes from vitest/config (not vite) so the `test` block below typechecks; the Vite
// build behaviour is identical.
import { defineConfig } from 'vitest/config'
import vue from '@vitejs/plugin-vue'
import { fileURLToPath, URL } from 'node:url'

export default defineConfig({
  plugins: [vue()],
  resolve: {
    alias: {
      // Sibling play repo: https://github.com/schienstockd/feijoa
      // Cecelia imports feijoa's source directly. Feijoa's own node_modules (roughjs,
      // animejs) is used at bundle time — Vite walks the resolution chain from the
      // aliased file's location. Run `npm install` in feijoa once; edits then hot-
      // reload through cecelia's Vite. Vue is deduped so both apps share one instance.
      // See docs/todo/SKETCH_ENGINE_PLAN.md.
      feijoa: fileURLToPath(new URL('../../feijoa/src/lib/index.ts', import.meta.url)),
    },
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
