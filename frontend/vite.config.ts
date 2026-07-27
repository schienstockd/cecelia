// defineConfig comes from vitest/config (not vite) so the `test` block below typechecks; the Vite
// build behaviour is identical.
import { defineConfig } from 'vitest/config'
import vue from '@vitejs/plugin-vue'

export default defineConfig({
  plugins: [vue()],
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
