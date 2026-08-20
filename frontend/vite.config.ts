// defineConfig comes from vitest/config (not vite) so the `test` block below typechecks; the Vite
// build behaviour is identical.
import { existsSync } from 'node:fs'
import { fileURLToPath, URL } from 'node:url'
import { defineConfig } from 'vitest/config'
import vue from '@vitejs/plugin-vue'
import type { ProxyOptions } from 'vite'

// Sibling checkout of `~/cc-workspace/feijoa` gets hot-reload from the local source; otherwise
// Vite resolves `feijoa` via node_modules (github:schienstockd/feijoa#main). Note the FOUR levels:
// frontend → cecelia-feijoa → cecelia → cc-workspace → feijoa (the cecelia repo has an inner
// cecelia-feijoa/ dir, so feijoa is three "../" hops away, not two).
// See docs/todo/SKETCH_ENGINE_PLAN.md.
const feijoaSibling = fileURLToPath(new URL('../../../feijoa/src/lib/index.ts', import.meta.url))
const feijoaAlias: Record<string, string> = existsSync(feijoaSibling) ? { feijoa: feijoaSibling } : {}

// A BACKEND RESTART IS NOT AN ERROR HERE.
//
// `pixi run dev` supervises the backend, and Settings → System Restart stops and starts it — so every
// dev session has windows of seconds where :8080 is not listening. Vite's default proxy handler prints a
// six-line Node stack per failed request during those windows:
//
//     [vite] http proxy error: /api/runner/status
//     Error: connect ECONNREFUSED 127.0.0.1:8080
//         at TCPConnectWrap.afterConnect [as oncomplete] (node:net:1705:16)
//
// and it is not one request. `TaskRunner.vue` polls `/api/runner/status` on a timer and sits on EVERY
// module page, so a restart scrolls the terminal — which is where the log rail's own output goes. Noise
// that appears on a normal action trains you to ignore the log, and then a real proxy error goes unread.
//
// So: swallow ECONNREFUSED/ECONNRESET with ONE line, and let anything else print as before — a proxy
// error that is not "the backend is not up yet" is still worth a stack. The client also gets an
// immediate 503 rather than a socket that hangs until it times out, so a `catch` runs when it should.
const quietWhenBackendDown: NonNullable<ProxyOptions['configure']> = (proxy) => {
  proxy.on('error', (err: NodeJS.ErrnoException, _req, res) => {
    const expected = err.code === 'ECONNREFUSED' || err.code === 'ECONNRESET'
    if (!expected) { console.error(err); return }
    console.log(`[vite] backend not reachable (${err.code}) — is it restarting?`)
    // `res` is a ServerResponse for /api and a Socket for a /ws upgrade; only the former can answer.
    const r = res as { writableEnded?: boolean; writeHead?: (n: number) => void; end?: (s: string) => void }
    if (r?.writeHead && !r.writableEnded) {
      r.writeHead(503)
      r.end?.('backend not reachable')
    }
  })
}

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
        configure: quietWhenBackendDown,
      },
      '/api': {
        target: 'http://localhost:8080',
        changeOrigin: false,
        configure: quietWhenBackendDown,
      },
    },
  },
})
