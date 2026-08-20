import { describe, it, expect } from 'vitest'
import { backendDownNoise } from './devProxyNoise'

// The exact strings Vite's `proxyMiddleware` builds (`http proxy error: <url>` / `ws proxy error:` /
// `ws proxy socket error:`), colour escapes included — those escapes are why the check is a substring
// match and not an anchored one.
const HTTP_MSG = '\x1b[31mhttp proxy error: /api/runner/status\x1b[39m\nError: connect ECONNREFUSED 127.0.0.1:8080'
const WS_MSG = '\x1b[31mws proxy error:\x1b[39m\nError: connect ECONNREFUSED 127.0.0.1:8080'
const WS_SOCK = '\x1b[31mws proxy socket error:\x1b[39m\nError: read ECONNRESET'

describe('backendDownNoise', () => {
  it('quiets the http proxy stack when the backend is not listening', () => {
    expect(backendDownNoise(HTTP_MSG, { code: 'ECONNREFUSED' }))
      .toBe('backend not reachable (ECONNREFUSED) — is it restarting?')
  })

  // The case the first fix missed entirely: /ws goes through the SAME logger but a different branch of
  // Vite's error handler, so a fix aimed at the http path left this one printing on every restart.
  it('quiets the ws proxy stack too', () => {
    expect(backendDownNoise(WS_MSG, { code: 'ECONNREFUSED' })).toBeTruthy()
    expect(backendDownNoise(WS_SOCK, { code: 'ECONNRESET' })).toBeTruthy()
  })

  it('lets a proxy error that is NOT the backend-down window keep its stack', () => {
    expect(backendDownNoise(HTTP_MSG, { code: 'EHOSTUNREACH' })).toBeNull()
    expect(backendDownNoise(HTTP_MSG, { code: 'ETIMEDOUT' })).toBeNull()
  })

  it('never swallows a message that is not a proxy error', () => {
    // an ECONNREFUSED from somewhere else in Vite is still worth reading
    expect(backendDownNoise('Error: connect ECONNREFUSED 127.0.0.1:9999', { code: 'ECONNREFUSED' })).toBeNull()
    expect(backendDownNoise('[vite] Internal server error', { code: 'ECONNREFUSED' })).toBeNull()
  })

  it('is a no-op with no error attached', () => {
    expect(backendDownNoise(HTTP_MSG)).toBeNull()
    expect(backendDownNoise(HTTP_MSG, null)).toBeNull()
    expect(backendDownNoise(HTTP_MSG, {})).toBeNull()
  })
})
