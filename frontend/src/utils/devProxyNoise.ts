// A BACKEND RESTART IS NOT AN ERROR — the predicate, kept out of `vite.config.ts` so it is testable.
//
// `pixi run dev` supervises the backend and Settings → System → Restart stops and starts it, so every
// dev session has windows of seconds where :8080 is not listening. Vite prints a six-line Node stack
// per failed proxied request in those windows, and it is not one request: `TaskRunner.vue` polls
// `/api/runner/status` on a timer from EVERY module page, and the app holds a `/ws` socket that
// reconnects. Noise that appears on a normal action trains you to ignore the log, and then a real
// proxy error goes unread.
//
// **Why this is a logger filter and not a proxy `error` handler.** The first attempt attached a second
// `error` listener through `server.proxy.configure`, which cannot work: Vite's `proxyMiddleware` calls
// `opts.configure(proxy)` and only THEN attaches its own logging listener, so ours ran first and Vite's
// still printed. Node calls every listener; no ordering suppresses a sibling. The log line is the thing
// we want to change, so the logger is where to change it.
//
// Vite hands the original error to the logger (`{ timestamp: true, error: err }`), so the discriminator
// is the errno CODE, not the wording of a message that may be reformatted upstream. The message is
// still checked for `proxy error` so that an unrelated ECONNREFUSED — something that is not the
// backend-down window this exists for — keeps its stack.

const QUIET_CODES = new Set(['ECONNREFUSED', 'ECONNRESET'])

// Vite has THREE spellings, and the test caught this: `http proxy error: <url>`, `ws proxy error:` and
// `ws proxy socket error:` — the last does not contain the substring `proxy error`, so a plain
// `includes` would have left the socket variant printing a stack on every backend restart.
const PROXY_ERROR = /proxy (?:socket )?error/

/**
 * The one line to print instead, or `null` to let the message through untouched.
 *
 * `msg` arrives with ANSI colour escapes around the headline (picocolors), which is why this matches a
 * substring rather than anchoring: `\x1b[31mws proxy error:\x1b[39m\nError: connect ECONNREFUSED …`.
 */
export function backendDownNoise(msg: string, err?: { code?: string } | null): string | null {
  if (!err?.code || !QUIET_CODES.has(err.code)) return null
  if (!PROXY_ERROR.test(msg)) return null
  return `backend not reachable (${err.code}) — is it restarting?`
}
