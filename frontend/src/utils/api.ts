/**
 * THE way the frontend calls the backend over HTTP. Every call goes through `apiFetch` (or one of the
 * shapes on top of it), so one place decides what a failed request means:
 *
 * - a non-2xx reply becomes an `ApiError` carrying the status and the server's own `error` text
 *   (the backend answers `{error: "..."}`), never a body parsed as if it had succeeded — a JSON error
 *   read as `Float32` gives garbage points, read as JSON gives `undefined` fields;
 * - a request that never got a reply (backend down / restarting) is an `ApiError` with status 0;
 * - every failure is logged once to the log rail (`source: 'api'`), so a call site that shows nothing
 *   still leaves a trace. `quiet` turns that off for statuses the caller expects (a 404 probe, a 409
 *   it resolves) — the caller still gets the error;
 * - `timeoutMs` running out is a failure like any other: `ApiError` status 0, `code: 'timeout'`,
 *   logged — a request that never settles leaves a busy state stuck, and "still working" is the one
 *   failure a user cannot tell from a hang;
 * - an abort through the caller's own `signal` is re-thrown as the platform `AbortError` and not
 *   logged: the caller asked for it.
 *
 * No retries: a POST retried after a lost reply runs twice, and a 4xx answers the same again. A
 * restarting backend is handled where it is visible (`appControl` restart, the WS reconnect).
 *
 * Two ways to take the result: `await getJson(...)` throws, `await tryApi(getJson(...))` returns
 * `{ok, data}` / `{ok: false, error, status}` for a caller that shows the message itself.
 *
 * Streaming reads (`body.getReader()`) take the checked `Response` from `apiFetch` and read it.
 */
import { useLogStore } from '../stores/log'

/** A call that reaches the backend: a bare `fetch(` or any of this module's shapes. For source-scanning
 *  guards (`continuousControls.test.ts`, `dataRefreshCoverage.test.ts`) — they matched `fetch(` alone,
 *  and would go blind as calls move onto this client. */
export const BACKEND_CALL = /\b(?:fetch|apiFetch|getJson|postJson|postJsonOrNull|getBuffer|getText|svcPost)\s*(?:<[^>()]*>)?\s*\(/

type QueryValue = string | number | boolean | null | undefined
export type Query = Record<string, QueryValue | QueryValue[]>

export interface ApiOptions {
  method?: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE'
  /** Appended as a query string; `null`/`undefined` values are left out, arrays repeat the key. */
  query?: Query
  /** A JSON body (sets `Content-Type`). Use `body` for FormData / Blob / bytes instead. */
  json?: unknown
  body?: BodyInit
  headers?: Record<string, string>
  signal?: AbortSignal
  /** Give up after this long → `ApiError` with `code: 'timeout'`. Combined with `signal` when both are given. */
  timeoutMs?: number
  /** Don't log the failure: `true` for any status, or the statuses this caller handles itself. */
  quiet?: boolean | number[]
  keepalive?: boolean
  /** The browser HTTP cache mode — `'no-store'` for a poll or a read that must never be served stale. */
  cache?: RequestCache
}

export class ApiError extends Error {
  /** HTTP status; 0 = no reply (network error, backend down). */
  readonly status: number
  readonly route: string
  /** The backend's machine-readable reason (`{code: "stale_version"}`), or `'timeout'`. Switch on
   *  this, never on `message`, which is prose and free to change. */
  readonly code?: string
  /** The parsed reply body (JSON when it was JSON, else the text) — e.g. a 409's current state. */
  readonly body: unknown
  constructor(route: string, status: number, message: string, body: unknown = null, code?: string) {
    super(message)
    this.name = 'ApiError'
    this.route = route
    this.status = status
    this.body = body
    const c = code ?? (body && typeof body === 'object' ? (body as Record<string, unknown>).code : undefined)
    if (typeof c === 'string') this.code = c
  }
}

/** The caller's own abort (its `signal`) — not a failure. */
export function isAbort(e: unknown): boolean {
  return e instanceof DOMException && e.name === 'AbortError'
}

/** The message to show for any thrown value: the server's text for an `ApiError`. */
export function errorMessage(e: unknown): string {
  if (e instanceof Error) return e.message
  return String(e)
}

/** The server's refusal text, or `fallback` when the request never got an answer (status 0, or not
 *  an `ApiError` at all) — for a message line that says "it said no" apart from "it wasn't there". */
export function refusalMessage(e: unknown, fallback: string): string {
  return e instanceof ApiError && e.status > 0 ? e.message : fallback
}

export function withQuery(path: string, query?: Query): string {
  if (!query) return path
  const q = new URLSearchParams()
  for (const [k, v] of Object.entries(query)) {
    for (const x of Array.isArray(v) ? v : [v]) if (x !== null && x !== undefined) q.append(k, String(x))
  }
  const s = q.toString()
  return s ? `${path}${path.includes('?') ? '&' : '?'}${s}` : path
}

function routeOf(path: string): string {
  return path.split('?')[0]
}

function report(err: ApiError, method: string, quiet: ApiOptions['quiet']) {
  if (quiet === true || (Array.isArray(quiet) && quiet.includes(err.status))) return
  try {
    useLogStore().warn(`${method} ${err.route} failed — ${err.message}`, { source: 'api' })
  } catch { /* no Pinia (a unit test, a worker): the caller still gets the error */ }
}

async function readBody(res: Response): Promise<unknown> {
  const text = await res.text().catch(() => '')
  if (!text) return null
  try { return JSON.parse(text) } catch { return text }
}

function messageOf(body: unknown, res: Response): string {
  if (body && typeof body === 'object') {
    const b = body as Record<string, unknown>
    for (const k of ['error', 'message', 'detail']) if (typeof b[k] === 'string' && b[k]) return b[k] as string
  }
  if (typeof body === 'string' && body.trim()) return body.trim().slice(0, 300)
  return `HTTP ${res.status}${res.statusText ? ` ${res.statusText}` : ''}`
}

function signalFor(opts: ApiOptions): AbortSignal | undefined {
  if (!opts.timeoutMs) return opts.signal
  const t = AbortSignal.timeout(opts.timeoutMs)
  if (!opts.signal) return t
  if (typeof AbortSignal.any === 'function') return AbortSignal.any([opts.signal, t])
  const c = new AbortController()
  for (const s of [opts.signal, t]) {
    if (s.aborted) { c.abort(s.reason); break }
    s.addEventListener('abort', () => c.abort(s.reason), { once: true })
  }
  return c.signal
}

/** The checked request: resolves to a 2xx `Response`, throws `ApiError` (logged) otherwise. */
export async function apiFetch(path: string, opts: ApiOptions = {}): Promise<Response> {
  const method = opts.method ?? (opts.json !== undefined || opts.body !== undefined ? 'POST' : 'GET')
  const url = withQuery(path, opts.query)
  const headers: Record<string, string> = { ...opts.headers }
  let body = opts.body
  if (opts.json !== undefined) {
    headers['Content-Type'] ??= 'application/json'
    body = JSON.stringify(opts.json)
  }
  let res: Response
  try {
    res = await fetch(url, { method, headers, body, signal: signalFor(opts), keepalive: opts.keepalive, cache: opts.cache })
  } catch (e) {
    if (isAbort(e)) throw e
    const timedOut = !!opts.timeoutMs && e instanceof DOMException && e.name === 'TimeoutError'
    const err = timedOut
      ? new ApiError(routeOf(path), 0, `No answer within ${Math.round(opts.timeoutMs! / 1000)} s`, null, 'timeout')
      : new ApiError(routeOf(path), 0, `no reply from the backend (${errorMessage(e)})`)
    report(err, method, opts.quiet)
    throw err
  }
  if (res.ok) return res
  const parsed = await readBody(res)
  const err = new ApiError(routeOf(path), res.status, messageOf(parsed, res), parsed)
  report(err, method, opts.quiet)
  throw err
}

async function json<T>(res: Response): Promise<T> {
  const text = await res.text()
  return (text ? JSON.parse(text) : null) as T
}

/** GET → parsed JSON (`null` for an empty reply). */
export async function getJson<T = unknown>(path: string, opts: Omit<ApiOptions, 'json' | 'body'> = {}): Promise<T> {
  return json<T>(await apiFetch(path, { ...opts, method: opts.method ?? 'GET' }))
}

/** POST a JSON body → parsed JSON reply (`null` for an empty reply). */
export async function postJson<T = unknown>(path: string, body: unknown, opts: Omit<ApiOptions, 'json' | 'body'> = {}): Promise<T> {
  return json<T>(await apiFetch(path, { ...opts, method: opts.method ?? 'POST', json: body ?? {} }))
}

/** Binary reply (plot data, membership, images) — only ever a 2xx body. */
export async function getBuffer(path: string, opts: ApiOptions = {}): Promise<ArrayBuffer> {
  return (await apiFetch(path, opts)).arrayBuffer()
}

/** Text reply (logs, markdown, CSV). */
export async function getText(path: string, opts: ApiOptions = {}): Promise<string> {
  return (await apiFetch(path, opts)).text()
}

export type ApiResult<T> =
  | { ok: true; data: T }
  | { ok: false; error: string; status: number; code?: string; body: unknown }

/** For a caller that shows the message itself: never throws, except an abort (re-thrown). */
export async function tryApi<T>(p: Promise<T>): Promise<ApiResult<T>> {
  try {
    return { ok: true, data: await p }
  } catch (e) {
    if (isAbort(e)) throw e
    if (e instanceof ApiError) return { ok: false, error: e.message, status: e.status, code: e.code, body: e.body }
    return { ok: false, error: errorMessage(e), status: 0, body: null }
  }
}
