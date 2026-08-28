// One home for the backend service-control endpoints — the Pluto notebooks server and the napari
// bridge. Both the Settings service panel (SettingsModule) and the Notebooks page (NotebooksModule)
// drive these; route through here so an endpoint string / request shape lives in exactly ONE place
// (the same reason app quit goes through the appControl store). App-level lifecycle (quit / update /
// dev restart) stays in appControl — this is only the per-service start/stop/restart controls.

/**
 * A failed service call. `code` is the backend's machine-readable reason (see docs/API.md); a caller
 * that must react differently per reason — a different severity, a different label — switches on
 * `code`, NEVER on the message text, which is prose and free to change.
 */
export interface SvcError extends Error {
  status: number
  code?: string
}

/** POST JSON to a service endpoint. Returns the parsed body; throws `SvcError` (server message |
 *  HTTP n, plus `status`/`code`) on a non-2xx response so callers can surface failures in their
 *  own UI state.
 *
 *  `timeoutMs` bounds the wait. Worth setting for anything whose UI shows a busy state: a request
 *  that never settles leaves that state stuck with no way out, and "still working" is the one
 *  failure mode a user cannot distinguish from a hang. Rejects with `code: 'timeout'`. */
export async function svcPost(url: string, body?: object, timeoutMs?: number): Promise<any> {
  let res: Response
  try {
    res = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body ?? {}),
      signal: timeoutMs ? AbortSignal.timeout(timeoutMs) : undefined,
    })
  } catch (e) {
    // an abort is our own deadline, and must not read like a network error
    if (timeoutMs && e instanceof Error && (e.name === 'TimeoutError' || e.name === 'AbortError')) {
      const t = new Error(`No answer within ${Math.round(timeoutMs / 1000)} s`) as SvcError
      t.status = 0
      t.code = 'timeout'
      throw t
    }
    throw e
  }
  const data = await res.json().catch(() => ({}))
  if (!res.ok) {
    const err = new Error((data as any)?.error ?? `HTTP ${res.status}`) as SvcError
    err.status = res.status
    err.code = (data as any)?.code
    throw err
  }
  return data
}

/** Pluto notebooks server (port 7660). */
export const notebooksApi = {
  launch: (projectUid: string) => svcPost('/api/notebooks/launch', { projectUid }),
  restart: (projectUid: string) => svcPost('/api/notebooks/restart', { projectUid }),
  shutdown: () => svcPost('/api/notebooks/shutdown'),
}

/** Napari bridge (port 7655). */
export const napariApi = {
  restart: () => svcPost('/api/napari/restart'),
  close: () => svcPost('/api/napari/close'),
}

/**
 * Task-preview worker (port 7656) — the resident process that runs a task's real compute over the
 * region the viewer is showing.
 *
 * `status` is the ONE way to learn which image the viewer has open: the backend tracks it, and a caller
 * that guesses instead acts on an image the user is not looking at. `run` deliberately passes
 * `imageUid` for the backend to CHECK, not to select — a mismatch is a 409, never a silent switch.
 */
export const previewApi = {
  status: async (): Promise<any> => {
    const res = await fetch('/api/preview/status')
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    return res.json()
  },
  start: () => svcPost('/api/preview/start'),
  /** Stop the worker AND sweep the preview labels store under `taskDir`. `taskDir` is optional for
   *  callers that don't know the open image (Settings module), but a normal toggle-off from a viewer
   *  window MUST pass it so the scratch bytes don't outlive the toggle. */
  stop: (taskDir?: string) => svcPost('/api/preview/stop', { taskDir }),
  /**
   * One plane of the task's real compute. Deadlined because the control shows "Previewing…" for the
   * whole round trip and the scheduler treats a run as in flight until it settles — so a request that
   * never comes back wedges both, permanently, with the mask of some earlier run still on screen. The
   * window is far above a real preview (warm 0.14–0.9 s, cold 2048² a few seconds; the worker's 17.7 s
   * import shows up as an immediate `starting` reply, not a slow one), so hitting it means stuck.
   *
   * `region` and the open-image fields come from the browser viewer (P7): the API uses them as source
   * of truth for what's on screen rather than asking napari.
   */
  run: (body: {
    projectUid: string
    imageUid: string
    valueName: string
    funName: string
    params: object
    region: object
    zarrPath?: string
    taskDir?: string
  }) => svcPost('/api/preview/run', body, PREVIEW_RUN_TIMEOUT_MS),
}

export const PREVIEW_RUN_TIMEOUT_MS = 90_000

/** Per-project observer session: the assistant session id + cumulative token totals. */
export interface ObserverPass {
  at: string
  trigger: string          // 'manual' | 'auto'
  model: string
  ok: boolean
  appended: boolean        // did it write a [Claude] lab-log entry this pass?
  inputTokens: number
  outputTokens: number
  note: string             // the assistant's own verdict/reasoning for the pass
}

export interface ObserverSession {
  sessionId: string
  inputTokens: number
  outputTokens: number
  turns: number
  passes?: ObserverPass[]  // activity log, newest-first
}

/** Is the user's OWN terminal set up with the observer MCP? `state`: 'missing' (never registered),
 *  'stale' (registered but pointing at another interpreter/port — would fail silently), 'current'. */
// 'shadowed' = registered correctly, but a per-folder (`local`-scope) entry overrides it in the dirs
// named by `shadowedDirs` — so the user's terminal still has no tools. Not ready, same as 'stale'.
export interface ObserverTerminal {
  state: 'missing' | 'stale' | 'shadowed' | 'current'
  ready: boolean
  shadowedDirs?: string[]
}

/** In-app AI observer — needs an assistant CLI (e.g. Claude Code) on the machine. */
export const observerApi = {
  /** Availability (drives the disabled-with-why UI) + this project's session/usage when a uid is
   *  given. Never throws → unavailable on error. */
  status: async (projectUid?: string): Promise<{ available: boolean; models?: string[]; defaultModel?: string; prompt?: string; mcpConfigPath?: string; terminal?: ObserverTerminal; session?: ObserverSession }> => {
    try {
      const q = projectUid ? `?projectUid=${encodeURIComponent(projectUid)}` : ''
      const res = await fetch(`/api/observer/status${q}`)
      return res.ok ? await res.json() : { available: false }
    } catch { return { available: false } }
  },
  /** One-shot: the assistant reviews the project and may append a [Claude] lab-log note. `model` is a
   *  CLI alias (haiku|sonnet|opus); `trigger` is 'manual' (button) or 'auto' (Watch). Returns
   *  { ok, available, model, trigger, message, error, appended, appendedLine, inputTokens,
   *    outputTokens, session }. */
  feedback: (projectUid: string, model?: string, trigger: 'manual' | 'auto' = 'manual') =>
    svcPost('/api/observer/feedback', { projectUid, model, trigger }),
  /** Clear context: reset the project's session + token totals. Returns { ok, session }. */
  clear: (projectUid: string) => svcPost('/api/observer/clear', { projectUid }),
  /** One-click terminal setup: register (or re-sync) the observer MCP in the user's own Claude Code
   *  config so plain `claude` has the tools. Idempotent. Returns { ok, available, name, message, error }. */
  register: () => svcPost('/api/observer/register', {}),
}
