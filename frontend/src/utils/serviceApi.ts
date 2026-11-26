// One home for the backend service-control endpoints — the Pluto notebooks server and the task-
// preview worker. Both the Settings service panel (SettingsModal) and the Notebooks page
// (NotebooksModule) drive these; route through here so an endpoint string / request shape lives in
// exactly ONE place (the same reason app quit goes through the appControl store). App-level
// lifecycle (quit / update / dev restart) stays in appControl — this is only the per-service
// start/stop/restart controls.
import type { AiSetBy } from './aiSwitch'
import { getJson, postJson, tryApi, type ApiError } from './api'

/**
 * A failed service call — the client's `ApiError` (utils/api.ts): `status` (0 = no reply), `code` the
 * backend's machine-readable reason (see docs/API.md) or `'timeout'`. A caller that must react
 * differently per reason — a different severity, a different label — switches on `code`, NEVER on
 * the message text, which is prose and free to change.
 */
export type SvcError = ApiError

/** POST JSON to a service endpoint. Returns the parsed body (`{}` for an empty reply); throws
 *  `SvcError` (server message | HTTP n, plus `status`/`code`) on a non-2xx response so callers can
 *  surface failures in their own UI state.
 *
 *  `timeoutMs` bounds the wait. Worth setting for anything whose UI shows a busy state: a request
 *  that never settles leaves that state stuck with no way out, and "still working" is the one
 *  failure mode a user cannot distinguish from a hang. Rejects with `code: 'timeout'`. */
export async function svcPost(url: string, body?: object, timeoutMs?: number): Promise<any> {
  return (await postJson(url, body ?? {}, { timeoutMs })) ?? {}
}

/** Pluto notebooks server (port 7660). */
export const notebooksApi = {
  launch: (projectUid: string) => svcPost('/api/notebooks/launch', { projectUid }),
  restart: (projectUid: string) => svcPost('/api/notebooks/restart', { projectUid }),
  /** `force` also stops another Cecelia's server holding our port — only on the user's explicit ask. */
  shutdown: (force = false) => svcPost('/api/notebooks/shutdown', force ? { force: true } : undefined),
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
  /** Polled (Settings every 4 s, the preview store on each schedule) — quiet; callers catch. */
  status: (): Promise<any> => getJson('/api/preview/status', { quiet: true }),
  /** Warm the worker. `funName`/`params` name the task about to be previewed, so the worker starts in
   *  the env its model needs (cellpose 3 models run in `cellpose-v3`) rather than paying a second
   *  warm-up switching over on the first request. */
  start: (ctx?: { funName?: string; params?: Record<string, unknown> | null }) =>
    svcPost('/api/preview/start', { funName: ctx?.funName, params: ctx?.params ?? undefined }),
  /** Stop the worker AND sweep the preview labels store under `taskDir`. `taskDir` is optional for
   *  callers that don't know the open image (Settings module), but a normal toggle-off from a viewer
   *  window MUST pass it so the scratch bytes don't outlive the toggle. */
  stop: (taskDir?: string) => svcPost('/api/preview/stop', { taskDir }),
  /** Stop another Cecelia's worker holding our port — only on the user's explicit, confirmed ask. */
  stopForeign: () => svcPost('/api/preview/stop', { force: true }),
  /**
   * One plane of the task's real compute. Deadlined because the control shows "Previewing…" for the
   * whole round trip and the scheduler treats a run as in flight until it settles — so a request that
   * never comes back wedges both, permanently, with the mask of some earlier run still on screen. The
   * window is far above a real preview (warm 0.14–0.9 s, cold 2048² a few seconds; the worker's 17.7 s
   * import shows up as an immediate `starting` reply, not a slow one), so hitting it means stuck.
   *
   * `region` and the open-image fields come from the browser viewer (P7): the API uses them as source
   * of truth for what's on screen.
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
  /** Availability (drives the disabled-with-why UI) + where the CLI was found, the model allow-list, and whether the user's own
   *  terminal is set up. Never throws → unavailable on error. */
  status: async (): Promise<{ available: boolean; cliPath?: string; models?: string[]; defaultModel?: string; mcpConfigPath?: string; terminal?: ObserverTerminal; ai?: { enabled: boolean; setBy: AiSetBy } }> => {
    const r = await tryApi(getJson<any>('/api/observer/status', { quiet: true }))
    return r.ok ? r.data : { available: false }
  },
  /** One-click terminal setup: register (or re-sync) the observer MCP in the user's own Claude Code
   *  config so plain `claude` has the tools. Idempotent. Returns { ok, available, name, message, error }. */
  register: () => svcPost('/api/observer/register', {}),
}
