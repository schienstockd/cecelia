// One home for the backend service-control endpoints — the Pluto notebooks server and the napari
// bridge. Both the Settings service panel (SettingsModule) and the Notebooks page (NotebooksModule)
// drive these; route through here so an endpoint string / request shape lives in exactly ONE place
// (the same reason app quit goes through the appControl store). App-level lifecycle (quit / update /
// dev restart) stays in appControl — this is only the per-service start/stop/restart controls.

/** POST JSON to a service endpoint. Returns the parsed body; throws Error(server message | HTTP n)
 *  on a non-2xx response so callers can surface failures in their own UI state. */
export async function svcPost(url: string, body?: object): Promise<any> {
  const res = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body ?? {}),
  })
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as any)?.error ?? `HTTP ${res.status}`)
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

/** In-app AI observer — needs an assistant CLI (e.g. Claude Code) on the machine. */
export const observerApi = {
  /** Availability (drives the disabled-with-why UI) + this project's session/usage when a uid is
   *  given. Never throws → unavailable on error. */
  status: async (projectUid?: string): Promise<{ available: boolean; models?: string[]; defaultModel?: string; prompt?: string; session?: ObserverSession }> => {
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
}
