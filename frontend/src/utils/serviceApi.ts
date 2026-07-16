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
