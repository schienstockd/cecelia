// System env probe — `GET /api/system/envs` cached at module scope.
//
// One consumer today: the cellpose-model param advisory (`paramAdvisors.ts`) needs to know whether
// the opt-in `cellpose-v3` env is installed on this Mac. The state changes rarely — only when the
// user clicks Install and the job finishes — so a full round-trip on every re-render is waste; a
// module-level cache is enough, with `invalidateSystemEnvs()` called by `stores/ws.ts` on the
// `system:env-install-complete` frame.
//
// Shape mirrors `api_system_envs` in `api/src/system_api.jl`.

export interface SystemEnvInfo {
  installed: boolean
  supported: boolean
  approxSizeMb?: number
  description?: string
}

export interface SystemEnvsPayload {
  /** pixi platform label: 'osx-arm64' | 'linux-64' | 'win-64' | 'osx-64' | 'unknown' */
  platform: string
  envs: Record<string, SystemEnvInfo>
}

let _cache: Promise<SystemEnvsPayload | null> | null = null

export function getSystemEnvs(): Promise<SystemEnvsPayload | null> {
  if (_cache) return _cache
  _cache = (async () => {
    try {
      const res = await fetch('/api/system/envs')
      if (!res.ok) return null
      return await res.json() as SystemEnvsPayload
    } catch {
      return null
    }
  })()
  return _cache
}

/** Drop the cached probe. Call from the ws handler when an install job finishes so the next
 *  `getSystemEnvs()` sees the freshly installed env. */
export function invalidateSystemEnvs(): void {
  _cache = null
}

/** Request the install for the named env. The result is a job id; the WS frame
 *  `system:env-install-complete` marks the end. */
export async function requestEnvInstall(env: string): Promise<{ started: boolean; jobId?: string; error?: string }> {
  try {
    const res = await fetch('/api/system/envs/install', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ env }),
    })
    const d = await res.json()
    if (!res.ok) return { started: false, error: d?.error ?? `HTTP ${res.status}` }
    return { started: true, jobId: d.jobId }
  } catch (e) {
    return { started: false, error: String(e) }
  }
}
