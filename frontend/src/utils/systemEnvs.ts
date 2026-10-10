// System env probe — `GET /api/system/envs` cached at module scope.
//
// One consumer today: the cellpose-model param advisory (`paramAdvisors.ts`) needs to know whether
// the opt-in `cellpose-v3` env is installed on this Mac. The state changes rarely — only when the
// user clicks Install and the job finishes — so a full round-trip on every re-render is waste; a
// module-level cache is enough, with `invalidateSystemEnvs()` called by `stores/ws.ts` when a
// `system-env-install:<env>` job ends.
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

const _changed = new Set<() => void>()

/** Run `cb` whenever the env state may have changed (an install finished, failed, or was already
 *  there). The install button and the task-definition lists listen: the model dropdown hides the v3
 *  models until the env exists, so they must refetch in place. Returns the unsubscribe. */
export function onSystemEnvsChanged(cb: () => void): () => void {
  _changed.add(cb)
  return () => { _changed.delete(cb) }
}

/** Drop the cached probe and tell the listeners. Called by the ws handler when an install job
 *  finishes, and by `requestEnvInstall` when the env turns out to be there already (no job, so no
 *  ws frame). */
export function invalidateSystemEnvs(): void {
  _cache = null
  for (const cb of [..._changed]) cb()
}

export type EnvInstallResult =
  | { status: 'started'; jobId: string }   // the ws `system-env-install:<env>` job takes it from here
  | { status: 'present' }                  // installed already — nothing to wait for
  | { status: 'error'; error: string }

/** Request the install for the named env. Mirrors `api_system_envs_install` in
 *  `api/src/system_api.jl`: 202 + jobId, 200 + `alreadyPresent`, or an error body. */
export async function requestEnvInstall(env: string): Promise<EnvInstallResult> {
  try {
    const res = await fetch('/api/system/envs/install', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ env }),
    })
    const d = await res.json().catch(() => null)
    if (!res.ok) return { status: 'error', error: d?.error ?? `HTTP ${res.status}` }
    if (d?.alreadyPresent) {
      invalidateSystemEnvs()
      return { status: 'present' }
    }
    return { status: 'started', jobId: String(d?.jobId ?? '') }
  } catch (e) {
    return { status: 'error', error: String(e) }
  }
}
