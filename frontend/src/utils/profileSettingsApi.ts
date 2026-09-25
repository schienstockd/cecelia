// Per-profile settings API client — USER_PROFILE_PLAN Phase 4.
//
// Two endpoints:
//   GET  /api/profile/settings          → { profile, settings }
//   POST /api/profile/settings/patch    → { profile, settings } (body: partial dict, null deletes)
//
// Both are thin wrappers — no error-throwing on network failure so a launch never gets stuck on
// a boot fetch. The settings store falls back to localStorage defaults on failure and re-tries
// later; that's what the empty-bag return conveys.

export type ProfileSettingsValue = string | number | boolean | null

export interface ProfileSettingsResult {
  profile: string
  settings: Record<string, ProfileSettingsValue>
}

async function _json(res: Response): Promise<unknown> {
  try { return await res.json() } catch { return null }
}

/**
 * Fetch the active profile's settings. Empty bag on failure OR timeout — the caller uses its
 * own defaults. A 5s timeout is critical: if the backend hasn't been restarted to pick up the
 * new /api/profile/settings routes (api/src/ isn't Revise-tracked), the proxy may hang the
 * request forever, and any UI keyed off the store's `profileHydrated` flag would spin
 * indefinitely. AbortSignal.timeout is standard on every browser that supports WebGPU (Chrome
 * 103+, Firefox 100+).
 */
export async function fetchProfileSettings(apiBase = ''): Promise<ProfileSettingsResult> {
  try {
    const res = await fetch(`${apiBase}/api/profile/settings`, {
      cache: 'no-store',
      signal: AbortSignal.timeout(5000),
    })
    if (!res.ok) return { profile: 'default', settings: {} }
    const body = await _json(res) as Partial<ProfileSettingsResult> | null
    return {
      profile: body?.profile ?? 'default',
      settings: body?.settings ?? {},
    }
  } catch { return { profile: 'default', settings: {} } }
}

/**
 * PATCH the active profile's settings — shallow merge on the server. A `null` value in `patch`
 * DELETES the key from the stored bag (so the frontend can reset a key to its default). Silent
 * failure returns the passed patch as the merged settings so caller ergonomics stay simple; a
 * real failure surfaces via the empty `profile` field.
 */
export async function patchProfileSettings(patch: Record<string, ProfileSettingsValue>,
                                           apiBase = ''): Promise<ProfileSettingsResult> {
  try {
    const res = await fetch(`${apiBase}/api/profile/settings/patch`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(patch),
      // `keepalive` lets the flush from a `beforeunload` handler actually land on the server;
      // a normal fetch is cancelled when the page unloads. Cheap for the small payloads this
      // endpoint gets (browsers cap at ~64 KiB per keepalive request).
      keepalive: true,
    })
    if (!res.ok) return { profile: '', settings: patch }
    const body = await _json(res) as { profile?: string; settings?: Record<string, ProfileSettingsValue> } | null
    return {
      profile: body?.profile ?? '',
      settings: body?.settings ?? patch,
    }
  } catch { return { profile: '', settings: patch } }
}
