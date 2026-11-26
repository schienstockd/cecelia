// Per-profile settings API client — USER_PROFILE_PLAN Phase 4.
//
// Two endpoints:
//   GET  /api/profile/settings          → { profile, settings }
//   POST /api/profile/settings/patch    → { profile, settings } (body: partial dict, null deletes)
//
// Both are thin wrappers — no error-throwing on network failure so a launch never gets stuck on
// a boot fetch. The settings store falls back to localStorage defaults on failure and re-tries
// later; that's what the empty-bag return conveys.

import { getJson, postJson, tryApi } from './api'

export type ProfileSettingsValue = string | number | boolean | null

export interface ProfileSettingsResult {
  profile: string
  settings: Record<string, ProfileSettingsValue>
  /** False when the fetch failed and `profile`/`settings` are the empty fallback — a caller that
   *  compares `profile` against something must not act on the fallback's `'default'`. */
  ok?: boolean
}

/**
 * Fetch the active profile's settings. Empty bag on failure OR timeout — the caller uses its
 * own defaults. A 5s timeout is critical: if the backend hasn't been restarted to pick up the
 * new /api/profile/settings routes (api/src/ isn't Revise-tracked), the proxy may hang the
 * request forever, and any UI keyed off the store's `profileHydrated` flag would spin
 * indefinitely.
 */
export async function fetchProfileSettings(apiBase = ''): Promise<ProfileSettingsResult> {
  const r = await tryApi(getJson<Partial<ProfileSettingsResult> | null>(
    `${apiBase}/api/profile/settings`, { timeoutMs: 5000 }))
  const body = r.ok ? r.data : null
  if (!body?.profile) return { profile: 'default', settings: {}, ok: false }
  return { profile: body.profile, settings: body.settings ?? {}, ok: true }
}

/**
 * PATCH the active profile's settings — shallow merge on the server. A `null` value in `patch`
 * DELETES the key from the stored bag (so the frontend can reset a key to its default). Silent
 * failure returns the passed patch as the merged settings so caller ergonomics stay simple; a
 * real failure surfaces via the empty `profile` field.
 */
export async function patchProfileSettings(patch: Record<string, ProfileSettingsValue>,
                                           apiBase = ''): Promise<ProfileSettingsResult> {
  // `keepalive` lets the flush from a `beforeunload` handler actually land on the server;
  // a normal fetch is cancelled when the page unloads. Cheap for the small payloads this
  // endpoint gets (browsers cap at ~64 KiB per keepalive request).
  const r = await tryApi(postJson<{ profile?: string; settings?: Record<string, ProfileSettingsValue> } | null>(
    `${apiBase}/api/profile/settings/patch`, patch, { keepalive: true }))
  if (!r.ok) return { profile: '', settings: patch }
  return {
    profile: r.data?.profile ?? '',
    settings: r.data?.settings ?? patch,
  }
}
