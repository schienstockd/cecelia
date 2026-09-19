// Pure helpers for the Kiwi cockpit's Recent-captures row. Split from the SFC so its formatting is
// unit-tested (frontend rule: pure logic in utils/*.ts). Fetcher wraps GET /api/viewer/captures.
//
// The address envelope shape is documented in docs/todo/BIDIR_CONTEXT_PLAN.md → *Captured payload
// shape*. This module ONLY names the fields Kiwi's one-line row renders; a future v2 detail view
// would use more.

/** Capture surface tags — matches api/src/captures_api.jl `envelope.surface`. */
export type CaptureSurface = 'ui' | 'plot' | 'viewer_frame' | 'viewer_slab'

/** One-line address the row shows below the timestamp. */
export interface CaptureAddress {
  projectUid?: string
  imageUid?: string
  valueName?: string
  t?: number | [number, number]
  z?: number
  domAnchor?: string
  plotSpec?: { specId?: string }
}

/** One row in the list. */
export interface CaptureRow {
  captureId: string
  createdAt: string        // ISO-ish; the id encodes the same time — see _new_capture_id in Julia
  surface: CaptureSurface
  address: CaptureAddress | null
  // Kiwi PR B: server sets this when a capture was created by re-annotating an earlier one. Kiwi
  // shows a small "↳" glyph on such rows so a user reading the list can tell a refinement from a
  // fresh share. Never used for ordering — the id-encoded timestamp is still the sort key.
  previousCaptureId?: string
}

/** Response envelope from GET /api/viewer/captures. */
interface CapturesResponse { items?: unknown[] }

/** Fetch the newest captures for this project. Any network / shape failure ⇒ `[]`, not a throw —
 *  a glance row goes empty instead of red. */
export async function fetchRecentCaptures(
  projectUid: string, limit = 10, apiBase = ''
): Promise<CaptureRow[]> {
  if (!projectUid) return []
  try {
    const url = `${apiBase}/api/viewer/captures?projectUid=${encodeURIComponent(projectUid)}&limit=${limit}`
    const res = await fetch(url)
    if (!res.ok) return []
    const json = await res.json() as CapturesResponse
    return (json.items ?? []).map(parseCapture).filter((c): c is CaptureRow => c !== null)
  } catch {
    return []
  }
}

function parseCapture(raw: unknown): CaptureRow | null {
  if (!raw || typeof raw !== 'object') return null
  const r = raw as Record<string, unknown>
  const captureId = typeof r.captureId === 'string' ? r.captureId : ''
  if (!captureId) return null
  const surface = (r.surface as CaptureSurface) ?? 'viewer_frame'
  const createdAt = typeof r.createdAt === 'string' ? r.createdAt : ''
  const address = r.address && typeof r.address === 'object'
    ? (r.address as CaptureAddress) : null
  const out: CaptureRow = { captureId, createdAt, surface, address }
  if (typeof r.previousCaptureId === 'string') out.previousCaptureId = r.previousCaptureId
  return out
}

/** Compact address label — `image 1SqevM · t=3, z=7` / `plot dotplot` / `ui viewer.share`.
 *  Uses `·` as the visual separator so it renders as ONE line at the row's width. */
export function formatAddress(row: CaptureRow): string {
  const a = row.address
  if (row.surface === 'ui') {
    return a?.domAnchor ? `ui · ${a.domAnchor}` : 'ui'
  }
  if (row.surface === 'plot') {
    return a?.plotSpec?.specId ? `plot · ${a.plotSpec.specId}` : 'plot'
  }
  // viewer_frame / viewer_slab
  const bits: string[] = []
  if (a?.imageUid) bits.push(`image ${a.imageUid}`)
  const t = a?.t
  if (Array.isArray(t)) bits.push(`t=${t[0]}–${t[1]}`)      // en-dash for the range
  else if (typeof t === 'number') bits.push(`t=${t}`)
  if (typeof a?.z === 'number') bits.push(`z=${a.z}`)
  const head = row.surface === 'viewer_slab' ? 'slab' : 'viewer'
  return bits.length > 0 ? `${head} · ${bits.join(', ')}` : head
}

/** Delete a single capture (Kiwi post-PR #3 follow-up). Idempotent on the server;
 *  network failure ⇒ `false` so the caller can leave the row visible without throwing. */
export async function deleteCapture(
  projectUid: string, captureId: string, apiBase = ''
): Promise<boolean> {
  if (!projectUid || !captureId) return false
  try {
    const res = await fetch(`${apiBase}/api/viewer/capture/delete`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, captureId }),
    })
    return res.ok
  } catch {
    return false
  }
}

/** Bulk-clear every capture for a project. Returns the count the server actually removed,
 *  or `null` on a network failure. */
export async function clearAllCaptures(
  projectUid: string, apiBase = ''
): Promise<number | null> {
  if (!projectUid) return null
  try {
    const res = await fetch(`${apiBase}/api/viewer/captures/clear`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid }),
    })
    if (!res.ok) return null
    const raw = await res.json() as Record<string, unknown>
    return typeof raw.cleared === 'number' ? raw.cleared : 0
  } catch {
    return null
  }
}

/** Fetch a capture's frame as a data URL. Used by Kiwi row thumbnails (PR B) — the endpoint
 *  returns the FULL PNG (not a downscaled thumb), which is fine at the row cap of 10; a browser
 *  will decode + downscale for the 48-px slot. Any failure ⇒ empty string so the row still
 *  renders without a broken-image icon. */
export async function fetchCaptureFrame(
  projectUid: string, captureId: string, apiBase = ''
): Promise<string> {
  if (!projectUid || !captureId) return ''
  try {
    const url = `${apiBase}/api/viewer/capture?projectUid=${encodeURIComponent(projectUid)}`
              + `&captureId=${encodeURIComponent(captureId)}`
    const res = await fetch(url)
    if (!res.ok) return ''
    const json = await res.json() as { frame?: string }
    return typeof json.frame === 'string' ? json.frame : ''
  } catch {
    return ''
  }
}

/** Relative "how long ago" — `just now` / `5m` / `2h` / `3d`. Not localized; a glance row is
 *  meant to be short. `createdAt` is ISO-ish; anything unparseable ⇒ empty (the row still shows,
 *  just without a timestamp — better than hiding the capture). */
export function formatWhen(createdAt: string, now: Date = new Date()): string {
  if (!createdAt) return ''
  const then = new Date(createdAt)
  const ms = now.getTime() - then.getTime()
  if (!Number.isFinite(ms) || ms < 0) return ''
  if (ms < 60_000)         return 'just now'
  if (ms < 60 * 60_000)    return `${Math.floor(ms / 60_000)}m`
  if (ms < 24 * 3_600_000) return `${Math.floor(ms / 3_600_000)}h`
  return `${Math.floor(ms / 86_400_000)}d`
}
