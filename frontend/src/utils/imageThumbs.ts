/**
 * The image table's per-row thumbnails: which rows show one, and the fetch.
 *
 * A thumbnail is the image at timepoint 0, z max-projected, coloured the way the viewer shows it —
 * the table's counterpart of the viewer's mini map (`GET /api/images/thumbnail`, rendered server-side
 * by `api/src/thumbnails_api.jl`). Which rows show one is per project and per person: a set of image
 * uids in `profileStorage` under `thumbsStorageKey(projectUid)`.
 */

export const THUMB_PX = 160

export function thumbsStorageKey(projectUid: string): string {
  return `cc.imageThumbs:${projectUid}`
}

/** The stored uid list → a set. Anything unreadable is "no thumbnails shown", never a throw. */
export function parseShown(raw: string | null): Set<string> {
  if (!raw) return new Set()
  try {
    const v = JSON.parse(raw)
    return new Set(Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string') : [])
  } catch {
    return new Set()
  }
}

export function serializeShown(shown: ReadonlySet<string>): string {
  return JSON.stringify([...shown].sort())
}

/** Flip one row. Returns a new set so a `ref` holding it re-renders. */
export function toggleShown(shown: ReadonlySet<string>, uid: string): Set<string> {
  const next = new Set(shown)
  if (next.has(uid)) next.delete(uid)
  else next.add(uid)
  return next
}

/** The header button: all of `uids` shown → hide them all; otherwise show them all. Rows outside
 *  `uids` (another set's images) keep their state. */
export function toggleAllShown(shown: ReadonlySet<string>, uids: readonly string[]): Set<string> {
  const next = new Set(shown)
  const allOn = uids.length > 0 && uids.every(u => next.has(u))
  for (const u of uids) {
    if (allOn) next.delete(u)
    else next.add(u)
  }
  return next
}

export function allShown(shown: ReadonlySet<string>, uids: readonly string[]): boolean {
  return uids.length > 0 && uids.every(u => shown.has(u))
}

/** Cache key: a new active version, or a new `rev`, is a new thumbnail. `rev` is the image's
 *  `dataVersion` (project store), bumped when a task finishes on it — a re-run that rewrites a store
 *  under the same name changes neither the image record nor its version, only that. */
export function thumbCacheKey(projectUid: string, imageUid: string, version: string | undefined, rev = 0): string {
  return `${projectUid}|${imageUid}|${version ?? ''}|${rev}`
}

const _cache = new Map<string, Promise<string | null>>()

/** The thumbnail as a data URL, or null when the image has none to show (not imported, unreadable).
 *  One request per key per session; a failure is not cached, so the next toggle retries. A new key
 *  drops the image's older ones, so the cache holds one thumbnail per image. */
export function loadThumb(projectUid: string, imageUid: string, version: string | undefined,
                          rev = 0): Promise<string | null> {
  const key = thumbCacheKey(projectUid, imageUid, version, rev)
  const hit = _cache.get(key)
  if (hit) return hit
  dropOlderThumbs(projectUid, imageUid, key)
  const q = new URLSearchParams({ projectUid, imageUid, size: String(THUMB_PX) })
  const p = fetch(`/api/images/thumbnail?${q}`)
    .then(async r => (r.ok ? ((await r.json()).png as string) ?? null : null))
    .catch(() => null)
    .then(url => {
      if (url === null) _cache.delete(key)
      return url
    })
  _cache.set(key, p)
  return p
}

function dropOlderThumbs(projectUid: string, imageUid: string, keep: string) {
  const prefix = `${projectUid}|${imageUid}|`
  for (const k of [..._cache.keys()]) if (k.startsWith(prefix) && k !== keep) _cache.delete(k)
}

/** How many thumbnails the session holds — for tests. */
export const thumbCacheSize = () => _cache.size
