// A "the pixels behind this vn just changed" signal — the guard #779 cannot detect on its own.
//
// #779 invalidates the tile atlas when the STORE identity changes (e.g. `default` → `smoothed`,
// same imageUid, different valueName). What it can't detect is a re-run of a task that overwrites
// the SAME `ccidSmoothed.ome.zarr` (or a label store) IN PLACE: identity string unchanged, bytes
// changed, cache HIT still returns stale pixels. `ViewerPanel` publishes on task completion; every
// viewer (in-panel and pop-out) threads the rev into its `sourceId`/`BrickSource.rev`, and the
// invalidation logic #779 wired for a version swap fires the same way for a same-store rewrite.
//
// **`localStorage` + `storage` AND a same-window CustomEvent**, mirroring `openProjectChannel.ts`
// but adding the same-window path. The `storage` event fires only in the OTHER windows of the
// origin, so a pop-out viewer hears it, but the in-panel viewer living in the SAME window as
// `ViewerPanel` does not — publishing on task-done and having nothing happen in-panel is the exact
// bug this channel exists to close. The CustomEvent covers the same-window case; pop-outs get the
// storage event as normal.
//
// **Scoped.** The rev alone is a broadcast — every viewer in the origin reallocates on every task
// done, including popouts showing a different image. `scope` narrows it: an `imageUid` limits to
// viewers on that image; `valueName`/`labelValueName` further narrow to the intensity or label vn
// that changed. Legacy `cc.viewerSlabsTick` retired — its labels-only cross-window path is a
// subset of this one now, and it never covered same-window (in-panel) at all.

const KEY = 'cc.viewerCacheClearRev'
const SAME_WINDOW_EVENT = 'cc:viewer-cache-clear'

/** What the publisher names as changed. Empty = broadcast (unknown, invalidate everyone). */
export interface ViewerCacheClearScope {
  /** The image whose bytes changed. Absent = every image on this origin. */
  imageUid?: string
  /** The intensity vn whose store was rewritten. Absent = any intensity vn on `imageUid`. */
  valueName?: string
  /** The label vn whose store was rewritten. Absent = any label vn on `imageUid`. */
  labelValueName?: string
}

/** The full payload a subscriber receives — the rev to thread into `sourceId` + the scope. */
export interface ViewerCacheClearEvent extends ViewerCacheClearScope {
  rev: string
}

/**
 * Publish a fresh revision. Fires in this window AND every other window on this origin.
 *
 * Pass a scope naming what changed — `{imageUid}` on any task done, plus `valueName` or
 * `labelValueName` when the task result names the specific store. Scope-less publish is a broadcast
 * and forces every viewer to reallocate; only use it when the caller genuinely doesn't know.
 */
export function publishViewerCacheClear(scope: ViewerCacheClearScope = {}): void {
  const ev: ViewerCacheClearEvent = { rev: String(Date.now()), ...scope }
  const payload = JSON.stringify(ev)
  try { localStorage.setItem(KEY, payload) } catch { /* storage disabled — pop-outs won't hear it */ }
  // storage events don't fire in the publishing window, so raise a same-window CustomEvent too.
  try { window.dispatchEvent(new CustomEvent(SAME_WINDOW_EVENT, { detail: ev })) } catch {}
}

/** The last-published rev, or `''` when nothing was ever published. Used at mount to seed `sourceId`. */
export function readViewerCacheClearRev(): string {
  try {
    const raw = localStorage.getItem(KEY)
    if (raw === null) return ''
    return parseEventPayload(raw)?.rev ?? ''
  } catch { return '' }
}

/**
 * Parse the storage-string payload back into an event. Tolerates the pre-scope format (a bare rev
 * string) so a rev written by an older tab doesn't wedge a newer subscriber. Returns null on any
 * parse failure — an unreadable payload is treated as "nothing to react to".
 */
function parseEventPayload(raw: string): ViewerCacheClearEvent | null {
  if (raw === '') return null
  // Legacy format: bare rev string, no braces. Kept parsing so a mid-session upgrade doesn't drop
  // the last pre-upgrade rev on the floor. Broadcast (no scope), which is what the pre-scope
  // channel effectively was.
  if (!raw.startsWith('{')) return { rev: raw }
  try {
    const p = JSON.parse(raw)
    if (typeof p !== 'object' || p === null || typeof p.rev !== 'string') return null
    const ev: ViewerCacheClearEvent = { rev: p.rev }
    if (typeof p.imageUid === 'string') ev.imageUid = p.imageUid
    if (typeof p.valueName === 'string') ev.valueName = p.valueName
    if (typeof p.labelValueName === 'string') ev.labelValueName = p.labelValueName
    return ev
  } catch { return null }
}

/**
 * What a `storage` event means for a viewer holding rev `currentRev`, as a pure decision:
 * `null` = nothing to do; otherwise the parsed event to react to. Separated from the listener
 * because the listener is the untestable half. Three ignores are real: an event about any other
 * key (the app writes several to localStorage), a payload we can't parse, and one whose rev equals
 * what we already hold (the publisher fires a storage set AND a CustomEvent in the same tick to
 * cover the same-window case, and a viewer sitting in another window will see the storage event
 * first and then re-trigger nothing on the follow-up).
 */
export function viewerCacheClearFromStorageEvent(
  e: Pick<StorageEvent, 'key' | 'newValue'>,
  currentRev: string,
): ViewerCacheClearEvent | null {
  if (e.key !== KEY) return null
  const raw = e.newValue ?? ''
  if (raw === '') {
    // localStorage.removeItem → the "no rev" state. Preserve the previous behaviour: treat as an
    // empty rev, so a subscriber holding `''` at mount does not reallocate on a spurious clear.
    return currentRev === '' ? null : { rev: '' }
  }
  const ev = parseEventPayload(raw)
  if (ev === null) return null
  return ev.rev === currentRev ? null : ev
}

/**
 * Does an event's scope name us? Broadcast (no scope) always matches. Otherwise the imageUid must
 * match (if named), then each vn field must EITHER be unspecified in the event (any vn on this
 * image) OR equal the corresponding vn we currently render.
 *
 * Kept pure so the same match rule can be asserted in tests — the subscribe callback is untestable
 * and shouldn't own the decision.
 */
export function viewerCacheClearMatches(
  ev: ViewerCacheClearScope,
  current: { imageUid: string; valueName?: string; labelValueName?: string },
): boolean {
  // Broadcast (no field named) is a "we don't know what changed" — matches every viewer.
  if (ev.imageUid === undefined && ev.valueName === undefined && ev.labelValueName === undefined) {
    return true
  }
  if (ev.imageUid !== undefined && ev.imageUid !== current.imageUid) return false
  // A vn named in the event that we don't render is not for us. A vn unnamed (undefined) in the
  // event means "any vn on this image" — matches. A vn named that we render at, matches.
  if (ev.valueName !== undefined && ev.valueName !== (current.valueName ?? '')) return false
  if (ev.labelValueName !== undefined && ev.labelValueName !== (current.labelValueName ?? '')) return false
  return true
}

/**
 * Subscribe to clear signals from THIS window (task done, button click) or any pop-out. Returns
 * unsubscribe. The scope filter lives at the CALL SITE — this returns every event; ViewerWindow
 * consults `viewerCacheClearMatches` against its current image/vn/labelName so the filter uses the
 * live values at reallocate time, not whatever was current at subscribe time.
 */
export function onViewerCacheClear(cb: (ev: ViewerCacheClearEvent) => void): () => void {
  const onStorage = (e: StorageEvent) => {
    // Reads localStorage inside the callback so we always see the latest, no need to thread state.
    const decision = viewerCacheClearFromStorageEvent(e, readViewerCacheClearRev())
    if (decision !== null) cb(decision)
  }
  const onCustom = (e: Event) => {
    const detail = (e as CustomEvent).detail
    // Legacy same-window fire path was a bare rev string; treat that as a broadcast for continuity.
    if (typeof detail === 'string') { cb({ rev: detail }); return }
    if (detail && typeof detail === 'object' && typeof (detail as { rev?: unknown }).rev === 'string') {
      cb(detail as ViewerCacheClearEvent)
    }
  }
  window.addEventListener('storage', onStorage)
  window.addEventListener(SAME_WINDOW_EVENT, onCustom)
  return () => {
    window.removeEventListener('storage', onStorage)
    window.removeEventListener(SAME_WINDOW_EVENT, onCustom)
  }
}
