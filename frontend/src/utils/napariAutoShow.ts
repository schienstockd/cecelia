// Decisions for the "restore the user's remembered overlays" step that runs every time an image
// (re)opens in napari — which label sets, branch (skeleton) sets, track ribbons and population point
// layers to ask for, and which segmentation's obs columns to read.
//
// WHY THIS IS NOT IN THE COMPONENT: the autoshow used to live entirely inside ViewerPanel.vue, which
// App.vue mounts behind `v-if="settings.viewerPanelOpen"` — the floating Viewer panel is OFF by
// default. So opening an image while that panel was closed restored nothing at all (no subscriber
// existed to hear `napari:opened`), and the toggles — read from localStorage, so they still showed ON
// — only took effect once the user flipped them off and on by hand. The orchestration now lives in
// composables/useNapariAutoShow.ts, mounted once at app level; the decisions live here, pure and
// unit-tested, so they can't drift from the panel's own per-toggle requests.

// The CELL-grained population types that render as centroid POINT overlays, in display order. Only
// these belong here: show-populations plots by cell label, whereas track/trackclust are track-grained
// (membership is track_ids) and render as ribbons via show-tracks. ONE list — the ViewerPanel toggle
// row and the autoshow both derive from it, so adding a pop type can't restore-but-not-show (or the
// reverse).
export const CELL_POP_TYPES = ['flow', 'clust', 'region'] as const


// ── Which value names can have TRACKS ──────────────────────────────────────────────────────────────
//
// `labels` ∪ `labelPropsNames`, and the union is the whole point. They are two independent ccid.json
// registries: `labels` is segmentations with mask PIXELS, `labelPropsNames` is anything with a
// measurement table. A track set imported directly — ImageJ, TrackMate — for an unsegmented image is
// only in the second, and tracks need nothing but a `track_id` column.
//
// ONE helper because getting it wrong is invisible: the viewer seeded its toggles from the union
// while `pushTracksNow` re-derived the same record from `labels` alone, so toggling an imported set
// stored `true`, `getTrackVisibility` dropped the key on the way back out (it returns only names in
// the list it is given), and napari was asked to show nothing. The toggle looked live and did
// nothing.
export function trackableValueNames(img: { labels?: Record<string, unknown>
                                           labelPropsNames?: string[] } | null | undefined): string[] {
  const masks = Object.keys(img?.labels ?? {})
  const seen = new Set(masks)
  return [...masks, ...(img?.labelPropsNames ?? []).filter(vn => !seen.has(vn))]
}
export type CellPopType = typeof CELL_POP_TYPES[number]

export interface AutoShowInput {
  labels: Record<string, string[]>            // registered cell-label sets: {valueName → files}
  branchLabels: Record<string, string[]>      // registered skeleton-label sets: {valueName → files}
  labelVisibility: Record<string, boolean>    // remembered per-segmentation label toggles
  branchVisibility: Record<string, boolean>   // remembered per-segmentation branch toggles
  trackVisibility: Record<string, boolean>    // remembered per-segmentation track-ribbon toggles
  popTypes: string[]                          // cell-grained pop types remembered as shown
  showGatedTracks: boolean
  showTrackclust: boolean
}

export interface AutoShowPlan {
  labels: Record<string, string[]>            // → one show-labels request (empty = skip)
  branchLabels: Record<string, string[]>      // → one show-labels request (empty = skip)
  trackValueNames: string[]                   // segmentations whose ribbons to show
  popTypes: string[]                          // pop types to push as centroid points
  showGatedTracks: boolean
  showTrackclust: boolean
  pushTracks: boolean                         // whether a show-tracks request is needed at all
}

// Keep only the entries the user has toggled on AND that actually have files registered. A toggle can
// outlive its data (label set deleted, project restored from an export), and asking for a set with no
// files is a request the bridge can only skip.
function _visibleWithFiles(
  registry: Record<string, string[]>, visibility: Record<string, boolean>,
): Record<string, string[]> {
  const out: Record<string, string[]> = {}
  for (const [vn, files] of Object.entries(registry ?? {})) {
    if (visibility?.[vn] && files?.length) out[vn] = files
  }
  return out
}

export function buildAutoShowPlan(input: AutoShowInput): AutoShowPlan {
  const labels       = _visibleWithFiles(input.labels, input.labelVisibility)
  const branchLabels = _visibleWithFiles(input.branchLabels, input.branchVisibility)
  // Track ribbons are keyed by segmentation like labels are, but there is no per-segmentation file
  // registry for them (the server resolves the _tracked h5ad), so the toggle alone decides.
  const trackValueNames = Object.keys(input.trackVisibility ?? {}).filter(vn => input.trackVisibility[vn])
  const popTypes = [...(input.popTypes ?? [])]
  return {
    labels, branchLabels, trackValueNames, popTypes,
    showGatedTracks: !!input.showGatedTracks,
    showTrackclust:  !!input.showTrackclust,
    // an empty valueNames list still has to be sent when a master toggle is on; with everything off
    // there is nothing to say (the bridge already cleared its layers on open).
    pushTracks: trackValueNames.length > 0 || !!input.showGatedTracks || !!input.showTrackclust,
  }
}

// ── Live previews (watch a task's output while it is still being written) ────────
// A running task publishes the stores it is streaming into (`live_outputs` on the Julia TaskRecord,
// surfaced by GET /api/tasks). That snapshot is the ONLY way the viewer can learn about a
// segmentation's label store before it finishes, because `ccid.json` — the source for every labels
// picker — is only written on success.

// One in-flight task's declaration, as GET /api/tasks returns it (snake_case, matching the rest
// of that route's payload — it predates this and the observer reads it too).
export interface TaskListEntry {
  id: string
  fun_name?: string
  image_uid?: string
  status?: string
  live_outputs?: { kind?: string; value_name?: string; files?: string[] }[]
}

// A label store being written right now, ready to hand to a show-labels request.
export interface LivePreview {
  taskId: string
  valueName: string
  files: string[]
}

// The cell-label stores currently being written for one image: `{valueName → files}` plus the task
// each belongs to. Only `kind === 'labels'` is included — `branchLabels` would need the other store
// family, and nothing declares one today (segment.branching writes its store once at the end).
//
// QUEUED tasks are excluded on purpose: a queued task has created nothing yet, so offering a preview
// would resolve to a store that isn't on disk (the bridge would skip it and the row would be a dead
// toggle). Only `running` has bytes to look at.
export function liveLabelPreviews(
  tasks: TaskListEntry[] | null | undefined, imageUid: string,
): LivePreview[] {
  if (!imageUid) return []
  const out: LivePreview[] = []
  const seen = new Set<string>()
  for (const t of tasks ?? []) {
    if (t?.image_uid !== imageUid || t?.status !== 'running') continue
    for (const o of t.live_outputs ?? []) {
      const valueName = o?.value_name ?? ''
      const files = o?.files ?? []
      // A value_name can only be written by one task at a time (they'd clobber each other), so the
      // first declaration wins rather than producing two rows for one store.
      if (o?.kind !== 'labels' || !valueName || !files.length || seen.has(valueName)) continue
      seen.add(valueName)
      out.push({ taskId: t.id, valueName, files })
    }
  }
  return out.sort((a, b) => a.valueName.localeCompare(b.valueName))
}

// Throttle for the refresh that follows progress ticks. Cellpose emits one per XY tile — many per
// second on a tiled frame — while a refresh re-reads label chunks from disk, so ticks are coalesced
// rather than followed one-for-one. Pure so the interval is testable without a clock.
export const PREVIEW_REFRESH_MIN_MS = 2000
export function shouldRefreshPreview(
  lastAtMs: number | undefined, nowMs: number, minIntervalMs: number = PREVIEW_REFRESH_MIN_MS,
): boolean {
  return lastAtMs === undefined || nowMs - lastAtMs >= minIntervalMs
}

// ── Autoshow claims ─────────────────────────────────────────────────────────────
// A caller that reopens an image to reproduce a DIFFERENT view than the remembered toggles (the
// analysis board's zoom-to-source replays a captured frame) claims that image's next open, and the
// autoshow then stands down for it exactly once.
//
// Keyed by image uid and held in a SET, not a single slot: two zoom-to-source clicks in quick
// succession both have opens in flight, and a single slot would let the first one's claim be
// overwritten — its image would then get the remembered overlays pushed over the captured frame.
export interface ClaimRegistry {
  claim(imageUid: string): void
  release(imageUid?: string): void   // no argument → drop every claim
  consume(imageUid: string): boolean // true if it WAS claimed (and clears it)
  size(): number
}
export function createClaimRegistry(): ClaimRegistry {
  const claims = new Set<string>()
  return {
    claim: uid => { if (uid) claims.add(uid) },
    release: uid => { uid === undefined ? claims.clear() : claims.delete(uid) },
    consume: uid => claims.delete(uid),
    size: () => claims.size,
  }
}

// The segmentation/version whose obs columns back the colour-by dropdown and the colour-labels push.
// Mirrors what the server opens when no valueName is passed: the registered `_active` version, else
// the last non-default, else "default", else nothing. Shared by ViewerPanel's dropdown and the
// app-level autoshow so the two can't disagree about which segmentation is being shown.
export function activeValueName(
  img: { filepaths?: Record<string, unknown>; activeValueName?: string } | null | undefined,
): string {
  const names = Object.keys(img?.filepaths ?? {})
  const nonDefault = names.filter(n => n !== 'default')
  if (img?.activeValueName && names.includes(img.activeValueName)) return img.activeValueName
  if (nonDefault.length > 0) return nonDefault[nonDefault.length - 1]
  if (names.includes('default')) return 'default'
  return names[0] ?? ''
}
