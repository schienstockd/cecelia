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
