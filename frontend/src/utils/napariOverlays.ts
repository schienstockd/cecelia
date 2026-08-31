// The ONE place that builds the napari overlay requests (show-tracks / show-populations /
// colour-labels). Both the interactive ViewerPanel and the non-interactive callers (zoom-to-source,
// the strip) go through these builders, so there's a single request shape per endpoint instead of two
// divergent inline copies. Each builder returns the raw Response (or undefined on a network error) so
// callers can still harvest the legend from the reply; it does not read/parse the body itself.
import { parseOverlays, type OverlayPushConfig } from './overlayLayers'
import { channelLegend, type LegendItem } from './viewLegend'

const _post = (path: string, body: unknown): Promise<Response | undefined> =>
  fetch(path, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })
    .catch(() => undefined)

/** Apply a WHOLE captured view snapshot (keyframe select, zoom-to-source). One-shot and awaited — it
 *  answers a click, not a drag, so it is deliberately NOT coalesced: the caller wants this exact
 *  snapshot applied, and a later one must not silently replace it. The bridge skips absent layers. */
export function applyViewState(snapshot: unknown): Promise<Response | undefined> {
  return _post('/api/napari/apply-view-state', { viewState: snapshot })
}

// ── Capture-view legend (shared: analysis-board strip + movie title card) ───────
// The ONE path that turns a captured napari view snapshot into legend pieces: channels from the
// snapshot's layer colormaps (channelLegend), populations + colour-by from the canonical
// /api/napari/overlay-legend (overlay pops parsed from the snapshot's layer names). Both the board
// strip (ImageStripView) and the single-record movie card go through this, so their legends match.
export interface CapturedViewLegend {
  channels: LegendItem[]
  populations: { name: string; colour: string }[]
  colourBy?: { column: string; items: { value: string; colour: string; label: string }[] }
}
export async function captureViewLegend(
  projectUid: string, imageUid: string,
  snapshot: { layers?: Record<string, unknown> } | null | undefined,
  colourBy: string, colourOverrides: Record<string, string> = {},
): Promise<CapturedViewLegend> {
  const layers = (snapshot?.layers ?? {}) as Record<string, { colormap?: string; visible?: boolean }>
  const channels = channelLegend(layers)
  const overlayPops = parseOverlays(snapshot?.layers as Record<string, unknown>)
    .map(o => ({ valueName: o.valueName, popType: o.popType, path: o.path }))
  let populations: { name: string; colour: string }[] = []
  let cby: CapturedViewLegend['colourBy'] | undefined
  const res = await _post('/api/napari/overlay-legend', { projectUid, imageUid, colourBy, overlayPops, colourOverrides })
  if (res?.ok) {
    const j = await res.json().catch(() => ({})) as CapturedViewLegend & { ok?: boolean }
    populations = j.populations ?? []
    cby = j.colourBy
  }
  return { channels, populations, colourBy: cby }
}

// The movie title-card helpers moved to `./titleCard.ts` (a card is a MOVIE-side artefact and lives
// on the recorded frames, not on the interactive canvas). Re-exported here so any caller that hasn't
// migrated its import path yet keeps working during the napari-retirement cross-cut.
export { buildTitleCard, unionViewSnapshot, type TitleCardPayload } from './titleCard'

// One show-labels request. Cell labels (`labels`) and branch/skeleton labels (`branchLabels`) share
// the endpoint and its single `show` flag; either payload may be empty. Sending both in ONE request
// (rather than two) keeps them atomic against the bridge's layer reconciliation.
export interface PushLabelsOpts {
  labels?: Record<string, string[]>         // {valueName → label files} → labels/ store
  branchLabels?: Record<string, string[]>   // {valueName → label files} → branchLabels/ store
  show: boolean
  // `labels` names stores a task is still WRITING → show them in their own `(vn) Labels (live)` layer
  // (level 0 only, caching forced off bridge-side). Never applies to branchLabels: those are written
  // once, at the end of segment.branching, so there is no partial store to watch.
  preview?: boolean
  // Mask outline width in px, 0 = filled — the per-set `labelContour`. It MUST ride every show-labels
  // push: this endpoint REBUILDS the Labels layer, and the backend defaults the value to 0, so a push
  // that omits it silently refills a mask the user had outlined. That is how a set outline was lost on
  // every mask toggle and on the post-open overlay restore, which is why movies recorded filled — the
  // outline was already gone before the recorder ran. Cell labels only (the backend never applies it to
  // branch/skeleton layers). Omit ONLY where there is no set to read it from.
  labelContour?: number
}
// The request body, split out from the fetch so the wire shape is unit-testable — `labelContour`
// going missing is invisible until you watch a movie come out filled.
export function labelsRequestBody(o: PushLabelsOpts): Record<string, unknown> {
  return {
    ...(o.labels       && Object.keys(o.labels).length       ? { allLabels: o.labels }             : {}),
    ...(o.branchLabels && Object.keys(o.branchLabels).length ? { allBranchLabels: o.branchLabels } : {}),
    // labelsCache hardcoded true: matches the pre-P6 default in `settings.napariLabelsCache`. That
    // toggle went away in P6 (napari-specific concept); the bridge itself is deleted in P9.
    showLabels: o.show, labelsCache: true,
    ...(o.preview ? { preview: true } : {}),
    ...(o.labelContour === undefined ? {} : { labelContour: o.labelContour }),
  }
}
export function pushLabels(o: PushLabelsOpts): Promise<Response | undefined> {
  return _post('/api/napari/show-labels', labelsRequestBody(o))
}

// Re-read live-preview layers in place, without rebuilding them — the progress-tick counterpart to
// pushLabels({preview: true}). A value_name with no preview layer is a no-op bridge-side, so this is
// safe to fire whether or not the user still has the preview on.
export function refreshLabels(labels: Record<string, string[]>): Promise<Response | undefined> {
  return _post('/api/napari/refresh-labels', { allLabels: labels })
}

export interface PushTracksOpts {
  valueNames: string[]            // segmentations whose whole-segmentation (_tracked) ribbons to show
  showGatedTracks: boolean
  showTrackclust: boolean
  colorBy?: string                // obs column to colour vertices by ('' → each pop's own colour)
  colourOverrides?: Record<string, string>
  tailWidth?: number
}
export function pushTracks(projectUid: string, imageUid: string, o: PushTracksOpts): Promise<Response | undefined> {
  return _post('/api/napari/show-tracks', {
    projectUid, imageUid, valueNames: o.valueNames,
    showGatedTracks: o.showGatedTracks, showTrackclust: o.showTrackclust,
    colorBy: o.colorBy ?? '', colourOverrides: o.colourOverrides ?? {},
    ...(o.tailWidth != null ? { tailWidth: o.tailWidth } : {}),
  })
}

export interface PushPopulationsOpts { popType: string; show: boolean; valueName?: string; pointsSize?: number }
export function pushPopulations(projectUid: string, imageUid: string, o: PushPopulationsOpts): Promise<Response | undefined> {
  return _post('/api/napari/show-populations', {
    projectUid, imageUid, valueName: o.valueName || undefined,
    popType: o.popType, show: o.show, pointsSize: o.pointsSize ?? 6,
  })
}

export interface PushColourLabelsOpts { column: string; valueName?: string; colourOverrides?: Record<string, string> }
export function pushColourLabels(projectUid: string, imageUid: string, o: PushColourLabelsOpts): Promise<Response | undefined> {
  return _post('/api/napari/colour-labels', {
    projectUid, imageUid, valueName: o.valueName || undefined,
    column: o.column, colourOverrides: o.colourOverrides ?? {},
  })
}

// ── zoom-to-source restore ─────────────────────────────────────────────────────
// Re-request the tracks/pops a captured frame had, via the shared builders above (so it can't drift
// from the ViewerPanel's requests). SEQUENTIAL — the bridge processes one command at a time; parallel
// pushes let a later push's layer reconciliation race an earlier one (tracks would stick, points not).
export interface RestoreOverlaysOpts extends OverlayPushConfig {
  colourBy?: string
  pointsSize?: number
}
export async function restoreOverlays(projectUid: string, imageUid: string, cfg: RestoreOverlaysOpts): Promise<void> {
  if (cfg.trackValueNames.length || cfg.showGatedTracks || cfg.showTrackclust) {
    await pushTracks(projectUid, imageUid, {
      valueNames: cfg.trackValueNames, showGatedTracks: cfg.showGatedTracks,
      showTrackclust: cfg.showTrackclust, colorBy: cfg.colourBy,
    })
  }
  for (const pt of cfg.popTypes) {
    await pushPopulations(projectUid, imageUid, { popType: pt, show: true, pointsSize: cfg.pointsSize })
  }
  if (cfg.colourBy) {
    await pushColourLabels(projectUid, imageUid, { column: cfg.colourBy })
  }
}
