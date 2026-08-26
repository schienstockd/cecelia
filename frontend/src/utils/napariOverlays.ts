// The ONE place that builds the napari overlay requests (show-tracks / show-populations /
// colour-labels). Both the interactive ViewerPanel and the non-interactive callers (zoom-to-source,
// the strip) go through these builders, so there's a single request shape per endpoint instead of two
// divergent inline copies. Each builder returns the raw Response (or undefined on a network error) so
// callers can still harvest the legend from the reply; it does not read/parse the body itself.
import { parseOverlays, type OverlayPushConfig } from './overlayLayers'
import { channelLegend, type LegendItem } from './viewLegend'
import { debouncedLatest } from './debouncedLatest'

const _post = (path: string, body: unknown): Promise<Response | undefined> =>
  fetch(path, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })
    .catch(() => undefined)

// ── Live view-property pushes (coalesced — see docs/UI.md → "Continuous controls") ──────────────
// The pushes a SLIDER drives, as opposed to the one-shot overlay builders below.
//
// `<input type="range">` emits an event per pixel of travel — a short drag is 20–60 events — and each
// of these lands a napari command that costs a plane load or a layer-props apply. The bridge processes
// one command at a time (see `restoreOverlays`), so pushing per event queues seconds of already-
// superseded work: the viewer keeps stepping through z slices long after the mouse was released.
//
// So each live push owns ONE module-level `debouncedLatest`. A burst collapses to a single call; an
// event arriving mid-flight REPLACES the pending argument instead of queueing behind it; and because
// the scheduler will not start a second call while one is in flight, it self-paces to however slow
// napari actually is on this image. The scheduler is module-level on purpose — there is one viewer, so
// one scheduler per endpoint, and a second call site cannot reintroduce the spam.
//
// The wait is short deliberately: these are settings you judge by WATCHING the viewer, so the push has
// to track the drag rather than wait for it to finish. Coalescing, not deferral.
const LIVE_PUSH_WAIT = 80

const _zViewPush = debouncedLatest<{ show3D: boolean; zSlice: number | null }>(
  async a => { await _post('/api/napari/set-z-view', { show3D: a.show3D, zSlice: a.show3D ? null : a.zSlice }) },
  { wait: LIVE_PUSH_WAIT },
)
/** Whole stack in 3D, or one z slice in 2D. Fire-and-forget: napari not running is not an error here —
 *  the value is persisted by the caller and applies on the next open. */
export function pushZView(show3D: boolean, zSlice: number | null): void {
  _zViewPush.schedule({ show3D, zSlice })
}

const _contourPush = debouncedLatest<Record<string, { contour: number }>>(
  async layers => { await _post('/api/napari/apply-view-state', { viewState: { layers } }) },
  { wait: LIVE_PUSH_WAIT },
)
/** Mask outline width on the label layers currently on screen. `contour` is a captured view prop
 *  (`napari_utils._VIEW_LAYER_KEYS`), so a PARTIAL view-state apply is enough — the layer keeps its
 *  data, position and colouring, where a show-labels rebuild would re-read the store. */
export function pushLabelContour(valueNames: string[], contour: number): void {
  if (!valueNames.length) return
  const layers: Record<string, { contour: number }> = {}
  for (const vn of valueNames) layers[`(${vn}) Labels`] = { contour }
  _contourPush.schedule(layers)
}

const _detailPush = debouncedLatest<number | null>(
  async level => { await _post('/api/napari/set-3d-level', { level }) },
  { wait: LIVE_PUSH_WAIT },
)
/** How much detail the 3D view renders: a multiscale level index (0 = full resolution, higher =
 *  coarser), or null for napari's own choice. Coalesced — it is a slider, and each change re-slices
 *  every multiscale layer in the viewer. */
export function pushDetail3d(level: number | null): void {
  _detailPush.schedule(level)
}

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

// The movie title-card payload the recorder consumes (Phase H). Channels are NOT included here — the
// recorder adds them from the live viewer; the frontend supplies only the non-channel sections + title.
export interface TitleCardPayload {
  enabled: boolean
  note: string
  durationSec: number
  title: string
  sections: { heading: string; items: { label: string; colour: string }[] }[]
}
// Build the title-card payload for a captured view — the ONE builder shared by single-record and the
// animation page (both live-view paths). Title = image name + its attribute values; Populations +
// colour-by sections come from captureViewLegend (the same path as the board strip). Channels are
// normally added by the recorder from the live viewer and omitted here — EXCEPT when `includeChannels`
// is set (the animation page, which passes a UNION snapshot across all keyframes so the card reflects
// everything shown "at some point"; the recorder can't reconstruct that union from one live view).
export async function buildTitleCard(
  projectUid: string, imageUid: string,
  snapshot: { layers?: Record<string, unknown> } | null | undefined,
  image: { name?: string; attr?: Record<string, string> } | null | undefined,
  opts: { note: string; durationSec: number; colourBy: string; colourOverrides?: Record<string, string>; includeChannels?: boolean },
): Promise<TitleCardPayload> {
  const leg = await captureViewLegend(projectUid, imageUid, snapshot, opts.colourBy, opts.colourOverrides ?? {})
  const sections: TitleCardPayload['sections'] = []
  if (opts.includeChannels && leg.channels.length) sections.push({ heading: 'Channels', items: leg.channels })
  const pops = leg.populations.map(p => ({ label: p.name, colour: p.colour }))
  if (pops.length) sections.push({ heading: 'Populations', items: pops })
  const cby = (leg.colourBy?.items ?? []).filter(it => it.colour).map(it => ({ label: it.label, colour: it.colour }))
  if (cby.length) sections.push({ heading: leg.colourBy?.column || 'Colour by', items: cby })
  const attrs = image?.attr ? Object.keys(image.attr).sort().map(k => image.attr![k]?.trim()).filter(Boolean) : []
  const title = [image?.name ?? '', ...attrs].filter(Boolean).join(' — ')
  return { enabled: true, note: opts.note, durationSec: opts.durationSec, title, sections }
}

// Merge the layers of several view snapshots into ONE — a layer is present/visible if it's visible in
// ANY snapshot, with a colormap taken from a snapshot where it's shown. Lets the animation card describe
// every channel/overlay that appears "at some point" across the keyframes (Phase H4). Pure.
export function unionViewSnapshot(
  snapshots: ({ layers?: Record<string, unknown> } | null | undefined)[],
): { layers: Record<string, unknown> } {
  const merged: Record<string, { colormap?: string; visible?: boolean; [k: string]: unknown }> = {}
  for (const s of snapshots) {
    const layers = (s?.layers ?? {}) as Record<string, { colormap?: string; visible?: boolean }>
    for (const [name, l] of Object.entries(layers)) {
      const prev = merged[name]
      const shownHere = l?.visible !== false
      merged[name] = {
        ...(prev ?? {}), ...l,
        visible: shownHere || prev?.visible === true,
        // keep a colormap from a snapshot where the layer is actually shown
        colormap: (shownHere && typeof l?.colormap === 'string') ? l.colormap
          : (prev?.colormap ?? (typeof l?.colormap === 'string' ? l.colormap : undefined)),
      }
    }
  }
  return { layers: merged }
}

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
