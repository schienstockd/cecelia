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
