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
