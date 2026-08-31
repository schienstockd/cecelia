// Remainder of the once-large napari overlay module during the P9 decommission. The push builders
// (show-labels / show-tracks / show-populations / colour-labels / refresh-labels) and the
// `restoreOverlays` sequencer all went with the panel mirrors — the WebGPU viewer reads overlay
// visibility off the shared settings bag and the `pingViewerOverlays` tick, so a second request path
// is a mirror the browser viewer doesn't need. Kept here: `applyViewState` (still a POST) and
// `captureViewLegend` (still resolves the legend server-side via `/api/napari/overlay-legend`).
// Both go in PR 4 with the rest of the ImageStripView / animation capture path.
import { parseOverlays } from './overlayLayers'
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
