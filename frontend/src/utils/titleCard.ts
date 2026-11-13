// Movie title-card payload + the helpers that build it. Shared by every place that records a movie —
// the viewer's Record movie (QuickMovie), the batch page, and the animation page — so all three cards read
// identically. Split out of the viewer's overlay-push layer because a card is a MOVIE-side artefact:
// it lives on the recorded frames, not on the interactive canvas.
//
// `captureViewLegend` is the internal helper (channels from the snapshot's layer colormaps,
// populations + colour-by from the server-side legend endpoint); `buildTitleCard` composes those into
// a title + labeled sections; `unionViewSnapshot` merges several view snapshots for the animation
// case, where a card needs to describe everything shown "at some point" across the keyframes.

import { captureViewLegend } from './viewerOverlays'

// The movie title-card payload the recorder consumes. Channels are NOT included here — the
// recorder adds them from the live viewer; the frontend supplies only the non-channel sections + title.
export interface TitleCardPayload {
  enabled: boolean
  note: string
  durationSec: number
  title: string
  sections: { heading: string; items: { label: string; colour: string | null }[] }[]   // null → no swatch
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
// ANY snapshot, with its drawn colour — a channel's `colormap`, a track layer's `colour` (null = by
// track / speed) — taken from a snapshot where it's shown. Lets the animation card describe every
// channel/overlay that appears "at some point" across the keyframes. Pure.
export function unionViewSnapshot(
  snapshots: ({ layers?: Record<string, unknown> } | null | undefined)[],
): { layers: Record<string, unknown> } {
  type Layer = { colormap?: string; colour?: string | null; visible?: boolean; [k: string]: unknown }
  const merged: Record<string, Layer> = {}
  for (const s of snapshots) {
    const layers = (s?.layers ?? {}) as Record<string, Layer>
    for (const [name, l] of Object.entries(layers)) {
      const prev = merged[name]
      const shownHere = l?.visible !== false
      const next: Layer = {
        ...(prev ?? {}), ...l,
        visible: shownHere || prev?.visible === true,
        // keep a colormap from a snapshot where the layer is actually shown
        colormap: (shownHere && typeof l?.colormap === 'string') ? l.colormap
          : (prev?.colormap ?? (typeof l?.colormap === 'string' ? l.colormap : undefined)),
      }
      // same rule for a track layer's `colour`; null is a value ("by track"), so test presence
      if (prev && 'colour' in prev && !(shownHere && l != null && 'colour' in l)) next.colour = prev.colour
      merged[name] = next
    }
  }
  return { layers: merged }
}
