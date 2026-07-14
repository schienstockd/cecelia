// The shared "view legend" model — the backbone for describing what a napari view shows, as
// colour swatches: image channels (by colormap), populations, and a categorical colour-by. One model
// feeds every legend: the analysis-board image strip, the animation page, and (later) movie overlays.
// Pure + unit-tested; the presentational <ViewLegend> component renders a LegendSection[].
// See docs/todo/ANIMATION_PLAN.md (Phase C — the legend derives from the view snapshot).
import { napariColormapHex } from './napariColormap'

export interface LegendItem { label: string; colour: string }
export interface LegendSection { title: string; items: LegendItem[] }

/**
 * Channel legend from a view snapshot's napari layers: visible layers whose colormap is a single-hue
 * channel colour (continuous maps / labels / tracks return no colour and are skipped). Ported from
 * `ImageStripView.legendFor` so the strip and everything else derive channels the same way.
 */
export function channelLegend(
  layers: Record<string, { colormap?: string; visible?: boolean }> | undefined | null,
): LegendItem[] {
  const out: LegendItem[] = []
  for (const [name, l] of Object.entries(layers ?? {})) {
    if (l?.visible === false) continue
    const colour = napariColormapHex(l?.colormap)
    if (colour) out.push({ label: name, colour })
  }
  return out
}

/**
 * Assemble the non-empty legend sections in a stable order (channels → populations → colour-by) for
 * `<ViewLegend>`. Empty groups are dropped so a legend never shows a bare heading.
 */
export function viewLegendSections(parts: {
  channels?: LegendItem[]
  populations?: LegendItem[]
  colourBy?: LegendItem[]
}): LegendSection[] {
  const sections: LegendSection[] = []
  if (parts.channels?.length)    sections.push({ title: 'Channels', items: parts.channels })
  if (parts.populations?.length) sections.push({ title: 'Populations', items: parts.populations })
  if (parts.colourBy?.length)    sections.push({ title: 'Colour by', items: parts.colourBy })
  return sections
}
