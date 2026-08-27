// FlowJo-style "pseudocolour" blue-heat ramp for the 2D density renderer (plots/density.ts →
// PlotLayers). Low end lifted off pure black so sparse density stays visible.
// (R: .flowColorRampBlueHeat, flowHelpers.R:775.)
//
// The five anchors live in `palettes.json` under `heatRamp` — same file the Julia offline renderer
// reads to interpolate the track "speed" ramp. One list, two callers, so a dot on a plot and a
// track segment in a movie cannot disagree.
import palettesJson from './palettes.json'

export function hexRgb(h: string): [number, number, number] {
  return [parseInt(h.slice(1, 3), 16), parseInt(h.slice(3, 5), 16), parseInt(h.slice(5, 7), 16)]
}

export const BLUE_HEAT_ANCHORS: readonly string[] = palettesJson.heatRamp

// packed RGB lookup (256×3): index a 0..255 density bucket → [r,g,b]
export const BLUE_HEAT_RGB: Uint8ClampedArray = (() => {
  const rgb = BLUE_HEAT_ANCHORS.map(hexRgb), n = 256
  const out = new Uint8ClampedArray(n * 3)
  for (let i = 0; i < n; i++) {
    const t = (i / (n - 1)) * (rgb.length - 1)
    const k = Math.min(rgb.length - 2, Math.floor(t)), f = t - k
    for (let j = 0; j < 3; j++) out[i * 3 + j] = Math.round(rgb[k][j] + (rgb[k + 1][j] - rgb[k][j]) * f)
  }
  return out
})()

// The ramp's ONE lookup: a 0..1 position → its CSS colour. Both users index it here rather than doing
// their own arithmetic on BLUE_HEAT_RGB — the density buckets and the colour-by ramp (dots + the
// colour bar that labels them) have to agree, or the legend describes a colour the dots don't use.
export function heatCss(t: number): string {
  const i = Math.min(255, Math.max(0, Math.round((isFinite(t) ? t : 0) * 255))) * 3
  return `rgb(${BLUE_HEAT_RGB[i]},${BLUE_HEAT_RGB[i + 1]},${BLUE_HEAT_RGB[i + 2]})`
}
