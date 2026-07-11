// FlowJo-style "pseudocolour" blue-heat ramp for the 2D density renderer (plots/density.ts →
// PlotLayers). Low end lifted off pure black so sparse density stays visible.
// (R: .flowColorRampBlueHeat, flowHelpers.R:775.)

export function hexRgb(h: string): [number, number, number] {
  return [parseInt(h.slice(1, 3), 16), parseInt(h.slice(3, 5), 16), parseInt(h.slice(5, 7), 16)]
}

export const BLUE_HEAT_ANCHORS = ['#0b1a4d', '#1793ff', '#04fa00', '#ffa805', '#ff3856']

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
