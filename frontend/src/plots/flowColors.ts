// FlowJo-style "pseudocolour" blue-heat ramp, shared by the raster density renderer (2D canvas) AND
// the WebGL scatter (regl, UMAP). One source of truth for the colours so the gating raster and any
// remaining WebGL plot read the same. Low end lifted off pure black so sparse density stays visible.
// (R: .flowColorRampBlueHeat, flowHelpers.R:775.)

export function hexRgb(h: string): [number, number, number] {
  return [parseInt(h.slice(1, 3), 16), parseInt(h.slice(3, 5), 16), parseInt(h.slice(5, 7), 16)]
}

// interpolate `anchors` to `n` hex stops (regl's colour parser wants hex strings)
export function buildRamp(anchors: string[], n: number): string[] {
  const rgb = anchors.map(hexRgb)
  const out: string[] = []
  const hex = (v: number) => v.toString(16).padStart(2, '0')
  for (let i = 0; i < n; i++) {
    const t = (i / (n - 1)) * (rgb.length - 1)
    const k = Math.min(rgb.length - 2, Math.floor(t)), f = t - k
    const c = [0, 1, 2].map(j => Math.round(rgb[k][j] + (rgb[k + 1][j] - rgb[k][j]) * f))
    out.push(`#${hex(c[0])}${hex(c[1])}${hex(c[2])}`)
  }
  return out
}

export const BLUE_HEAT_ANCHORS = ['#0b1a4d', '#1793ff', '#04fa00', '#ffa805', '#ff3856']
export const BLUE_HEAT_RAMP = buildRamp(BLUE_HEAT_ANCHORS, 256)   // hex stops (regl)

// packed RGB lookup (256×3) for the 2D raster renderer — index a 0..255 density bucket → [r,g,b]
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
