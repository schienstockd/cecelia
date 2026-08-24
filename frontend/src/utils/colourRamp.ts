// Continuous colour ramps for the volume viewer's overlays — the ONE place a value becomes a colour
// on a ramp in this app.
//
// WHY THIS EXISTS AT ALL, given the codebase's rule against a second palette. There was no ramp to
// reuse. `image_render.jl` says so explicitly: napari's perceptual maps are not ramps from black, so it
// cannot approximate them from a name and falls back to gray unless a props file carries the LUT. The
// plots get theirs from Observable Plot (`scheme: 'turbo'`), which is a d3 scale and not reachable from
// WGSL or from a Float32Array. So overlay colour-by needed a table, and this is it — one table, two
// ramps, used by the viewer only.
//
// CHANNEL colours are still the server's job (`resolved_display_specs`), and that separation is
// deliberate: a channel LUT is a stored napari property being reproduced faithfully, while an overlay
// ramp is a display choice made here. Do not resolve channel colormaps through this.
//
// GENERATED, not typed. Values are matplotlib 3.11.0's own colormaps sampled at 32 evenly spaced
// points — `colormaps['viridis'](i / (N - 1))` — which is the same source napari's `viridis`/`turbo`
// come from, so a colour-by here matches what napari showed for the same column. Regenerate with the
// snippet in docs/todo/WEB_VIEWER_PLAN.md rather than hand-editing a stop.
//
// Turbo: Mikhailov, A. (2019), "Turbo, An Improved Rainbow Colormap for Visualization", Google AI Blog.
// Viridis: Smith, N. & van der Walt, S. (2015), SciPy talk / matplotlib.

export type RampName = 'viridis' | 'turbo'

/** viridis, 32 stops. */
const VIRIDIS: readonly (readonly [number, number, number])[] = [
  [0.2670, 0.0049, 0.3294], [0.2770, 0.0503, 0.3757], [0.2823, 0.0950, 0.4173],
  [0.2829, 0.1359, 0.4534], [0.2780, 0.1804, 0.4867], [0.2693, 0.2188, 0.5096],
  [0.2573, 0.2561, 0.5266], [0.2431, 0.2921, 0.5385], [0.2259, 0.3308, 0.5473],
  [0.2105, 0.3637, 0.5522], [0.1959, 0.3954, 0.5553], [0.1823, 0.4262, 0.5571],
  [0.1681, 0.4600, 0.5581], [0.1563, 0.4896, 0.5579], [0.1448, 0.5191, 0.5566],
  [0.1337, 0.5485, 0.5535], [0.1235, 0.5817, 0.5474], [0.1194, 0.6111, 0.5390],
  [0.1248, 0.6405, 0.5271], [0.1433, 0.6695, 0.5112], [0.1807, 0.7014, 0.4882],
  [0.2264, 0.7289, 0.4628], [0.2815, 0.7552, 0.4326], [0.3441, 0.7800, 0.3974],
  [0.4219, 0.8058, 0.3519], [0.4966, 0.8264, 0.3064], [0.5756, 0.8446, 0.2564],
  [0.6576, 0.8602, 0.2031], [0.7519, 0.8750, 0.1432], [0.8353, 0.8860, 0.1026],
  [0.9162, 0.8961, 0.1007], [0.9932, 0.9062, 0.1439]
]

/** turbo, 32 stops. */
const TURBO: readonly (readonly [number, number, number])[] = [
  [0.1900, 0.0718, 0.2322], [0.2250, 0.1635, 0.4510], [0.2511, 0.2524, 0.6337],
  [0.2682, 0.3382, 0.7805], [0.2767, 0.4313, 0.9025], [0.2747, 0.5109, 0.9728],
  [0.2542, 0.5895, 0.9990], [0.2071, 0.6687, 0.9742], [0.1452, 0.7538, 0.9050],
  [0.1036, 0.8218, 0.8244], [0.0945, 0.8784, 0.7427], [0.1353, 0.9220, 0.6656],
  [0.2345, 0.9607, 0.5561], [0.3504, 0.9848, 0.4500], [0.4738, 0.9976, 0.3496],
  [0.5865, 0.9974, 0.2685], [0.6849, 0.9794, 0.2160], [0.7661, 0.9463, 0.2031],
  [0.8413, 0.8999, 0.2093], [0.9061, 0.8434, 0.2219], [0.9605, 0.7718, 0.2281],
  [0.9878, 0.7033, 0.2136], [0.9968, 0.6209, 0.1830], [0.9899, 0.5285, 0.1440],
  [0.9656, 0.4224, 0.0980], [0.9313, 0.3348, 0.0622], [0.8869, 0.2615, 0.0375],
  [0.8317, 0.1991, 0.0213], [0.7556, 0.1373, 0.0094], [0.6754, 0.0898, 0.0045],
  [0.5834, 0.0493, 0.0049], [0.4796, 0.0158, 0.0106]
]

const RAMPS: Record<RampName, readonly (readonly [number, number, number])[]> = {
  viridis: VIRIDIS,
  turbo: TURBO,
}

/**
 * Sample a ramp at `t` in 0..1, linearly interpolating between stops.
 *
 * Out of range is CLAMPED rather than wrapped: a value above the range means "at least the top of the
 * scale", and wrapping would paint the brightest cells the colour of the dimmest.
 */
export function sampleRamp(name: RampName, t: number): [number, number, number] {
  const stops = RAMPS[name] ?? VIRIDIS
  const u = Math.min(1, Math.max(0, Number.isFinite(t) ? t : 0)) * (stops.length - 1)
  const i = Math.floor(u)
  const j = Math.min(i + 1, stops.length - 1)
  const f = u - i
  const a = stops[i], b = stops[j]
  return [a[0] + (b[0] - a[0]) * f, a[1] + (b[1] - a[1]) * f, a[2] + (b[2] - a[2]) * f]
}

/** `n` evenly spaced hex colours from a ramp — for a legend, which is HTML and wants strings. */
export function rampSwatches(name: RampName, n: number): string[] {
  const out: string[] = []
  for (let i = 0; i < Math.max(1, n); i++) {
    const [r, g, b] = sampleRamp(name, n <= 1 ? 0.5 : i / (n - 1))
    const h = (v: number) => Math.round(Math.min(1, Math.max(0, v)) * 255).toString(16).padStart(2, '0')
    out.push('#' + h(r) + h(g) + h(b))
  }
  return out
}
