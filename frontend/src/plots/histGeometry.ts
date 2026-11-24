// 1D gates on a channel's histogram — docs/POPULATION.md → 1D gates.
//
//   range     an interval [lo, hi] — one population
//   bisector  one cut splitting the axis: `name+` above it (side 1), `name-` at or below (side -1). The
//             two halves share a `group`, like a slice's sides, so dragging the cut moves both. A rule
//             (`{kind: 'valley', lo, hi}`) makes the server put the cut at the parent's density valley
//             inside that window, re-found on every image — the cut is then not draggable, the window is.
//
// A 1D gate names its channel as both x_channel and y_channel; the histogram plot asks the server for
// x = y = that channel, so the server's outlines (plotmeta) match it like any 2D gate. Geometry is in
// the plot's x data coordinates; the histogram's y is counts, which no gate uses.
import type { GateSpec } from '../stores/gating'
import { niceStep } from '../utils/niceStep'

type Box = { xMin: number; xMax: number; yMin: number; yMax: number }

/** The Y "measure" that makes a gate plot a histogram of its X channel (stored in the plot's `ui.y`). */
export const HIST_AXIS = '__histogram__'

export const isHistGate = (g?: Partial<Pick<GateSpec, 'kind'>> | null): boolean =>
  g?.kind === 'range' || g?.kind === 'bisector'

/** Bins across the histogram's x range. */
export const HIST_BINS = 128

/** Counts of the x values (every `stride`-th float of `points`, from 0) in `n` equal bins over [lo, hi].
 *  Values outside, or not finite, are not counted. */
export function histCounts(points: Float32Array, lo: number, hi: number, n = HIST_BINS, stride = 2): number[] {
  const out = new Array<number>(n).fill(0)
  const w = (hi - lo) / n
  if (!(w > 0)) return out
  for (let i = 0; i < points.length; i += stride) {
    const v = points[i]
    if (!Number.isFinite(v) || v < lo || v > hi) continue
    out[Math.min(n - 1, Math.floor((v - lo) / w))]++
  }
  return out
}

/** Count-axis ticks from 0 to `max`: a 1/2/5 × 10^k step, at most ~5 ticks. */
export function countTicks(max: number): { pos: number; label: string }[] {
  if (!(max > 0)) return [{ pos: 0, label: '0' }]
  const step = niceStep(max / 5)
  const out: { pos: number; label: string }[] = []
  for (let v = 0; v <= max + 1e-9; v += step) out.push({ pos: v, label: String(Math.round(v)) })
  return out
}

/** `base+` (above the cut) and `base-` for a new bisector. `/` is the path separator, so it is dropped. */
export function bisectorRegions(base: string): { name: string; region: number }[] {
  const b = base.replace(/\//g, '').trim()
  return [{ name: `${b}+`, region: 1 }, { name: `${b}-`, region: -1 }]
}

/** The search window a new valley rule starts with: a fifth of the view either side of the cut, inside
 *  the view. Narrow on purpose — the pile of zero-intensity cells at the axis floor is a "population" a
 *  whole-axis window would cut against. */
export function defaultRuleWindow(cut: number, view: Box): [number, number] {
  const span = view.xMax - view.xMin
  return [Math.max(view.xMin, cut - 0.2 * span), Math.min(view.xMax, cut + 0.2 * span)]
}

/** The x interval a 1D gate's population covers in the view (for its shading and its label). */
export function histRegion(g: GateSpec, view: Box): [number, number] | null {
  let a: number, b: number
  if (g.kind === 'range') { a = g.lo!; b = g.hi! }
  else if (g.kind === 'bisector') { [a, b] = g.side === -1 ? [view.xMin, g.cut!] : [g.cut!, view.xMax] }
  else return null
  a = Math.max(a, view.xMin); b = Math.min(b, view.xMax)
  return b > a ? [a, b] : null
}

/** The x positions a 1D gate's handles sit at: a range's two ends, a fixed bisector's cut, a rule
 *  bisector's window ends (its cut is the rule's to place). */
export function histHandles(g: GateSpec): { part: 'lo' | 'hi' | 'cut' | 'rule-lo' | 'rule-hi'; x: number }[] {
  if (g.kind === 'range') return [{ part: 'lo', x: g.lo! }, { part: 'hi', x: g.hi! }]
  if (g.kind !== 'bisector') return []
  return g.rule ? [{ part: 'rule-lo', x: g.rule.lo }, { part: 'rule-hi', x: g.rule.hi }] : [{ part: 'cut', x: g.cut! }]
}

/** `g` with one handle moved to x (the ends kept in order). */
export function moveHistHandle(g: GateSpec, part: string, x: number): GateSpec {
  if (part === 'lo' || part === 'hi') {
    const lo = part === 'lo' ? x : g.lo!, hi = part === 'hi' ? x : g.hi!
    return { ...g, lo: Math.min(lo, hi), hi: Math.max(lo, hi) }
  }
  if (part === 'cut') return { ...g, cut: x }
  if ((part === 'rule-lo' || part === 'rule-hi') && g.rule) {
    const lo = part === 'rule-lo' ? x : g.rule.lo, hi = part === 'rule-hi' ? x : g.rule.hi
    return { ...g, rule: { ...g.rule, lo: Math.min(lo, hi), hi: Math.max(lo, hi) } }
  }
  return g
}

/** A bisector switched between a fixed cut and following the valley: on, the window starts around the
 *  current cut (`defaultRuleWindow`); off, the cut stays where the rule last put it. */
export function toggleRule(g: GateSpec, view: Box): GateSpec {
  if (g.kind !== 'bisector') return g
  if (g.rule) { const { rule: _r, rule_ok: _ok, ...rest } = g; return rest }
  const [lo, hi] = defaultRuleWindow(g.cut!, view)
  return { ...g, rule: { kind: 'valley', lo, hi } }
}
