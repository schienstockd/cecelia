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
import type { GateSpec, GateRule } from '../stores/gating'
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

/** Everything a histogram plot shows over the x view [xMin, xMax]: the bins (base + each overlay pop),
 *  the count axis's top (a little headroom over the tallest bin) and its ticks. One builder for the Gate
 *  page's plot and the gating-strategy montage's 1D tiles, so both draw the same histogram. */
export function histogramView(points: Float32Array, layers: { colour: string; points: Float32Array }[],
                              xMin: number, xMax: number) {
  const base = histCounts(points, xMin, xMax)
  const max = Math.max(1, ...base)
  return {
    hist: { lo: xMin, hi: xMax, base, layers: layers.map(l => ({ colour: l.colour, counts: histCounts(l.points, xMin, xMax) })) },
    yMax: max * 1.08,
    yTicks: countTicks(max),
  }
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

// A 1D gate's rules, keyed by the field that holds each: a bisector's `rule` (its cut), a range's
// `lo_rule` / `hi_rule` (its ends).
type RuleField = 'rule' | 'lo_rule' | 'hi_rule'
const END_RULE = { lo: 'lo_rule', hi: 'hi_rule', cut: 'rule' } as const
// the valley rule held in field `f` (a `rule` may also be a 2D shape's follow rule — not a 1D one)
const ruleAt = (g: GateSpec, f: RuleField): GateRule | undefined => {
  const r = g[f]; return r?.kind === 'valley' ? r : undefined
}

/** A 1D gate's vertical lines: each end / the cut, whether a rule places it, and whether that rule found
 *  its valley on this parent (a miss is drawn dashed — the stored position stands in). */
export function histLines(g: GateSpec): { end: 'lo' | 'hi' | 'cut'; x: number; ruled: boolean; ok: boolean }[] {
  const line = (end: 'lo' | 'hi' | 'cut', x: number, rule?: GateRule, ok?: boolean) =>
    ({ end, x, ruled: !!rule, ok: ok !== false })
  if (g.kind === 'range') return [line('lo', g.lo!, g.lo_rule, g.lo_rule_ok), line('hi', g.hi!, g.hi_rule, g.hi_rule_ok)]
  if (g.kind === 'bisector') return [line('cut', g.cut!, ruleAt(g, 'rule'), g.rule_ok)]
  return []
}

/** The windows a 1D gate's rules search (shaded on the plot) — what the server searches (`end_window`):
 *  a range's two end windows split their overlap at its middle, and one ruled end beside a fixed end
 *  stops at that end. An emptied window is left out. */
export function histWindows(g: GateSpec): [number, number][] {
  if (g.kind !== 'range') { const r = ruleAt(g, 'rule'); return r ? [[r.lo, r.hi]] : [] }
  const out: [number, number][] = []
  const L = g.lo_rule, H = g.hi_rule
  if (L) { const b = H ? (H.lo < L.hi ? (H.lo + L.hi) / 2 : L.hi) : Math.min(L.hi, g.hi!); if (b > L.lo) out.push([L.lo, b]) }
  if (H) { const a = L ? (H.lo < L.hi ? (H.lo + L.hi) / 2 : H.lo) : Math.max(H.lo, g.lo!); if (H.hi > a) out.push([a, H.hi]) }
  return out
}

/** The x positions a 1D gate's handles sit at: a fixed end / cut, or a ruled one's two window ends (its
 *  position is the rule's to place). `part` = the end itself, or `<rule field>-lo|hi`. */
export function histHandles(g: GateSpec): { part: string; x: number }[] {
  const out: { part: string; x: number }[] = []
  for (const l of histLines(g)) {
    const f = END_RULE[l.end], r = ruleAt(g, f)
    if (r) out.push({ part: `${f}-lo`, x: r.lo }, { part: `${f}-hi`, x: r.hi })
    else out.push({ part: l.end, x: l.x })
  }
  return out
}

/** `g` with one handle moved to x (the ends, and a window's ends, kept in order). */
export function moveHistHandle(g: GateSpec, part: string, x: number): GateSpec {
  if (part === 'lo' || part === 'hi') {
    const lo = part === 'lo' ? x : g.lo!, hi = part === 'hi' ? x : g.hi!
    return { ...g, lo: Math.min(lo, hi), hi: Math.max(lo, hi) }
  }
  if (part === 'cut') return { ...g, cut: x }
  const m = /^(rule|lo_rule|hi_rule)-(lo|hi)$/.exec(part)
  const f = m?.[1] as RuleField | undefined, r = f ? ruleAt(g, f) : undefined
  if (!f || !r) return g
  const lo = m![2] === 'lo' ? x : r.lo, hi = m![2] === 'hi' ? x : r.hi
  return { ...g, [f]: { ...r, lo: Math.min(lo, hi), hi: Math.max(lo, hi) } }
}

/** A range moved by dx — its ends and any rule windows with them. */
export function shiftRange(g: GateSpec, dx: number): GateSpec {
  const mv = (r?: GateRule) => r && { ...r, lo: r.lo + dx, hi: r.hi + dx }
  return { ...g, lo: g.lo! + dx, hi: g.hi! + dx, lo_rule: mv(g.lo_rule), hi_rule: mv(g.hi_rule) }
}

/** A bisector's cut (`end` 'cut', the default) or a range's end switched between a fixed position and
 *  following the valley: on, the window starts around the current position (`defaultRuleWindow`); off,
 *  it stays where the rule last put it. */
export function toggleRule(g: GateSpec, view: Box, end: 'lo' | 'hi' | 'cut' = 'cut'): GateSpec {
  if (!isHistGate(g) || (g.kind === 'bisector') !== (end === 'cut')) return g
  const f = END_RULE[end], okKey = `${f}_ok` as 'rule_ok' | 'lo_rule_ok' | 'hi_rule_ok'
  if (g[f]) { const out = { ...g }; delete out[f]; delete out[okKey]; return out }
  const at = end === 'cut' ? g.cut! : g[end]!
  const [lo, hi] = defaultRuleWindow(at, view)
  return { ...g, [f]: { kind: 'valley', lo, hi } }
}

/** What a 1D gate's hovered handle or ruled line means, in one short line (drawn beside it): a window
 *  edge, a fixed end / cut, or a ruled one — which says whether its rule found a valley here. */
export function histTip(g: GateSpec, hit: { kind: 'hist-handle'; part: string } | { kind: 'hist-line'; end: 'lo' | 'hi' | 'cut' }): string {
  if (hit.kind === 'hist-handle') {
    if (/rule-(lo|hi)$/.test(hit.part)) return 'Valley search window · drag to widen'
    return 'Drag · double-click: follow the valley'
  }
  const l = histLines(g).find(x => x.end === hit.end)
  const what = hit.end === 'cut' ? 'cut' : 'end'
  return l && !l.ok ? `No valley in window: ${what} stays put` : `${what === 'cut' ? 'Cut' : 'End'} at the valley · double-click: fix it`
}
