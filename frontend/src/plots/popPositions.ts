/**
 * Pure logic for the population positions plot (`components/plots/PopPositionsView.vue`): where each
 * population's cells sit on the image, frame by frame. The data is `GET /api/plots/pop-positions` —
 * Float32 `[x, y, t, popIdx, value]` per cell, `popIdx` the index of the requested population the cell is
 * coloured by — the deepest it is in (-1 = none) — and `value` its colour-by column (NaN = none). This
 * module turns it into the layers a `GateScatterCell` draws: every cell of the frame as the base (shaded
 * by its value when colouring by one), each population's cells as a coloured layer on top.
 */
import type { SeriesTarget } from './types'
import { categoricalColours } from './plot'
import { measurePickGroups, type SearchGroup } from '../utils/searchSelect'
import { lensLabel } from '../utils/viewer/lensQuery'

/** Floats per cell on the wire: x, y, t, popIdx, value. */
export const POS_STRIDE = 5

/** One population a tile colours: the request token, its legend name and its colour. */
export interface PosPop { token: string; label: string; colour: string }

/** The wire token for a population (`popType~valueNamePrefixedPath`), as `/api/plots/cluster-map` and
 *  `/api/plots/pop-positions` read it (Julia `pop_index_by_label`). */
export const popWireToken = (t: SeriesTarget): string => `${t.popType}~${t.valueName}${t.pop}`

/**
 * The populations to colour, in request order (the order is the colour order; a cell in
 * several takes the deepest — server-side, `pop_index_by_label`).
 *
 * Two sources, one per host: the population RAIL's picks (`series`, already narrowed to this plot's
 * family) on the board and the summary pages, or — on a Cluster / Region page, which has its own manager
 * — the populations that manager has switched on (`shownPops`, bare paths under `popType`). A
 * population keeps its own colour where it has one; the rest take the population palette in order.
 */
export function positionPops(input: {
  series?: readonly SeriesTarget[]
  shownPops?: readonly { path: string; name: string; colour?: string }[]
  popType: string
  /** the colour a rail population carries in its gating map, when known */
  colourOf?: (t: SeriesTarget) => string | undefined
}): PosPop[] {
  const raw: { token: string; label: string; colour?: string }[] = input.series?.length
    ? input.series.map(t => ({ token: popWireToken(t), label: t.pop.split('/').filter(Boolean).pop() ?? t.pop,
                               colour: input.colourOf?.(t) }))
    : (input.shownPops ?? []).map(p => ({ token: `${input.popType}~${p.path}`, label: p.name, colour: p.colour }))
  const fallback = categoricalColours(raw.length)
  return raw.map((p, i) => ({ ...p, colour: p.colour || fallback[i]! }))
}

/** The distinct frames in one image's data, ascending. */
export function framesIn(buf: Float32Array): number[] {
  const s = new Set<number>()
  for (let i = 2; i < buf.length; i += POS_STRIDE) s.add(buf[i]!)
  return [...s].sort((a, b) => a - b)
}

/**
 * One image's cells as drawable layers: `base` = every cell of `frame` as xy pairs (null = every frame),
 * `pops[k]` = the cells of population `k` among them. A cell sits in at most one population layer
 * (the server picks which), so the colours never stack.
 */
export function positionLayers(buf: Float32Array, frame: number | null, nPops: number):
    { base: Float32Array; values: Float32Array; pops: Float32Array[]; n: number } {
  const base: number[] = [], values: number[] = []
  const pops: number[][] = Array.from({ length: nPops }, () => [])
  for (let i = 0; i + POS_STRIDE <= buf.length; i += POS_STRIDE) {
    if (frame !== null && buf[i + 2] !== frame) continue
    const x = buf[i]!, y = buf[i + 1]!, k = buf[i + 3]!
    base.push(x, y); values.push(buf[i + 4]!)
    if (k >= 0 && k < nPops) pops[k]!.push(x, y)
  }
  return { base: Float32Array.from(base), values: Float32Array.from(values),
           pops: pops.map(p => Float32Array.from(p)), n: base.length / 2 }
}

/** The colour-by picker's groups: the lens scores first (Find similar's score maps — the reason the
 *  setting exists), then every other cell column; '' = colour by populations. */
export function colourByGroups(columns: readonly string[], lensNames: Record<string, string> = {}): SearchGroup[] {
  const lens = columns.filter(c => c.startsWith('spatial.lens.'))
  const rest = columns.filter(c => !c.startsWith('spatial.lens.'))
  return measurePickGroups([{ title: 'Lens scores', cols: lens }, { title: 'Cell measures', cols: rest }]
                             .filter(g => g.cols.length),
                           c => (lens.includes(c) ? lensLabel(c, lensNames) : c),
                           [{ value: '', label: 'Populations' }])
}

/** ONE colour scale over every tile, so one colour is one value on every image: the span of the tiles'
 *  ranges (a lens score's is fixed server-side, so every tile already agrees); ticks inside it, deduped.
 *  null when no tile has values. */
export function sharedValueScale(metas: readonly { valueRange?: [number, number] | null;
                                                   valueTicks?: { pos: number; label: string }[] }[]):
    { extent: [number, number]; ticks: { pos: number; label: string }[] } | null {
  const rs = metas.map(m => m.valueRange).filter((r): r is [number, number] => !!r)
  if (!rs.length) return null
  const extent: [number, number] = [Math.min(...rs.map(r => r[0])), Math.max(...rs.map(r => r[1]))]
  const seen = new Map<number, { pos: number; label: string }>()
  for (const m of metas) for (const t of m.valueTicks ?? [])
    if (t.pos >= extent[0] && t.pos <= extent[1] && !seen.has(t.pos)) seen.set(t.pos, t)
  return { extent, ticks: [...seen.values()].sort((a, b) => a.pos - b.pos) }
}

/** The frame a tile shows: the persisted pick when the data has it, else the first frame; null = all. */
export function resolveFrame(want: number | null | undefined, frames: readonly number[]): number | null {
  if (want === null) return null
  if (want !== undefined && frames.includes(want)) return want
  return frames.length > 1 ? (frames[0] ?? null) : null    // a still has nothing to step through
}

/**
 * The largest box of aspect `aspect` (width / height) that fits a `w × h` cell — `object-fit: contain`
 * in numbers, so a map is never stretched and a bigger panel always means a bigger map.
 */
export function containBox(w: number, h: number, aspect: number): { w: number; h: number } {
  if (!(w > 0 && h > 0)) return { w: 0, h: 0 }
  const a = aspect > 0 && Number.isFinite(aspect) ? aspect : 1
  return w / h > a ? { w: h * a, h } : { w: w, h: w / a }
}
