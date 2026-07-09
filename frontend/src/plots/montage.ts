import type { GateSpec, TransformSpec } from '../stores/gating'

// ── Gate MONTAGE model ────────────────────────────────────────────────────────────────────────────
// A montage is a grid of read-only gate scatters. Each tile renders ONE parent population's cells as a
// density scatter on a channel pair (xChan × yChan), with the child gate outlines that define
// populations on that pair. This model + query builders are shared by the two montage producers — the
// read-only gating-strategy plot (tree-derived tiles) and the channel-pairs matrix (channel-product
// tiles) — so both feed the ONE renderer, GateMontage.vue (feedback_use_existing_framework). Kept as a
// plain .ts (no Vue) so the pure logic — orientation/transpose reuse — is unit-tested (docs/DEV.md).

export interface PanelChild { path: string; name: string; colour: string; gate: GateSpec }
// A tile is a scatter (fetches + renders the point cloud), the matrix diagonal (a labelled cell, no
// fetch), or a correlation cell (upper triangle of the pairs matrix — shows Pearson r reused from its
// mirror scatter's points, no fetch). Tree-derived montages (gating strategy) leave it unset → scatter.
export type TileRole = 'scatter' | 'diagonal' | 'corr'
export interface PanelDef {
  key: string                    // stable tile id (unique within a montage)
  parentPath: string             // population whose cells are the density backdrop ('root' = all events)
  parentName: string
  xChan: string; yChan: string   // the tile's axes (raw column names)
  xt: TransformSpec; yt: TransformSpec
  children: PanelChild[]         // gate outlines already oriented to (xChan,yChan)
  role?: TileRole                // default 'scatter'
}

export interface MontageId { projectUid: string; imageUid: string; valueName: string; popType: string }
export interface Ext { xMin: number; xMax: number; yMin: number; yMax: number }
export interface Tick { pos: number; label: string }

// axis-transform → query params (mirrors GatePlotPanel.axisQ; logicle carries its shape)
export function axisQ(p: 'x' | 'y', ts: TransformSpec): string {
  let q = `&${p}t=${ts.kind}`
  if (ts.kind === 'logicle') q += `&${p}T=${ts.T ?? 262144}&${p}W=${ts.W ?? 0.5}&${p}M=${ts.M ?? 4.5}&${p}A=${ts.A ?? 0}`
  return q
}
export function idQ(id: MontageId): string {
  return `projectUid=${id.projectUid}&imageUid=${id.imageUid}&valueName=${encodeURIComponent(id.valueName)}&popType=${id.popType}`
}
// fromZero (default true) → x0=1/y0=1: fixed whole-dataset axis per side, so the parent density + child
// gates align across tiles. false → x0=0/y0=0: autoscale each axis to the population. Alignment holds
// either way because a tile's x-range depends only on (x-channel, pop) and its y-range only on
// (y-channel, pop) — never on the OTHER axis — so a matrix column (shared x) and row (shared y) still
// line up when autoscaled. Mirrors GatePlotPanel's axisFromZero → x0/y0 flag.
export function plotQ(id: MontageId, pop: string, xc: string, yc: string, xt: TransformSpec, yt: TransformSpec,
                      fromZero = true): string {
  const z = fromZero ? 1 : 0
  return `${idQ(id)}&x=${encodeURIComponent(xc)}&y=${encodeURIComponent(yc)}&pop=${encodeURIComponent(pop)}${axisQ('x', xt)}${axisQ('y', yt)}&x0=${z}&y0=${z}`
}

// ── Transpose reuse ─────────────────────────────────────────────────────────────────────────────
// The pairs matrix renders BOTH (a,b) and (b,a) tiles. Their point clouds are the same data with the
// two axes swapped, so we fetch ONE canonical orientation (channels in sorted order) per (parent,pair,
// transforms) and TRANSPOSE it for the mirror tile — halving the fetches for the N×N matrix. The
// gating-strategy montage has one def per sorted pair already, so it fetches exactly as before.

// interleaved [x0,y0,x1,y1,…] → [y0,x0,y1,x1,…]
export function transposePoints(pts: Float32Array): Float32Array {
  const out = new Float32Array(pts.length)
  for (let i = 0; i + 1 < pts.length; i += 2) { out[i] = pts[i + 1]; out[i + 1] = pts[i] }
  return out
}
export const transposeExt = (e: Ext): Ext => ({ xMin: e.yMin, xMax: e.yMax, yMin: e.xMin, yMax: e.xMax })

// Pearson correlation of an interleaved [x0,y0,x1,y1,…] point cloud (the pairs matrix shows this in the
// upper triangle, ggpairs-style). Computed on the PLOTTED coords (post-transform) since that's what the
// user sees; orientation-invariant so it doesn't matter which tile of a mirror pair supplies the points.
// null when undefined (fewer than 2 points, or a degenerate axis with zero variance).
export function pearson(points: Float32Array): number | null {
  const n = Math.floor(points.length / 2)
  if (n < 2) return null
  let sx = 0, sy = 0
  for (let i = 0; i < n; i++) { sx += points[2 * i]; sy += points[2 * i + 1] }
  const mx = sx / n, my = sy / n
  let cxy = 0, cxx = 0, cyy = 0
  for (let i = 0; i < n; i++) {
    const dx = points[2 * i] - mx, dy = points[2 * i + 1] - my
    cxy += dx * dy; cxx += dx * dx; cyy += dy * dy
  }
  const d = Math.sqrt(cxx * cyy)
  return d > 0 ? cxy / d : null
}

export interface Orient { a: string; b: string; ta: TransformSpec; tb: TransformSpec; swap: boolean; groupKey: string }
// Canonical (sorted-channel) orientation for a tile + whether the tile is the mirror of it. `groupKey`
// identifies the shared fetch: same parent + same unordered channel pair + same transforms → one fetch.
export function canonicalOrient(d: Pick<PanelDef, 'parentPath' | 'xChan' | 'yChan' | 'xt' | 'yt'>): Orient {
  const swap = d.xChan > d.yChan
  const a = swap ? d.yChan : d.xChan
  const b = swap ? d.xChan : d.yChan
  const ta = swap ? d.yt : d.xt
  const tb = swap ? d.xt : d.yt
  return { a, b, ta, tb, swap, groupKey: `${d.parentPath}|${a}${axisQ('x', ta)}|${b}${axisQ('y', tb)}` }
}
