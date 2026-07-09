import type { GateSpec, TransformSpec } from '../stores/gating'

// ── Gate MONTAGE model ────────────────────────────────────────────────────────────────────────────
// A montage is a grid of read-only gate scatters. Each tile renders ONE parent population's cells as a
// density scatter on a channel pair (xChan × yChan), with the child gate outlines that define
// populations on that pair. This model + query builders are shared by the two montage producers — the
// read-only gating-strategy plot (tree-derived tiles) and the channel-pairs matrix (channel-product
// tiles) — so both feed the ONE renderer, GateMontage.vue (feedback_use_existing_framework). Kept as a
// plain .ts (no Vue) so the pure logic — orientation/transpose reuse — is unit-tested (docs/DEV.md).

export interface PanelChild { path: string; name: string; colour: string; gate: GateSpec }
export interface PanelDef {
  key: string                    // stable tile id (unique within a montage)
  parentPath: string             // population whose cells are the density backdrop ('root' = all events)
  parentName: string
  xChan: string; yChan: string   // the tile's axes (raw column names)
  xt: TransformSpec; yt: TransformSpec
  children: PanelChild[]         // gate outlines already oriented to (xChan,yChan)
  diagonal?: boolean             // xChan===yChan (pairs matrix): a labelled cell, no scatter fetch
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
// x0=1&y0=1 → fixed whole-dataset axis per side, so the parent density + child gates align across tiles
export function plotQ(id: MontageId, pop: string, xc: string, yc: string, xt: TransformSpec, yt: TransformSpec): string {
  return `${idQ(id)}&x=${encodeURIComponent(xc)}&y=${encodeURIComponent(yc)}&pop=${encodeURIComponent(pop)}${axisQ('x', xt)}${axisQ('y', yt)}&x0=1&y0=1`
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
