import type { FlatPop, GateSpec, TransformSpec } from '../stores/gating'
import { orientGate } from './gateGeometry'
import type { PanelDef, PanelChild, TileRole } from './montage'

// ── Channel-pairs matrix (R ggpairs()) ───────────────────────────────────────────────────────────
// Turn a set of channels into a scatter-plot MATRIX for ONE displayed population — the single gate
// plot's X/Y generalised to a list (the user's ask). Columns = X channel, rows = Y channel. Laid out
// like GGally::ggpairs: the LOWER triangle holds the scatters, the DIAGONAL names each channel, and the
// UPPER triangle shows each pair's correlation (reused from its mirror scatter — no extra fetch). Only
// the lower triangle fetches, so it's N(N-1)/2 point clouds, not N². On each scatter the displayed
// population's child gates whose channel-pair matches that tile — in EITHER order (orientGate) — are
// attached as outlines, so the gates that define the populations show (read-only). Pure + unit-tested.

// one transform applied to every axis (default logicle for flow, linear for track). Logicle carries its
// FlowJo shape (matches GatePlotPanel.tspec) so the axis reads identically to the single plot.
export const pairTransform = (k: TransformSpec['kind']): TransformSpec =>
  k === 'logicle' ? { kind: k, T: 262144, W: 0.5, M: 4.5, A: 0 } : { kind: k }

// Reconcile a persisted channel selection with the current segmentation's columns: drop any channel
// that no longer exists (a value_name switch changes the columns — else buildPairDefs would ask
// plotdata for a missing column → broken tiles), and if that empties the list, seed the first few
// defaults. Pure so the segmentation-switch pruning is unit-tested; mirrors the single plot's prune.
export function reconcileChannels(selected: string[], columns: string[], defaults: string[], max = 4): string[] {
  if (!columns.length) return selected
  const valid = selected.filter(c => columns.includes(c))
  return valid.length ? valid : defaults.slice(0, Math.min(max, defaults.length))
}

// Estimate the fetch/render load of an N×N matrix so the panel can warn before a heavy load. Each
// off-diagonal channel PAIR fetches the population's cells once (mirror tiles share it), so the point
// load ≈ pairs × population cell-count. `parentCount` is null when unknown (e.g. 'root') → fall back to
// a tile-count heuristic. Pure + unit-tested.
export interface MatrixLoad { tiles: number; fetches: number; estPoints: number | null; heavy: boolean }
export function estimateMatrixLoad(nChannels: number, parentCount: number | null, heavyPoints = 1_000_000): MatrixLoad {
  const tiles = nChannels * nChannels
  const fetches = (nChannels * (nChannels - 1)) / 2
  const estPoints = parentCount != null ? fetches * parentCount : null
  const heavy = nChannels < 2 ? false : (estPoints != null ? estPoints >= heavyPoints : nChannels >= 5)
  return { tiles, fetches, estPoints, heavy }
}

export function buildPairDefs(
  channels: string[], parentPath: string, kind: TransformSpec['kind'], children: FlatPop[],
): PanelDef[] {
  const ts = pairTransform(kind)
  const parentName = parentPath === 'root' ? 'all events' : parentPath
  const gated = children.filter(c => c.gate)
  const defs: PanelDef[] = []
  // ggpairs layout: LOWER triangle (row > col) = scatter; DIAGONAL = channel name; UPPER triangle
  // (row < col) = the correlation of its mirror. Only scatter tiles fetch, so we render N(N-1)/2 point
  // clouds instead of N² — half the load, no duplicate (a,b)/(b,a) plots.
  channels.forEach((yc, ri) => {             // rows (y channel)
    channels.forEach((xc, ci) => {           // columns (x channel)
      const role: TileRole = ri === ci ? 'diagonal' : ri > ci ? 'scatter' : 'corr'
      const tileChildren: PanelChild[] = role !== 'scatter' ? [] : gated
        .map(c => {
          const g = orientGate(c.gate as GateSpec, xc, yc)
          return g ? { path: c.path, name: c.name, colour: c.colour, gate: g } : null
        })
        .filter((c): c is PanelChild => c !== null)
      defs.push({
        key: `${parentPath}::${xc}~~${yc}`,
        parentPath, parentName, xChan: xc, yChan: yc, xt: ts, yt: ts,
        children: tileChildren, role,
      })
    })
  })
  return defs
}
