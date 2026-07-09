import type { FlatPop, GateSpec, TransformSpec } from '../stores/gating'
import { orientGate } from './gateGeometry'
import type { PanelDef, PanelChild } from './montage'

// ── Channel-pairs matrix (R pairs()) ────────────────────────────────────────────────────────────
// Turn a set of channels into the full N×N montage of every channel-vs-channel scatter for ONE
// displayed population — the single gate plot's X/Y generalised to a list (the user's ask). Columns =
// X channel, rows = Y channel. The diagonal (channel vs itself) is a labelled cell, not a scatter. On
// each off-diagonal tile the displayed population's child gates whose channel-pair matches that tile —
// in EITHER order (orientGate) — are attached as outlines, so the gates that define the populations
// show, exactly as on the normal gating plot (but read-only). Pure + unit-tested (docs/DEV.md); the
// tiles it returns feed the shared GateMontage renderer.

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
  for (const yc of channels) {               // rows
    for (const xc of channels) {             // columns
      const diagonal = xc === yc
      const tileChildren: PanelChild[] = diagonal ? [] : gated
        .map(c => {
          const g = orientGate(c.gate as GateSpec, xc, yc)
          return g ? { path: c.path, name: c.name, colour: c.colour, gate: g } : null
        })
        .filter((c): c is PanelChild => c !== null)
      defs.push({
        key: `${parentPath}::${xc}~~${yc}`,
        parentPath, parentName, xChan: xc, yChan: yc, xt: ts, yt: ts,
        children: tileChildren, diagonal,
      })
    }
  }
  return defs
}
