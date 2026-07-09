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
