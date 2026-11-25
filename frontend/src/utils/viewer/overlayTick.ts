// The `cc.viewerOverlaysTick` ping — how a main-window writer tells the viewer (a popup with its own
// stores) to refresh its population overlays — and WHAT the viewer refetches for it.
//
// A tick names its SCOPE, so the refresh follows what actually changed:
//  - `all`  — anything (a task rewrote the table, a pop type or segmentation switched): every layer is
//             refetched whole. A bare `<imageUid>:<ts>` tick, the shape every older pinger writes, is this.
//  - `pops` — a gate write: membership moved, the cells did not. Only the edited (segmentation, pop type)
//             layer is refetched, and only its populations (`/api/viewer/overlays?part=pops`).
//  - `look` — a colour or show change: nothing is fetched. The tick carries each pop's colour + show,
//             patched onto the arrays the viewer already holds.
// Measured on a 608k-cell × 100-frame movie: every gate ping used to refetch the whole movie (42 MB,
// 0.12–0.3 s on the server, ~0.1 s to parse), a colour change included. docs/POPULATION.md →
// *Live updates and manual apply*.
//
// The colours a `look` tick carries are the gating map's, read back from the server's reply, so the
// viewer draws what a recording draws: the movie path reads the same map (`ARCHITECTURE.md` →
// *Recording parity*).

import type { OverlayPop } from '../viewerOverlays'
import { isClusterPopType } from '../clusterMeasure'

export const OVERLAY_TICK_KEY = 'cc.viewerOverlaysTick'

export type OverlayTickScope = 'all' | 'pops' | 'look'

/** One population's appearance — what a `look` tick carries per path. */
export interface PopLook { colour: string; show: boolean }

export interface OverlayTick {
  imageUid: string
  scope: OverlayTickScope
  /** the edited document — required for `pops` and `look` */
  valueName?: string
  popType?: string
  /** `look` only: path → colour + show, every population of the document */
  look?: Record<string, PopLook>
}

const RANK: Record<OverlayTickScope, number> = { look: 0, pops: 1, all: 2 }

/** The scope that covers both — a held `pops` tick followed by a `look` one must still refetch. */
export function widerScope(a: OverlayTickScope | null, b: OverlayTickScope): OverlayTickScope {
  return a === null || RANK[b] > RANK[a] ? b : a
}

/** The tick's localStorage value. `all` keeps the `<imageUid>:<ts>` shape; the scoped ones append their
 *  JSON after a second colon. The timestamp makes every write a change, which is what fires `storage`. */
export function encodeOverlayTick(t: OverlayTick, now: number = Date.now()): string {
  if (t.scope === 'all' || !t.valueName || !t.popType) return `${t.imageUid}:${now}`
  return `${t.imageUid}:${now}:` + JSON.stringify({ scope: t.scope, valueName: t.valueName, popType: t.popType,
                                                     ...(t.scope === 'look' ? { look: t.look ?? {} } : {}) })
}

/** Read a tick back. Anything that is not a well-formed scoped tick reads as `all` — the safe refresh. */
export function parseOverlayTick(value: string | null | undefined): OverlayTick | null {
  if (!value) return null
  const a = value.indexOf(':')
  const imageUid = a < 0 ? value : value.slice(0, a)
  if (!imageUid) return null
  const b = a < 0 ? -1 : value.indexOf(':', a + 1)
  if (b < 0) return { imageUid, scope: 'all' }
  try {
    const j = JSON.parse(value.slice(b + 1)) as Partial<OverlayTick>
    const scope = j.scope === 'pops' || j.scope === 'look' ? j.scope : null
    if (!scope || typeof j.valueName !== 'string' || typeof j.popType !== 'string') return { imageUid, scope: 'all' }
    if (scope === 'look' && (typeof j.look !== 'object' || j.look === null)) return { imageUid, scope: 'all' }
    return { imageUid, scope, valueName: j.valueName, popType: j.popType, ...(scope === 'look' ? { look: j.look } : {}) }
  } catch {
    return { imageUid, scope: 'all' }
  }
}

/** The tick a population-tree write sends: `look` when no population's membership changed (`changed`,
 *  from `popChanges.changedPopPaths`, is empty), else `pops`. No tree in the reply → `all`. */
export function writeScope(changed: ReadonlySet<string> | undefined): OverlayTickScope {
  return changed === undefined ? 'all' : changed.size === 0 ? 'look' : 'pops'
}

/** path → colour + show, from the store's flattened tree. Transient pops (the viewer's own selection)
 *  never reach the viewer's payload, so they are left out. */
export function lookOf(flat: readonly { path: string; colour: string; show: boolean; transient?: boolean }[]):
  Record<string, PopLook> {
  const out: Record<string, PopLook> = {}
  for (const p of flat) if (!p.transient) out[p.path] = { colour: p.colour, show: p.show }
  return out
}

/** `pops` with each listed path's colour + show from `look` — a new array when anything changed, the same
 *  one otherwise. A path the look does not list keeps what it had: membership is not this tick's business. */
export function patchPopsLook(pops: OverlayPop[], look: Readonly<Record<string, PopLook>>): OverlayPop[] {
  let changed = false
  const next = pops.map(p => {
    const l = look[p.path]
    if (!l || (l.colour === p.colour && l.show === p.show)) return p
    changed = true
    return { ...p, colour: l.colour, show: l.show }
  })
  return changed ? next : pops
}

/** What to fetch for one population layer the viewer wants. */
export type LayerFetch = 'reuse' | 'pops' | 'full'

/**
 * Per wanted (segmentation, pop type) layer, what a refresh for `tick` fetches. `held` is the colour-by
 * column each layer the viewer holds was fetched with, keyed `vn::popType`. A layer it does not hold, or
 * holds for another colour-by column, is fetched whole whatever the scope — its cells are not here yet.
 *
 * A cluster-family map is LENT to every co-clustered segmentation that has none of its own
 * (`_borrow_cluster_pop_tree`, relabelled onto that segmentation's cells), so an edit to it can move
 * the same pop type's layers on other segmentations too: those refetch their populations, on a `look`
 * tick as well — their relabelled pops are not the edited document's to patch.
 */
export function layerFetches(wanted: readonly { vn: string; popType: string }[],
                             held: ReadonlyMap<string, string>, colourBy: string,
                             tick: Pick<OverlayTick, 'scope' | 'valueName' | 'popType'>): LayerFetch[] {
  return wanted.map(({ vn, popType }) => {
    if (tick.scope === 'all' || held.get(`${vn}::${popType}`) !== colourBy) return 'full'
    if (tick.scope === 'pops' && vn === tick.valueName && popType === tick.popType) return 'pops'
    if (popType === tick.popType && vn !== tick.valueName && isClusterPopType(popType)) return 'pops'
    return 'reuse'
  })
}
