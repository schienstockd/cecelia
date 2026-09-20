// Rebuild a canvas's `panels[]` from a capture envelope's `panels[]`.
//
// A plot capture's `panels[]` carries everything needed to reproduce what the user was looking
// at when they shared: each entry has a `plotRef {specId, ui}`, a `dataSlice {series, ...}` and
// a `position` in the composite's frame. That's the same shape `useCanvasPanelsStore` persists as
// its own `CanvasEntry.panels[]` and `geom[]`, so restoring is a straight translation — no fresh
// server calls, no re-derivation.
//
// The `position` in the envelope is **composite-relative** (top-left of the composite = 0,0). The
// canvas store's geoms are in the WORKSPACE frame (top-left of the canvas = 0,0). For the first
// slice we treat these as equivalent — the composite bounding box was originally lifted from the
// workspace positions minus their min-corner, so restoring at composite-relative positions gives
// a layout that's PLACED at the workspace's top-left rather than at the original workspace spot.
// The user gets their panels back in a tight cluster; they can drag them into place. A stricter
// "restore to the exact workspace pixels" would require snapshotting the workspace origin too,
// which the envelope doesn't currently carry — an additive follow-up if the feel is off.

import type { PanelGeom } from '../stores/canvasPanels'
import type { CanvasItem } from '../composables/useCanvasPanels'

/** One panel entry from a capture envelope. Loose typing on purpose — the envelope is authored
 *  per-module (behaviourAnalysis today) and we don't want a strict schema fight. */
export interface CapturedPanel {
  panelId: string
  position: { x: number; y: number; w: number; h: number }
  plotRef?: { specId?: string; ui?: Record<string, unknown> }
  dataSlice?: { series?: unknown[]; imageUids?: unknown; setUid?: unknown; scope?: unknown }
}

/** Restore a canvas's persistence entry from a capture's `panels[]`.
 *  - `store` is the `useCanvasPanelsStore` instance (has `ensure` + `setGeom`).
 *  - `key` is the canvas key (`summary:behaviourAnalysis:none`) — the entry gets overwritten.
 *  - `panels` is the envelope's own panels array.
 *  Overwrites in place: no attempt to merge with a live layout, because the intent of "Zoom to
 *  source" is precisely "show me THIS shared configuration." */
export function restorePanelsFromCapture<S>(
  store: {
    ensure: (k: string) => { panels: CanvasItem<unknown>[]; activeId: number; nextId: number
                             arrangeSeq: number; shared: Record<string, unknown> }
    setGeom: (k: string, g: PanelGeom) => void
    delGeom: (k: string) => void
  },
  key: string,
  panels: CapturedPanel[],
  buildState: (p: CapturedPanel) => S,
): number {
  const entry = store.ensure(key)
  // Drop any existing geoms under this key — a stale one would drag the restored panel back to
  // wherever it used to sit (the store's `getGeom` win over the mount-time position).
  const currentIds = entry.panels.map(p => p.id)
  for (const id of currentIds) store.delGeom(`${key}:${id}`)
  const rebuilt: CanvasItem<unknown>[] = []
  let nextId = 0
  for (const cp of panels) {
    const id = ++nextId
    rebuilt.push({ id, arrange: null, state: buildState(cp) as unknown })
    store.setGeom(`${key}:${id}`, {
      x: Math.max(0, cp.position.x),
      y: Math.max(0, cp.position.y),
      w: Math.max(80, cp.position.w),
      h: Math.max(80, cp.position.h),
    })
  }
  entry.panels = rebuilt
  entry.nextId = nextId
  entry.activeId = rebuilt.at(-1)?.id ?? 0
  entry.arrangeSeq = 0
  return rebuilt.length
}
