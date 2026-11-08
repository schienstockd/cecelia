// The crop area drawn ON THE VIEWER for `editImages.cropImage` — the record the task form and the
// viewer window pass between them, and the conversion to the task's box.
//
// The form asks (`status: 'drawing'`), the viewer draws the box with the preview box's geometry
// (`previewBox.ts`, no budget, optionally square) plus a z and t range, and answers (`'done'`); the
// form reads the answer for ITS request and clears the record. Bridged through `viewerStore.cropPick`
// (localStorage + `storage` event), the same way the preview box crosses windows.

import type { L0Rect } from '../viewerPick'

export type CropPickStatus = 'drawing' | 'done'

export interface CropPick {
  /** Which form asked — a stale answer to an older request is ignored. */
  requestId: string
  imageUid: string
  /** The version the task will crop. The box is in ITS L0 pixels; versions differ in extent. */
  valueName: string
  status: CropPickStatus
  box: L0Rect | null
  /** Inclusive plane / frame index ranges. `null` = the whole axis. */
  z: [number, number] | null
  t: [number, number] | null
  square: boolean
  /** Set by the viewer on Use: the task's box (`cropTaskBox`), sized against the stack it shows. */
  result?: Record<string, number>
}

/** The half-open full-res box `editImages.cropImage` reads: `{x0,x1,y0,y1,z0,z1,t0,t1}`, with `-1`
 *  on an axis kept whole — a range covering every plane (or frame) trims nothing, so it is sent as
 *  "keep" rather than as bounds. */
export function cropTaskBox(box: L0Rect, z: [number, number] | null, t: [number, number] | null,
                            nZ: number, nT: number): Record<string, number> {
  const out: Record<string, number> = {
    x0: Math.min(box.x0, box.x1), x1: Math.max(box.x0, box.x1),
    y0: Math.min(box.y0, box.y1), y1: Math.max(box.y0, box.y1),
    z0: -1, z1: -1, t0: -1, t1: -1,
  }
  const axis = (r: [number, number] | null, n: number): [number, number] | null => {
    if (!r || n <= 1) return null
    const lo = Math.max(0, Math.min(r[0], r[1])), hi = Math.min(n - 1, Math.max(r[0], r[1]))
    return lo === 0 && hi === n - 1 ? null : [lo, hi + 1]
  }
  const zz = axis(z, nZ), tt = axis(t, nT)
  if (zz) { out.z0 = zz[0]; out.z1 = zz[1] }
  if (tt) { out.t0 = tt[0]; out.t1 = tt[1] }
  return out
}

/** The pick a form opens the viewer with: its current value (if any) restored, so Redraw starts
 *  from the box already chosen rather than from nothing. Square (the remembered choice) opens OFF
 *  over a restored box that is not square, so the switch never claims a shape the box doesn't have. */
export function cropPickFromValue(requestId: string, imageUid: string, valueName: string,
                                  value: Record<string, number> | null | undefined,
                                  square: boolean): CropPick {
  const has = (a?: number, b?: number) => a !== undefined && b !== undefined && a >= 0 && b > a
  const v = value ?? undefined
  const box = v && has(v.x0, v.x1) && has(v.y0, v.y1) ? { x0: v.x0, x1: v.x1, y0: v.y0, y1: v.y1 } : null
  return {
    requestId, imageUid, valueName, status: 'drawing', box,
    square: square && (!box || box.x1 - box.x0 === box.y1 - box.y0),
    z: v && has(v.z0, v.z1) ? [v.z0, v.z1 - 1] : null,
    t: v && has(v.t0, v.t1) ? [v.t0, v.t1 - 1] : null,
  }
}
