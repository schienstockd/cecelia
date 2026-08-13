/**
 * The XY extent the two flow panels render — the sizes they offer and how the answer reads back.
 *
 * Both panels show a CENTRED CROP of the frame, not the whole frame, because a whole frame costs far
 * more than a grid of ~180 px cells can show: measured on a 1044x1102 movie, the 16-plane sheet is
 * 36.3 MB and 8.2 s for the whole frame against 9.2 MB and 2.4 s at 512 px — and 36 MB used to arrive
 * as `websocket closed with status 1009: message too large`, because it does not fit in one websocket
 * frame. The full measurement lives on `FLOW_INSPECT_MAX_PX` in `api/src/optical_flow_api.jl`, which is
 * the same decision on the other side.
 *
 * Here rather than in either SFC because BOTH panels offer it and they must not drift — the same
 * reason `useFlowPlanes` exists at all. The two claim to show what a run is fed over the same window;
 * two lists of sizes would be two chances for them to be showing different amounts of it.
 */

/** Sent when the panel has no stored pick. Mirrors `FLOW_INSPECT_MAX_PX` (the server's fallback). */
export const DEFAULT_FLOW_REGION_PX = 512

/**
 * What the chips offer. The top of this list is bounded by the transport, not by taste. Measured, the
 * whole 16-plane reply runs ~35 bytes per pixel on real metric planes and ~67 on synthetic noise (the
 * pessimistic end); at 768 px that is at most ~40 MB, inside `WS_MAX_FRAME_SIZE` (64 MiB,
 * app/src/utils.jl) with room to spare, and at 1024 px it is ~70 MB, past it. Widening this list means
 * raising that number too — asserted in flowRegion.test.ts.
 */
export const FLOW_REGION_OPTIONS = [256, 512, 768]

/** The reply's `region` — half-open `[lo, hi)` per axis, as the preview worker echoes it back. */
export type FlowRegion = Record<string, number[]> | null | undefined

/**
 * `"512 × 512"` for the crop actually rendered, or `''` when there is nothing to report.
 *
 * Read back from the REPLY rather than from the chip, because the two legitimately differ: the crop is
 * capped at the axis length, so a small image shows its whole frame however wide a size is asked for.
 * A chip reading 512 over a 418 px image would be a claim the picture does not support.
 */
export function flowRegionLabel(region: FlowRegion): string {
  const span = (ax: string) => {
    const v = region?.[ax]
    return Array.isArray(v) && v.length === 2 ? Math.max(0, Math.round(v[1] - v[0])) : 0
  }
  const w = span('X')
  const h = span('Y')
  return w > 0 && h > 0 ? `${w} × ${h}` : ''
}
