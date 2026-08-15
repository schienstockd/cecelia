/**
 * The XY extent the two flow panels render — the sizes they offer and how the answer reads back.
 *
 * Both panels show a CENTRED CROP of the frame, not the whole frame, because a whole frame costs far
 * more than a grid of ~180 px cells can show: re-measured on a 1046x1104 movie, the 16-plane sheet is
 * 12.9 MB and 3.6 s for the whole frame against 3.6 MB and 0.8 s at 512 px. (Before the window read
 * and the PNG encoder were fixed those were 36.3 MB / 8.2 s and 9.2 MB / 2.4 s, and 36 MB used to
 * arrive as `websocket closed with status 1009: message too large`, because it did not fit in one
 * websocket frame.) The full measurement lives on `FLOW_INSPECT_MAX_PX` in
 * `api/src/optical_flow_api.jl`, which is the same decision on the other side.
 *
 * Here rather than in either SFC because BOTH panels offer it and they must not drift — the same
 * reason `useFlowPlanes` exists at all. The two claim to show what a run is fed over the same window;
 * two lists of sizes would be two chances for them to be showing different amounts of it.
 */

/** Sent when the panel has no stored pick. Mirrors `FLOW_INSPECT_MAX_PX` (the server's fallback). */
export const DEFAULT_FLOW_REGION_PX = 512

/**
 * What the chips offer. The top of this list used to be bounded by the transport; since the reply
 * became a palette PNG it is bounded by TIME. Re-measured, the whole 16-plane reply runs ~13 bytes per
 * pixel on real metric planes and ~22 on synthetic noise (the pessimistic end), so 768 px is ~13 MB
 * against `WS_MAX_FRAME_SIZE` (64 MiB, app/src/utils.jl) — a bound that now only starts to bite around
 * 1770 px, asserted in flowRegion.test.ts. What keeps 768 at the top instead is that it is ~1.8 s per
 * scrub for cells drawn at ~180 px. Widening the list is a UX call; check the frame cap anyway.
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
