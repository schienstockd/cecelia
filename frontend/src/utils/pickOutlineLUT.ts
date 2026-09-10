// The role-lookup table for the correction cockpit's viewer outline layer
// (see the correction-direct plan, PR #884 → P1). One byte per label id, indexed 0..maxId:
//
//   ROLE_OFF   = 0 — id is not in the pick set (shader draws nothing when pick-outline mode is on)
//   ROLE_PICK  = 1 — id is in `/Pick selection`               (thin outline, pick colour)
//   ROLE_FOCUS = 2 — id is the review focus (`/Review focus`) (thick outline, focus colour)
//
// Consumed by the mask shader as an r8uint 2D texture (one row, `maxId + 1` columns) — the same
// pattern the label palette texture already uses. Kept as pure logic so a Vitest run can pin its
// behaviour without a canvas or a GPU. The shader consumption + upload pipe live in P1b.
//
// Why a texture and not a uniform array: a uniform is bounded by `maxUniformBufferBindingSize`
// (16 KiB on the WebGPU baseline; 65 KiB on desktop) and label ids in a movie routinely run into
// the tens of thousands. A r8uint texture handles up to `maxTextureDimension2D` (8192 baseline,
// 16384 on desktop) ids per row, which we cap defensively — see `MAX_PICK_LUT_IDS` below.
//
// Focus wins over pick: an id that is both the review focus AND in the pick set is emitted as
// FOCUS. The alternative (pick + a separate flag) means the shader must sample twice; making the
// role total is cheaper and one lookup answers the whole question.

/** Off — id not in the pick set. Shader skips the fragment when pick-outline mode is on. */
export const ROLE_OFF = 0 as const
/** Pick — id in `/Pick selection`. Shader draws a thin outline in the pick colour. */
export const ROLE_PICK = 1 as const
/** Focus — the one review-focus id. Shader draws a thicker outline in the focus colour. */
export const ROLE_FOCUS = 2 as const

export type Role = typeof ROLE_OFF | typeof ROLE_PICK | typeof ROLE_FOCUS

/** Defensive cap on `maxId + 1`. `maxTextureDimension2D` is 8192 baseline / 16384 desktop; we
 *  cap at 65536 (2^16) because a segmentation with more than 65k distinct ids per image is
 *  outside any observed workload here, and a runaway `maxId` from a corrupted h5ad shouldn't
 *  allocate 128 MB. When the input exceeds this the builder logs and truncates — every id above
 *  the cap comes out as ROLE_OFF, which is the safe visual failure (the outline just doesn't
 *  draw), not a crash. */
export const MAX_PICK_LUT_IDS = 65536

export interface PickOutlineInputs {
  /** Ids in `/Pick selection` at the current view. Empty set is legal — everything comes out OFF. */
  pickIds: Iterable<number>
  /** The single review-focus id, or null when no cell is parked. Focus overrides pick when both
   *  name the same id. */
  focusId: number | null
  /** The largest label id the LUT must cover. Usually derived from the label store's max id, or
   *  from the largest id in `pickIds ∪ {focusId}` if that's known to be higher. */
  maxId: number
}

/** Build the r8uint bytes the shader samples. Returns a `Uint8Array` of length `min(maxId + 1,
 *  MAX_PICK_LUT_IDS)`. Byte 0 (background) is always ROLE_OFF — background is never a picked
 *  label. Ids strictly greater than the buffer's last index are silently dropped (see
 *  `MAX_PICK_LUT_IDS` above); anything else outside `[0, maxId]` is ignored — the shader path
 *  handles the out-of-range case as ROLE_OFF by construction. */
export function buildPickOutlineLUT(inputs: PickOutlineInputs): Uint8Array {
  const cappedMax = Math.min(Math.max(0, Math.floor(inputs.maxId)), MAX_PICK_LUT_IDS - 1)
  const buf = new Uint8Array(cappedMax + 1)
  for (const raw of inputs.pickIds) {
    const id = Math.floor(raw)
    if (id <= 0 || id > cappedMax) continue
    buf[id] = ROLE_PICK
  }
  if (inputs.focusId !== null && inputs.focusId !== undefined) {
    const fid = Math.floor(inputs.focusId)
    // Focus wins over pick — assigned AFTER the pick pass, so an id in both sets ends up FOCUS.
    if (fid > 0 && fid <= cappedMax) buf[fid] = ROLE_FOCUS
  }
  return buf
}

/** Whether two LUTs are byte-identical. Cheap short-circuit on length (a segmentation whose max
 *  id shrank would still register as a change). Used by the upload pipeline in P1b to skip a GPU
 *  write when the pick + focus set didn't actually change — `pickSet.size` doesn't tell us that,
 *  and rebuilding the LUT is O(pick_count) which is cheap next to `writeTexture`. */
export function pickOutlineLUTsEqual(a: Uint8Array, b: Uint8Array): boolean {
  if (a.length !== b.length) return false
  for (let i = 0; i < a.length; i++) if (a[i] !== b[i]) return false
  return true
}

/** Convenience — read one role out of a LUT with the shader's out-of-range rule (any id past the
 *  buffer reads as OFF). Only used in tests today, but named + exported so a future non-shader
 *  consumer (e.g. a canvas-fallback overlay) can share the semantic. */
export function pickOutlineRoleAt(lut: Uint8Array, id: number): Role {
  const i = Math.floor(id)
  if (i <= 0 || i >= lut.length) return ROLE_OFF
  const v = lut[i]
  return v === ROLE_FOCUS ? ROLE_FOCUS : v === ROLE_PICK ? ROLE_PICK : ROLE_OFF
}
