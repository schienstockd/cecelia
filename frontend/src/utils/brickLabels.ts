// ── Brick renderer: the mask's identity, kept apart from the image's ───────────────────────
//
// The brick atlas holds the image channels; the label atlas beside it holds the mask, slot for slot.
// A change of mask (ticked off, another segmentation, a preview re-run) invalidates only the label
// atlas — the channels stay resident. These are the pure decisions behind that split, so they can be
// tested without a device. See docs/todo/MASK_DECOUPLE_PLAN.md.

import { brickKey, type VirtualBrick } from './pageTable'
import type { MaskStore } from './viewerLabels'
import { tileMaskSourceId } from './tileViewer'

/** The fields of a `BrickSource` these helpers read (the renderer's type lives in `lib/webgpu`). */
export interface BrickSourceFields {
  projectUid: string
  imageUid: string
  valueName?: string
  labelName?: string
  labelStore?: MaskStore
  labelStoreId?: number
  liveSince?: string
  rev?: string
}

/** Same image bytes — every resident intensity brick is still valid. The mask fields are left out on
 *  purpose: a mask change is `brickLabelSourceId`'s question, not this one. */
export function sameBrickImage(a: BrickSourceFields | null, b: BrickSourceFields | null): boolean {
  return a !== null && b !== null
    && a.projectUid === b.projectUid
    && a.imageUid === b.imageUid
    && a.valueName === b.valueName
    && (a.rev ?? '') === (b.rev ?? '')
}

/** Which mask the label atlas should hold — `''` for none. The same id every other viewer path stamps
 *  its masks with (`tileMaskSourceId`), read off a `BrickSource`: a different id means every landed
 *  label brick is stale. */
export function brickLabelSourceId(s: BrickSourceFields | null): string {
  return tileMaskSourceId({
    vn: s?.labelName ?? '', hidden: false, store: s?.labelStore, storeId: s?.labelStoreId,
    liveSince: s?.liveSince, rev: s?.rev,
  })
}

/**
 * Resident bricks whose mask has not landed under the current source, in the order to fetch them:
 * the timepoint on screen first, then outward by timepoint distance. Only `level` — a brick at another
 * level is not drawn, and gets its mask when it is fetched again.
 *
 * `labelOf[slot]` is the brick key whose mask the slot's label bytes hold; `skip` filters out keys
 * already in flight or already failed for this source. At most `limit` are returned.
 */
export function labelBackfill(
  entries: readonly { brick: VirtualBrick; slot: number }[],
  labelOf: readonly (string | undefined)[],
  displayT: number, level: number | undefined,
  skip: (key: string) => boolean, limit: number,
): { brick: VirtualBrick; slot: number }[] {
  if (limit <= 0 || level === undefined) return []
  const want: { brick: VirtualBrick; slot: number; key: string }[] = []
  for (const e of entries) {
    if (e.brick.level !== level) continue
    const key = brickKey(e.brick)
    if (labelOf[e.slot] === key || skip(key)) continue
    want.push({ brick: e.brick, slot: e.slot, key })
  }
  want.sort((a, b) => Math.abs(a.brick.t - displayT) - Math.abs(b.brick.t - displayT))
  return want.slice(0, limit).map(({ brick, slot }) => ({ brick, slot }))
}
