import { describe, it, expect } from 'vitest'
import { sameBrickImage, brickLabelSourceId, labelBackfill } from './brickLabels'
import { brickKey, type VirtualBrick } from './pageTable'

const img = { projectUid: 'p', imageUid: 'i', valueName: 'default' }

describe('sameBrickImage', () => {
  it('ignores every mask field — a mask change keeps the intensity bricks', () => {
    expect(sameBrickImage(img, { ...img, labelName: 'cp', labelStore: 'live', labelStoreId: 3 })).toBe(true)
  })
  it('is false across image, version or rewrite stamp', () => {
    expect(sameBrickImage(img, { ...img, imageUid: 'j' })).toBe(false)
    expect(sameBrickImage(img, { ...img, valueName: 'corrected' })).toBe(false)
    expect(sameBrickImage(img, { ...img, rev: '2' })).toBe(false)
  })
  it('treats a missing rev as no rev', () => {
    expect(sameBrickImage({ ...img, rev: '' }, img)).toBe(true)
  })
  it('is false when either side is null', () => {
    expect(sameBrickImage(null, img)).toBe(false)
    expect(sameBrickImage(img, null)).toBe(false)
  })
})

describe('brickLabelSourceId', () => {
  it('is empty with no mask', () => {
    expect(brickLabelSourceId(null)).toBe('')
    expect(brickLabelSourceId(img)).toBe('')
  })
  it('changes with the mask, its store and the store stamp — not with the image', () => {
    const a = brickLabelSourceId({ ...img, labelName: 'cp' })
    expect(brickLabelSourceId({ ...img, labelName: 'nuc' })).not.toBe(a)
    expect(brickLabelSourceId({ ...img, labelName: 'cp', labelStore: 'preview', labelStoreId: 1 })).not.toBe(a)
    expect(brickLabelSourceId({ ...img, labelName: 'cp', labelStore: 'live', labelStoreId: 1 }))
      .not.toBe(brickLabelSourceId({ ...img, labelName: 'cp', labelStore: 'live', labelStoreId: 2 }))
    expect(brickLabelSourceId({ ...img, imageUid: 'j', labelName: 'cp' })).toBe(a)
  })
})

describe('labelBackfill', () => {
  const b = (t: number, level = 0, bx = 0): VirtualBrick => ({ t, level, bx, by: 0, bz: 0 })
  const entries = [b(5), b(0), b(3), b(4, 1), b(2, 0, 1)].map((brick, slot) => ({ brick, slot }))
  const none = () => false

  it('orders by distance from the timepoint on screen, current level only', () => {
    const got = labelBackfill(entries, [], 3, 0, none, 10).map(e => e.brick.t)
    expect(got).toEqual([3, 2, 5, 0])
  })
  it('skips slots whose mask already landed for their brick', () => {
    const labelOf: (string | undefined)[] = []
    labelOf[2] = brickKey(b(3))            // slot 2 holds b(3)'s mask
    labelOf[0] = brickKey(b(9))            // slot 0's mask is a previous occupant's
    const got = labelBackfill(entries, labelOf, 3, 0, none, 10).map(e => e.slot)
    expect(got).toEqual([4, 0, 1])
  })
  it('skips in-flight / failed keys and caps at limit', () => {
    const skip = (k: string) => k === brickKey(b(3))
    expect(labelBackfill(entries, [], 3, 0, skip, 2).map(e => e.brick.t)).toEqual([2, 5])
    expect(labelBackfill(entries, [], 3, 0, none, 0)).toEqual([])
  })
  it('does nothing before the first level is picked', () => {
    expect(labelBackfill(entries, [], 3, undefined, none, 10)).toEqual([])
  })
})
