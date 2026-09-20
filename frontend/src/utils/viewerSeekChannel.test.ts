import { describe, it, expect } from 'vitest'
import { parseSeekMessage } from './viewerSeekChannel'

describe('parseSeekMessage', () => {
  it('accepts a well-formed message with t + z', () => {
    expect(parseSeekMessage({ projectUid: 'PROJ', imageUid: 'IMG', t: 3, z: 7 }))
      .toEqual({ projectUid: 'PROJ', imageUid: 'IMG', t: 3, z: 7 })
  })
  it('accepts a message with only t (no z)', () => {
    expect(parseSeekMessage({ projectUid: 'P', imageUid: 'I', t: 0 }))
      .toEqual({ projectUid: 'P', imageUid: 'I', t: 0 })
  })
  it('floors non-integer t / z (defensive — a caller passing a float shouldn\'t change the axis to a fractional index)', () => {
    expect(parseSeekMessage({ projectUid: 'P', imageUid: 'I', t: 3.9, z: 7.1 }))
      .toEqual({ projectUid: 'P', imageUid: 'I', t: 3, z: 7 })
  })
  it('drops a negative t / z rather than pinning to 0 (a bogus value shouldn\'t silently move the viewer)', () => {
    const parsed = parseSeekMessage({ projectUid: 'P', imageUid: 'I', t: -1, z: -3 })
    expect(parsed).toEqual({ projectUid: 'P', imageUid: 'I' })
  })
  it('drops a NaN / Infinity t or z', () => {
    expect(parseSeekMessage({ projectUid: 'P', imageUid: 'I', t: NaN }))
      .toEqual({ projectUid: 'P', imageUid: 'I' })
    expect(parseSeekMessage({ projectUid: 'P', imageUid: 'I', z: Infinity }))
      .toEqual({ projectUid: 'P', imageUid: 'I' })
  })
  it('returns null when projectUid or imageUid is missing (a mis-typed row must not move a viewer)', () => {
    expect(parseSeekMessage({ imageUid: 'I' })).toBeNull()
    expect(parseSeekMessage({ projectUid: 'P' })).toBeNull()
    expect(parseSeekMessage({})).toBeNull()
  })
  it('returns null on garbage input (a channel-name collision with another app must not crash the viewer)', () => {
    expect(parseSeekMessage(null)).toBeNull()
    expect(parseSeekMessage('seek me')).toBeNull()
    expect(parseSeekMessage(42)).toBeNull()
  })
  it('carries an optional captureId + marks payload (blackboard attachment restore)', () => {
    const marks = [{ kind: 'rect', geom: { x: 0.1, y: 0.1, w: 0.2, h: 0.2 }, color: 'magenta' }]
    expect(parseSeekMessage({ projectUid: 'P', imageUid: 'I', t: 3, z: 7,
                              captureId: 'cap-abc', marks }))
      .toEqual({ projectUid: 'P', imageUid: 'I', t: 3, z: 7, captureId: 'cap-abc', marks })
  })
  it('drops a non-array `marks` (defensive: an unrelated postMessage claiming marks:{} must not crash the overlay)', () => {
    const parsed = parseSeekMessage({ projectUid: 'P', imageUid: 'I', marks: { not: 'array' } })
    expect(parsed).toEqual({ projectUid: 'P', imageUid: 'I' })
  })
  it('drops an empty-string captureId (nothing to label the chip with)', () => {
    const parsed = parseSeekMessage({ projectUid: 'P', imageUid: 'I', captureId: '' })
    expect(parsed).toEqual({ projectUid: 'P', imageUid: 'I' })
  })
})
