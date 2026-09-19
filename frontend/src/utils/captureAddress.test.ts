import { describe, expect, it } from 'vitest'
import {
  buildCaptureAddress, normalisePoint, rectToOverlayGeom, pointsToOverlayGeom,
} from './captureAddress'

describe('buildCaptureAddress', () => {
  it('drops empty/nullish fields — projectUid always survives', () => {
    expect(buildCaptureAddress({ projectUid: 'p1' })).toEqual({ projectUid: 'p1' })
  })
  it('keeps a scalar t and a slab-range t', () => {
    expect(buildCaptureAddress({ projectUid: 'p', t: 3 }).t).toBe(3)
    expect(buildCaptureAddress({ projectUid: 'p', t: [3, 7] }).t).toEqual([3, 7])
  })
  it('drops an extentUm with a non-positive axis (an unknown physical size)', () => {
    expect(buildCaptureAddress({ projectUid: 'p', extentUm: { x: 0, y: 42 } }).extentUm).toBeUndefined()
    expect(buildCaptureAddress({ projectUid: 'p', extentUm: { x: 10, y: 20, unit: 'µm' } }).extentUm)
      .toEqual({ x: 10, y: 20, unit: 'µm' })
  })
  it('drops empty imageUid / valueName / domAnchor', () => {
    const a = buildCaptureAddress({ projectUid: 'p', imageUid: '', valueName: '', domAnchor: '' })
    expect(a.imageUid).toBeUndefined()
    expect(a.valueName).toBeUndefined()
    expect(a.domAnchor).toBeUndefined()
  })
  it('drops a plotSpec missing its specId (an unauthored plot address is not a valid one)', () => {
    expect(buildCaptureAddress({ projectUid: 'p', plotSpec: { specId: '' } }).plotSpec).toBeUndefined()
    expect(buildCaptureAddress({ projectUid: 'p', plotSpec: { specId: 'motility.speed' } }).plotSpec)
      .toEqual({ specId: 'motility.speed' })
  })
})

describe('normalisePoint', () => {
  it('scales into [0,1] against the frame extent', () => {
    expect(normalisePoint([100, 50], 200, 100)).toEqual([0.5, 0.5])
    expect(normalisePoint([0, 0], 200, 100)).toEqual([0, 0])
    expect(normalisePoint([200, 100], 200, 100)).toEqual([1, 1])
  })
  it('clamps a point drawn past the frame edge (pointer overshoot on release)', () => {
    expect(normalisePoint([250, -10], 200, 100)).toEqual([1, 0])
  })
  it('returns 0 on a zero-sized frame instead of NaN — a dead frame is not a coord', () => {
    expect(normalisePoint([50, 25], 0, 100)).toEqual([0, 0.25])
    expect(normalisePoint([50, 25], 100, 0)).toEqual([0.5, 0])
  })
})

describe('rectToOverlayGeom', () => {
  it('emits {x, y, w, h} in frame-relative coords', () => {
    const r = rectToOverlayGeom({ xMin: 40, yMin: 20, xMax: 80, yMax: 60 }, 200, 100)
    expect(r.x).toBeCloseTo(0.2); expect(r.y).toBeCloseTo(0.2)
    expect(r.w).toBeCloseTo(0.2); expect(r.h).toBeCloseTo(0.4)
  })
})

describe('pointsToOverlayGeom', () => {
  it('emits {pts:[[x,y], …]} in frame-relative coords', () => {
    expect(pointsToOverlayGeom([[0, 0], [100, 100]], 200, 200)).toEqual({ pts: [[0, 0], [0.5, 0.5]] })
  })
  it('preserves order — a polyline is a sequence, not a set', () => {
    const out = pointsToOverlayGeom([[0, 0], [100, 0], [100, 100]], 200, 200)
    expect(out.pts).toEqual([[0, 0], [0.5, 0], [0.5, 0.5]])
  })
})
