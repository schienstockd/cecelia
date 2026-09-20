import { describe, it, expect } from 'vitest'
import { formatAddress, formatWhen, type CaptureRow } from './kiwiCaptures'

const R = (over: Partial<CaptureRow>): CaptureRow => ({
  captureId: 'cap-x',
  createdAt: '',
  surface: 'viewer_frame',
  address: null,
  ...over,
})

describe('formatAddress', () => {
  it('viewer frame with image + t + z', () => {
    expect(formatAddress(R({ surface: 'viewer_frame',
      address: { imageUid: '1SqevM', t: 3, z: 7 } }))).toBe('viewer · image 1SqevM, t=3, z=7')
  })
  it('viewer slab renders t as an en-dashed range', () => {
    expect(formatAddress(R({ surface: 'viewer_slab',
      address: { imageUid: 'ABC', t: [2, 5] } }))).toBe('slab · image ABC, t=2–5')
  })
  it('plot cites the specId', () => {
    expect(formatAddress(R({ surface: 'plot',
      address: { plotSpec: { specId: 'dotplot' } } }))).toBe('plot · dotplot')
  })
  it('multi-panel plot reads count + module (specId irrelevant)', () => {
    expect(formatAddress(R({ surface: 'plot', panelCount: 3,
      address: { plotSpec: { specId: 'multi-panel', params: { module: 'behaviourAnalysis' } } } })))
      .toBe('plot · 3 panels · behaviourAnalysis')
  })
  it('multi-panel plot without a module param drops the trailing bit', () => {
    expect(formatAddress(R({ surface: 'plot', panelCount: 2,
      address: { plotSpec: { specId: 'multi-panel' } } }))).toBe('plot · 2 panels')
  })
  it('single-plot capture with panelCount=1 keeps specId label', () => {
    expect(formatAddress(R({ surface: 'plot', panelCount: 1,
      address: { plotSpec: { specId: 'dotplot' } } }))).toBe('plot · dotplot')
  })
  it('ui cites the anchor', () => {
    expect(formatAddress(R({ surface: 'ui',
      address: { domAnchor: 'viewer.share' } }))).toBe('ui · viewer.share')
  })
  it('degrades to the surface word when there is no address', () => {
    expect(formatAddress(R({ surface: 'viewer_frame', address: null }))).toBe('viewer')
    expect(formatAddress(R({ surface: 'ui', address: null }))).toBe('ui')
    expect(formatAddress(R({ surface: 'plot', address: null }))).toBe('plot')
  })
})

describe('formatWhen', () => {
  const now = new Date('2026-09-19T12:00:00Z')
  it('recent → "just now"', () => {
    expect(formatWhen('2026-09-19T11:59:30Z', now)).toBe('just now')
  })
  it('minutes', () => {
    expect(formatWhen('2026-09-19T11:55:00Z', now)).toBe('5m')
  })
  it('hours', () => {
    expect(formatWhen('2026-09-19T10:00:00Z', now)).toBe('2h')
  })
  it('days', () => {
    expect(formatWhen('2026-09-16T12:00:00Z', now)).toBe('3d')
  })
  it('empty on unparseable / future / missing', () => {
    expect(formatWhen('', now)).toBe('')
    expect(formatWhen('not a date', now)).toBe('')
    expect(formatWhen('2026-09-19T13:00:00Z', now)).toBe('') // future ⇒ empty (clock skew, not "in the future")
  })
})
