import { describe, it, expect } from 'vitest'
import { hslCssToRgb, labelPaletteBytes, widenLabelSlab, labelBpv, LABEL_PALETTE_N } from './viewerLabels'
import { slabUrl, slabShapeError, type ViewerMeta } from './volumeViewer'
import { distinctColors } from '../plots/plot'

describe('hslCssToRgb', () => {
  it('matches the CSS Color 4 reference at the primaries', () => {
    expect(hslCssToRgb('hsl(0 100% 50%)')).toEqual([255, 0, 0])
    expect(hslCssToRgb('hsl(120 100% 50%)')).toEqual([0, 255, 0])
    expect(hslCssToRgb('hsl(240 100% 50%)')).toEqual([0, 0, 255])
    expect(hslCssToRgb('hsl(0 0% 50%)')).toEqual([128, 128, 128])
  })
  it('reads every colour distinctColors emits', () => {
    for (const css of distinctColors(LABEL_PALETTE_N)) expect(() => hslCssToRgb(css)).not.toThrow()
  })
  // Black would render as "this cell has no mask" — a wrong answer that looks like a real one.
  it('throws rather than returning black on anything else', () => {
    expect(() => hslCssToRgb('#ff0000')).toThrow()
    expect(() => hslCssToRgb('rgb(1,2,3)')).toThrow()
  })
})

describe('labelPaletteBytes', () => {
  it('is one opaque rgba row', () => {
    const b = labelPaletteBytes()
    expect(b.length).toBe(LABEL_PALETTE_N * 4)
    for (let i = 3; i < b.length; i += 4) expect(b[i]).toBe(255)
  })
  // The property the whole palette is chosen for: segmentation gives touching cells near-consecutive
  // ids, so consecutive ROWS have to be far apart in colour, not merely different.
  it('puts consecutive rows far apart in colour', () => {
    const b = labelPaletteBytes()
    for (let i = 0; i < 8; i++) {
      const d = Math.abs(b[i * 4] - b[i * 4 + 4]) + Math.abs(b[i * 4 + 1] - b[i * 4 + 5]) +
                Math.abs(b[i * 4 + 2] - b[i * 4 + 6])
      expect(d).toBeGreaterThan(80)
    }
  })
})

describe('widenLabelSlab', () => {
  it('hands a UInt32 slab straight through', () => {
    const buf = Uint32Array.from([1, 2, 4_000_000_000]).buffer
    expect(widenLabelSlab(buf, 4)).toBe(buf)
  })
  it('widens a narrower store rather than rendering it at half width', () => {
    const src = Uint16Array.from([0, 1, 65535]).buffer
    expect([...new Uint32Array(widenLabelSlab(src, 2))]).toEqual([0, 1, 65535])
    expect([...new Uint32Array(widenLabelSlab(Uint8Array.from([0, 7]).buffer, 1))]).toEqual([0, 7])
  })
  it('refuses a width that is not a label width', () => {
    expect(() => widenLabelSlab(new ArrayBuffer(8), 8)).toThrow(/bytes per voxel/)
  })
})

describe('labelBpv', () => {
  it('reads the header, and falls back to what every real store is', () => {
    expect(labelBpv('4')).toBe(4)
    expect(labelBpv('2')).toBe(2)
    expect(labelBpv(null)).toBe(4)
    expect(labelBpv('nonsense')).toBe(4)
  })
})

const META = { nX: 4, nY: 3, nZ: 2, nT: 5, nC: 2, bytesPerVoxel: 2,
               voxelUm: [1, 1, 1], channels: [] } as unknown as ViewerMeta

describe('the mask rides the image route', () => {
  it('adds `labels` and keeps the plane selection', () => {
    const u = slabUrl({ projectUid: 'p', imageUid: 'i', t: 3, c: 0, z: 5, zTo: 8, labels: 'base' })
    expect(u).toContain('labels=base')
    expect(u).toContain('z=5')
    expect(u).toContain('zTo=8')
  })
  it('omits it entirely for an image slab', () => {
    expect(slabUrl({ projectUid: 'p', imageUid: 'i', t: 0, c: 0 })).not.toContain('labels')
  })
  // Same geometry, different dtype — so the shape half of the guard is the same question and the
  // length half is not. Without the override every mask would read as "truncated".
  it('guards the mask at ITS bytes per voxel', () => {
    const bytes = 4 * 3 * 2 * 4
    expect(slabShapeError('2,3,4', bytes, META, 2, 4)).toBeNull()
    expect(slabShapeError('2,3,4', bytes, META, 2)).toMatch(/bytes, expected/)
    expect(slabShapeError('1,3,4', bytes, META, 2, 4)).toMatch(/but 2x3x4 was asked for/)
  })
})
