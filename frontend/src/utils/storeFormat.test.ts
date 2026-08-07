import { describe, it, expect } from 'vitest'
import { formatShape, storeFormatLine } from './storeFormat'

describe('formatShape', () => {
  it('joins the FULL shape, never abbreviating to the XY pair', () => {
    // dropping leading 1s would make a shard that legitimately spans z indistinguishable from one
    // that does not — the whole reason the shape is shown at all
    expect(formatShape([1, 1, 1, 512, 512])).toBe('1×1×1×512×512')
    expect(formatShape([1, 1, 4, 512, 512])).toBe('1×1×4×512×512')
  })

  it('is an em dash when there is no shape to show', () => {
    expect(formatShape(null)).toBe('—')
    expect(formatShape(undefined)).toBe('—')
    expect(formatShape([])).toBe('—')
  })
})

describe('storeFormatLine', () => {
  it('describes a v2 store as not sharded', () => {
    expect(storeFormatLine({ zarrFormat: 2, ngffVersion: '0.4', chunks: [1, 1, 1, 512, 512] }))
      .toBe('zarr v2 · NGFF 0.4 · chunks 1×1×1×512×512 · not sharded')
  })

  it('reports chunk AND shard for a sharded v3 store', () => {
    // the two are easy to swap; the readout has to state both so a wrong one is visible
    expect(storeFormatLine({
      zarrFormat: 3, ngffVersion: '0.5',
      chunks: [1, 1, 1, 32, 32], shard: [1, 1, 1, 64, 64],
    })).toBe('zarr v3 · NGFF 0.5 · chunks 1×1×1×32×32 · shard 1×1×1×64×64')
  })

  it('says "not sharded" explicitly rather than omitting it', () => {
    // omitting would make "unsharded" and "we could not read the shape" look the same
    expect(storeFormatLine({ zarrFormat: 2, chunks: [1, 512, 512] })).toContain('not sharded')
    expect(storeFormatLine({ zarrFormat: 3, chunks: [1, 512, 512], shard: [] })).toContain('not sharded')
    expect(storeFormatLine({ zarrFormat: 3, chunks: [1, 512, 512], shard: null })).toContain('not sharded')
  })

  it('is empty when nothing is known, so the caller can skip the line', () => {
    expect(storeFormatLine(null)).toBe('')
    expect(storeFormatLine(undefined)).toBe('')
    expect(storeFormatLine({})).toBe('')
    // a store row that only carries a size (missing/unreadable store) contributes no format line
    expect(storeFormatLine({ chunks: null, shard: null })).toBe('')
  })

  it('omits parts it does not have rather than printing placeholders', () => {
    expect(storeFormatLine({ zarrFormat: 2 })).toBe('zarr v2 · not sharded')
    expect(storeFormatLine({ ngffVersion: '0.5' })).toBe('NGFF 0.5 · not sharded')
  })
})
