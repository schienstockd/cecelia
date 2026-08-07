import { describe, it, expect } from 'vitest'
import { formatShape, storeFormatFacts, storeFormatTitle } from './storeFormat'

/** `[{k, v}]` → `{k: v}`, so an expectation reads as the facts and not as array indices. */
const asMap = (s: Parameters<typeof storeFormatFacts>[0]) =>
  Object.fromEntries(storeFormatFacts(s).map(f => [f.k, f.v]))

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

describe('storeFormatTitle', () => {
  it('brackets the NGFF version after the zarr format', () => {
    expect(storeFormatTitle({ zarrFormat: 2, ngffVersion: '0.4' })).toBe('zarr v2 (NGFF 0.4)')
    expect(storeFormatTitle({ zarrFormat: 3, ngffVersion: '0.5' })).toBe('zarr v3 (NGFF 0.5)')
  })

  it('drops the bracket when the NGFF version is absent, and never infers it', () => {
    // not hypothetical: stores our own v2 writers produced before `write_multiscales_attrs` carry no
    // version at all. Inferring 0.4 from "zarr v2" would state as read something never written.
    expect(storeFormatTitle({ zarrFormat: 2 })).toBe('zarr v2')
    expect(storeFormatTitle({ zarrFormat: 2, ngffVersion: null })).toBe('zarr v2')
  })

  it('is empty when the store could not be read, so the caller says so once', () => {
    expect(storeFormatTitle(null)).toBe('')
    expect(storeFormatTitle(undefined)).toBe('')
    expect(storeFormatTitle({})).toBe('')
  })
})

describe('storeFormatFacts', () => {
  it('leaves format and NGFF to the title — they are not repeated here', () => {
    // the whole point of the split: two crowded rows became one line plus a title
    expect(asMap({ zarrFormat: 2, ngffVersion: '0.4', chunks: [1, 1, 1, 512, 512] }))
      .toEqual({ chunks: '1×1×1×512×512', shard: 'none' })
  })

  it('LABELS every value rather than joining them into a sentence', () => {
    const facts = storeFormatFacts({
      zarrFormat: 3, label: 'zstd + shuffle', chunks: [1, 1, 1, 32, 32],
      shard: [1, 1, 1, 64, 64], separator: '/',
    })
    expect(facts.map(f => f.k)).toEqual(['codec', 'chunks', 'shard', 'keys'])
    for (const f of facts) {
      expect(f.k).not.toBe('')
      expect(f.v).not.toBe('')
    }
  })

  it('shows the codec first, as the API described it', () => {
    // blosc's shuffle is spelled differently per bioformats2raw version; that normalisation lives in
    // the API's `_describe_compressor`, and re-deriving a label here would be the second copy
    expect(storeFormatFacts({ zarrFormat: 2, label: 'zstd + shuffle' })[0])
      .toEqual({ k: 'codec', v: 'zstd + shuffle' })
    expect(asMap({ zarrFormat: 2 })).not.toHaveProperty('codec')
  })

  it('names the chunk-key separator rather than printing a bare slash', () => {
    // it decides how many DIRECTORIES the store costs (20,933 nested vs 4 flat on one 1.7 GB import),
    // and a bare `/` in a readout reads as a path fragment
    expect(asMap({ zarrFormat: 2, chunks: [1, 512, 512], separator: '/' }).keys).toBe('nested')
    expect(asMap({ zarrFormat: 2, chunks: [1, 512, 512], separator: '.' }).keys).toBe('flat')
    // absent -> omitted, not guessed: the DEFAULT differs per format ('.' for v2, '/' for v3)
    expect(asMap({ zarrFormat: 2, chunks: [1, 512, 512] })).not.toHaveProperty('keys')
  })

  it('reports chunk AND shard for a sharded v3 store', () => {
    // the two are easy to swap; the readout has to state both so a wrong one is visible
    expect(asMap({ zarrFormat: 3, chunks: [1, 1, 1, 32, 32], shard: [1, 1, 1, 64, 64] }))
      .toEqual({ chunks: '1×1×1×32×32', shard: '1×1×1×64×64' })
  })

  it('says shard "none" explicitly rather than omitting the fact', () => {
    // omitting would make "unsharded" and "we could not read the shape" look the same
    expect(asMap({ zarrFormat: 2, chunks: [1, 512, 512] }).shard).toBe('none')
    expect(asMap({ zarrFormat: 3, chunks: [1, 512, 512], shard: [] }).shard).toBe('none')
    expect(asMap({ zarrFormat: 3, chunks: [1, 512, 512], shard: null }).shard).toBe('none')
  })

  it('is empty when nothing is known, so the caller can skip the row', () => {
    expect(storeFormatFacts(null)).toEqual([])
    expect(storeFormatFacts(undefined)).toEqual([])
    expect(storeFormatFacts({})).toEqual([])
    // a store row that only carries a size (missing/unreadable store) contributes no facts
    expect(storeFormatFacts({ chunks: null, shard: null })).toEqual([])
  })
})
