import { afterEach, describe, expect, it, vi } from 'vitest'
import { allShown, loadThumb, parseShown, PREVIEW_PX, previewCaption, previewHtml, thumbCacheSize, serializeShown, thumbCacheKey, thumbsStorageKey, toggleAllShown, toggleShown } from './imageThumbs'

describe('image table thumbnails — which rows show one', () => {
  it('round-trips through storage, and reads junk as nothing shown', () => {
    const s = new Set(['b', 'a'])
    expect(parseShown(serializeShown(s))).toEqual(s)
    expect(serializeShown(s)).toBe('["a","b"]')
    expect(parseShown(null).size).toBe(0)
    expect(parseShown('not json').size).toBe(0)
    expect(parseShown('{"a":1}').size).toBe(0)
    expect([...parseShown('["a", 3, null]')]).toEqual(['a'])
  })

  it('flips one row without touching the others', () => {
    const s = new Set(['a'])
    const on = toggleShown(s, 'b')
    expect([...on].sort()).toEqual(['a', 'b'])
    expect(s.has('b')).toBe(false)          // a new set — the ref re-renders
    expect([...toggleShown(on, 'a')]).toEqual(['b'])
  })

  it('the header button shows all unless all are shown, and leaves other sets alone', () => {
    const other = 'x-other-set'
    const some = new Set(['a', other])
    const all = toggleAllShown(some, ['a', 'b', 'c'])
    expect(allShown(all, ['a', 'b', 'c'])).toBe(true)
    expect(all.has(other)).toBe(true)
    const none = toggleAllShown(all, ['a', 'b', 'c'])
    expect([...none]).toEqual([other])
    expect(allShown(new Set(), [])).toBe(false)
    expect(toggleAllShown(new Set(), []).size).toBe(0)
  })

  it('keys storage per project and the cache per version', () => {
    expect(thumbsStorageKey('P1')).not.toBe(thumbsStorageKey('P2'))
    expect(thumbCacheKey('P', 'i', 'default')).not.toBe(thumbCacheKey('P', 'i', 'corrected'))
    expect(thumbCacheKey('P', 'i', 'default', 0)).not.toBe(thumbCacheKey('P', 'i', 'default', 1))
  })
})

describe('image table thumbnails — the fetch cache', () => {
  afterEach(() => vi.unstubAllGlobals())

  it('re-fetches when the rev moves (a store rewritten in place), and keeps one entry per image', async () => {
    let n = 0
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({ png: `data:${++n}` }))))
    const first = await loadThumb('Pc', 'img', 'default', 0)
    expect(await loadThumb('Pc', 'img', 'default', 0)).toBe(first)     // same rev → cached
    const after = await loadThumb('Pc', 'img', 'default', 1)            // a task finished on it
    expect(after).not.toBe(first)
    expect(fetch).toHaveBeenCalledTimes(2)
    const before = thumbCacheSize()
    await loadThumb('Pc', 'img', 'default', 2)
    expect(thumbCacheSize()).toBe(before)                                // the older one was dropped
  })

  it('the hover preview is its own entry: fetching it neither evicts nor reuses the row thumbnail', async () => {
    vi.stubGlobal('fetch', vi.fn(async (u: string) =>
      new Response(JSON.stringify({ png: `data:${new URL(u, 'http://x').searchParams.get('size')}` }))))
    const row = await loadThumb('Ps', 'img', 'default', 0)
    const big = await loadThumb('Ps', 'img', 'default', 0, PREVIEW_PX)
    expect(big).toBe(`data:${PREVIEW_PX}`)
    expect(row).not.toBe(big)
    expect(await loadThumb('Ps', 'img', 'default', 0)).toBe(row)
    expect(fetch).toHaveBeenCalledTimes(2)
  })
})

describe('image table thumbnails — the hover preview', () => {
  it('names the channels, and Z / T only when the image has more than one', () => {
    expect(previewCaption({ channelNames: ['a', 'b', 'c'] })).toBe('3 channels')
    expect(previewCaption({ sizeC: 1, sizeZ: 1, sizeT: 1 })).toBe('1 channel')
    expect(previewCaption({ sizeC: 2, sizeZ: 12, sizeT: 40 })).toBe('2 channels · Z 12 · T 40')
    expect(previewCaption({})).toBe('')
  })

  it('escapes the name — it is the user\'s text, rendered as HTML', () => {
    const html = previewHtml('data:x', '<b>tumour & "LN"</b>', '')
    expect(html).not.toContain('<b>')
    expect(html).toContain('&lt;b&gt;tumour &amp; &quot;LN&quot;&lt;/b&gt;')
    expect(html).not.toContain('thumb-tip-cap')
  })
})
