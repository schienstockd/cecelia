import { describe, it, expect } from 'vitest'
import { movieStreamUrl, movieDisplayName, sortMovies, anchoredScroll, type MovieEntry } from './movies'

describe('movieStreamUrl', () => {
  it('builds the range-serve URL with encoded params', () => {
    expect(movieStreamUrl('NRUBxU', 'myImage_animation.mp4'))
      .toBe('/api/movies/file?projectUid=NRUBxU&name=myImage_animation.mp4')
  })
  it('encodes names/uids that need escaping', () => {
    expect(movieStreamUrl('p 1', 'a b.mp4')).toBe('/api/movies/file?projectUid=p%201&name=a%20b.mp4')
  })
})

describe('movieDisplayName', () => {
  it('drops the .mp4 extension', () => {
    expect(movieDisplayName('gBT_animation.mp4')).toBe('gBT_animation')
    expect(movieDisplayName('CASE.MP4')).toBe('CASE')
  })
  it('leaves a name without extension alone', () => {
    expect(movieDisplayName('plain')).toBe('plain')
  })
})

describe('anchoredScroll', () => {
  const vp = { w: 1000, h: 500 }

  it('no scroll while the content still fits after zoom', () => {
    // fit (800×400) zoomed to just-fits (1000×500) → both axes fit → no scroll
    expect(anchoredScroll({ w: 800, h: 400 }, { w: 1000, h: 500 }, vp, { x: 500, y: 250 }, { left: 0, top: 0 }))
      .toEqual({ left: 0, top: 0 })
  })

  it('zooming about the centre keeps the centre fixed', () => {
    // centred fit 1000×500 → 2000×1000, focal = viewport centre → scroll to keep centre centred
    const s = anchoredScroll({ w: 1000, h: 500 }, { w: 2000, h: 1000 }, vp, { x: 500, y: 250 }, { left: 0, top: 0 })
    expect(s).toEqual({ left: 500, top: 250 })   // (0.5*2000 - 500), (0.5*1000 - 250)
  })

  it('zooming about the left edge keeps the left edge fixed', () => {
    const s = anchoredScroll({ w: 1000, h: 500 }, { w: 2000, h: 1000 }, vp, { x: 0, y: 0 }, { left: 0, top: 0 })
    expect(s).toEqual({ left: 0, top: 0 })       // fraction 0 under x=0 → no left scroll
  })

  it('clamps to the scrollable range', () => {
    // focal at far right → would want scroll beyond max; clamped to a-v = 1000
    const s = anchoredScroll({ w: 1000, h: 500 }, { w: 2000, h: 1000 }, vp, { x: 1000, y: 500 }, { left: 0, top: 0 })
    expect(s).toEqual({ left: 1000, top: 500 })
  })

  it('accounts for existing scroll when already overflowing', () => {
    // already zoomed (content 2000 wide, scrolled 500) → zoom to 4000 about centre keeps centre point
    const s = anchoredScroll({ w: 2000, h: 1000 }, { w: 4000, h: 2000 }, vp, { x: 500, y: 250 }, { left: 500, top: 250 })
    // content point under centre before = 500+500=1000 (frac 0.5) → after 0.5*4000 - 500 = 1500
    expect(s).toEqual({ left: 1500, top: 750 })
  })
})

describe('sortMovies', () => {
  it('orders newest-first, name as tiebreak, without mutating input', () => {
    const list: MovieEntry[] = [
      { name: 'b.mp4', size: 1, mtime: 100 },
      { name: 'a.mp4', size: 1, mtime: 200 },
      { name: 'c.mp4', size: 1, mtime: 200 },
    ]
    const sorted = sortMovies(list)
    expect(sorted.map(m => m.name)).toEqual(['a.mp4', 'c.mp4', 'b.mp4'])
    expect(list[0].name).toBe('b.mp4')   // original untouched
  })
})
