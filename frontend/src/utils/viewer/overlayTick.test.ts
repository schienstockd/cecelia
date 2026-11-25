import { describe, it, expect } from 'vitest'
import { encodeOverlayTick, parseOverlayTick, widerScope, writeScope, patchPopsLook, layerFetches, lookOf } from './overlayTick'
import type { OverlayPop } from '../viewerOverlays'

const pop = (path: string, colour: string, show = true): OverlayPop =>
  ({ path, name: path, colour, show, isTrack: false, labels: [1, 2, 3] })

describe('overlay tick', () => {
  it('round-trips a scoped tick; a bare or unreadable one is `all`', () => {
    const look = { '/A': { colour: '#00ff00', show: false } }
    expect(parseOverlayTick(encodeOverlayTick({ imageUid: 'img', scope: 'look', valueName: 'seg', popType: 'flow', look }, 5)))
      .toEqual({ imageUid: 'img', scope: 'look', valueName: 'seg', popType: 'flow', look })
    expect(parseOverlayTick(encodeOverlayTick({ imageUid: 'img', scope: 'pops', valueName: 'a:b', popType: 'clust' }, 5)))
      .toEqual({ imageUid: 'img', scope: 'pops', valueName: 'a:b', popType: 'clust' })
    // every older pinger writes `<uid>:<ts>`
    expect(parseOverlayTick('img:1700000000000')).toEqual({ imageUid: 'img', scope: 'all' })
    expect(parseOverlayTick('img:1:{nope')).toEqual({ imageUid: 'img', scope: 'all' })
    expect(parseOverlayTick('img:1:{"scope":"look","valueName":"s","popType":"flow"}')).toEqual({ imageUid: 'img', scope: 'all' })
    expect(parseOverlayTick('')).toBeNull()
    // a scoped tick without its document cannot be scoped
    expect(encodeOverlayTick({ imageUid: 'img', scope: 'pops' }, 7)).toBe('img:7')
  })

  it('scopes widen and a write is scoped by what changed', () => {
    expect(widerScope(null, 'look')).toBe('look')
    expect(widerScope('pops', 'look')).toBe('pops')
    expect(widerScope('look', 'all')).toBe('all')
    expect(writeScope(new Set())).toBe('look')
    expect(writeScope(new Set(['/A']))).toBe('pops')
    expect(writeScope(undefined)).toBe('all')
  })

  it('patches colour + show by path and keeps the labels', () => {
    const pops = [pop('/A', '#ff0000'), pop('/B', '#0000ff')]
    const next = patchPopsLook(pops, { '/A': { colour: '#00ff00', show: false }, '/gone': { colour: '#fff', show: true } })
    expect(next.map(p => [p.path, p.colour, p.show])).toEqual([['/A', '#00ff00', false], ['/B', '#0000ff', true]])
    expect(next[0].labels).toBe(pops[0].labels)
    expect(next[1]).toBe(pops[1])
    expect(patchPopsLook(pops, { '/A': { colour: '#ff0000', show: true } })).toBe(pops)
    expect(lookOf([{ path: '/A', colour: '#1', show: true }, { path: '/sel', colour: '#2', show: true, transient: true }]))
      .toEqual({ '/A': { colour: '#1', show: true } })
  })

  it('a look tick fetches nothing; a pops tick only the edited layer\'s populations', () => {
    const wanted = [{ vn: 'seg', popType: 'flow' }, { vn: 'seg', popType: 'clust' }, { vn: 'nuc', popType: 'flow' }]
    const held = new Map(wanted.map(w => [`${w.vn}::${w.popType}`, '']))
    expect(layerFetches(wanted, held, '', { scope: 'look', valueName: 'seg', popType: 'flow' }))
      .toEqual(['reuse', 'reuse', 'reuse'])
    expect(layerFetches(wanted, held, '', { scope: 'pops', valueName: 'seg', popType: 'flow' }))
      .toEqual(['pops', 'reuse', 'reuse'])
    expect(layerFetches(wanted, held, '', { scope: 'all' })).toEqual(['full', 'full', 'full'])
    // a cluster map is lent to co-clustered segmentations: the same pop type elsewhere refetches its pops
    const cl = [{ vn: 'seg', popType: 'clust' }, { vn: 'nuc', popType: 'clust' }, { vn: 'nuc', popType: 'flow' }]
    const clHeld = new Map(cl.map(w => [`${w.vn}::${w.popType}`, '']))
    expect(layerFetches(cl, clHeld, '', { scope: 'look', valueName: 'seg', popType: 'clust' }))
      .toEqual(['reuse', 'pops', 'reuse'])
    expect(layerFetches(cl, clHeld, '', { scope: 'pops', valueName: 'seg', popType: 'clust' }))
      .toEqual(['pops', 'pops', 'reuse'])
    // cells not held, or held for another colour-by column: whole, whatever the scope
    expect(layerFetches(wanted, new Map([['seg::flow', 'area']]), '', { scope: 'pops', valueName: 'seg', popType: 'flow' }))
      .toEqual(['full', 'full', 'full'])
  })
})
