import { describe, it, expect } from 'vitest'
import {
  reconcilePopEyes, samePopEyes, setAllPopEyes, soloPopEyes, popEyeOn, eyeKey,
  type EyePop, type PopEyes, type ListedPop,
} from './popEyes'

const pop = (path: string, uid: string, show = true, popType = 'flow'): EyePop => ({ popType, path, uid, show })

describe('reconcilePopEyes', () => {
  it('first fetch with nothing stored seeds every eye from the manager show', () => {
    const next = [pop('/a', 'u1', true), pop('/b', 'u2', false)]
    expect(reconcilePopEyes({}, [], next, ['flow'])).toEqual({ 'flow::/a': true, 'flow::/b': false })
  })

  it('a refetch after a gate edit keeps the user toggles, even against the manager show', () => {
    const prev = [pop('/a', 'u1', true), pop('/b', 'u2', true)]
    // the user hid /a and the manager still shows it; the manager hid /b and the user showed it
    const stored: PopEyes = { 'flow::/a': false, 'flow::/b': true }
    const next = [pop('/a', 'u1', true), pop('/b', 'u2', false)]
    expect(reconcilePopEyes(stored, prev, next, ['flow'])).toEqual(stored)
  })

  it('a new pop is seeded from show; existing eyes are left alone', () => {
    const prev = [pop('/a', 'u1')]
    const next = [pop('/a', 'u1'), pop('/a/new', 'u9', false), pop('/a/on', 'u8', true)]
    expect(reconcilePopEyes({ 'flow::/a': false }, prev, next, ['flow']))
      .toEqual({ 'flow::/a': false, 'flow::/a/new': false, 'flow::/a/on': true })
  })

  it('seedOff (one-at-a-time) seeds a new pop off whatever its show', () => {
    expect(reconcilePopEyes({ 'flow::/a': true }, [pop('/a', 'u1')],
                            [pop('/a', 'u1'), pop('/b', 'u2', true)], ['flow'], true))
      .toEqual({ 'flow::/a': true, 'flow::/b': false })
  })

  it('a renamed pop carries its eye to the new path (uid match)', () => {
    const prev = [pop('/a', 'u1', true), pop('/b', 'u2', true)]
    const next = [pop('/renamed', 'u1', true), pop('/b', 'u2', true)]
    expect(reconcilePopEyes({ 'flow::/a': false, 'flow::/b': true }, prev, next, ['flow']))
      .toEqual({ 'flow::/renamed': false, 'flow::/b': true })
  })

  it('a deleted pop drops its eye', () => {
    const prev = [pop('/a', 'u1'), pop('/b', 'u2')]
    expect(reconcilePopEyes({ 'flow::/a': false, 'flow::/b': false }, prev, [pop('/b', 'u2')], ['flow']))
      .toEqual({ 'flow::/b': false })
  })

  it('a re-opened viewer (no previous payload) keeps stored eyes for pops still present', () => {
    expect(reconcilePopEyes({ 'flow::/a': false, 'flow::/gone': false }, [], [pop('/a', 'u1', true)], ['flow']))
      .toEqual({ 'flow::/a': false })
  })

  it('eyes of a pop type not fetched (panel turned it off) are kept untouched', () => {
    const stored = { 'flow::/a': false, 'clust::/c1': false }
    expect(reconcilePopEyes(stored, [pop('/a', 'u1')], [pop('/a', 'u1')], ['flow'])).toEqual(stored)
  })

  it('same path under two pop types are separate eyes', () => {
    const next = [pop('/x', 'u1', true, 'flow'), pop('/x', 'u2', false, 'clust')]
    expect(reconcilePopEyes({ 'flow::/x': false }, [], next, ['flow', 'clust']))
      .toEqual({ 'flow::/x': false, 'clust::/x': false })
  })
})

describe('master row helpers', () => {
  const listed: ListedPop[] = [
    { vn: 'segA', popType: 'flow', path: '/a' },
    { vn: 'segA', popType: 'flow', path: '/b' },
    { vn: 'segB', popType: 'flow', path: '/a' },
  ]
  const eyes = new Map<string, PopEyes>([['segA', { 'flow::/a': true, 'flow::/b': false, 'flow::/off-list': true }]])

  it('all off / all on act on every listed pop across segmentations, leaving others alone', () => {
    const off = setAllPopEyes(eyes, listed, false)
    expect(off.get('segA')).toEqual({ 'flow::/a': false, 'flow::/b': false, 'flow::/off-list': true })
    expect(off.get('segB')).toEqual({ 'flow::/a': false })
    const on = setAllPopEyes(off, listed, true)
    expect(on.get('segA')).toEqual({ 'flow::/a': true, 'flow::/b': true, 'flow::/off-list': true })
    expect(eyes.get('segA')!['flow::/b']).toBe(false)   // input not mutated
  })

  it('solo keeps one listed pop on — across segmentations — and falls back to the first', () => {
    const s = soloPopEyes(eyes, listed, 2)
    expect(s.get('segA')).toEqual({ 'flow::/a': false, 'flow::/b': false, 'flow::/off-list': true })
    expect(s.get('segB')).toEqual({ 'flow::/a': true })
    expect(soloPopEyes(eyes, listed, -1).get('segA')!['flow::/a']).toBe(true)
  })

  it('popEyeOn reads the stored eye, else the manager show', () => {
    expect(popEyeOn(eyes, 'segA', 'flow', '/b', true)).toBe(false)
    expect(popEyeOn(eyes, 'segB', 'flow', '/z', true)).toBe(true)
    expect(popEyeOn(eyes, 'segB', 'flow', '/z', false)).toBe(false)
  })

  it('samePopEyes / eyeKey', () => {
    expect(samePopEyes({ a: true }, { a: true })).toBe(true)
    expect(samePopEyes({ a: true }, { a: false })).toBe(false)
    expect(samePopEyes({ a: true }, { a: true, b: true })).toBe(false)
    expect(eyeKey('flow', '/a')).toBe('flow::/a')
  })
})
