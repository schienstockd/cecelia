import { describe, it, expect } from 'vitest'
import { toSeconds, frameSeconds, frameSecondsByImage, sharedFrameSeconds, frameAxisLabel } from './timeAxis'
import type { CciaImage } from '../stores/project'

const img = (o: Partial<CciaImage>): CciaImage => ({ uid: 'x', name: 'x', ...o } as CciaImage)

describe('toSeconds', () => {
  it('converts the recognised time units', () => {
    expect(toSeconds(30, 'second')).toBe(30)
    expect(toSeconds(30, 's')).toBe(30)
    expect(toSeconds(2, 'min')).toBe(120)
    expect(toSeconds(1, 'hour')).toBe(3600)
    expect(toSeconds(500, 'ms')).toBe(0.5)
  })

  // An unrecognised or missing unit is "we don't know", NOT "seconds" — the whole point of the null.
  it('returns null for an unknown or absent unit', () => {
    expect(toSeconds(30, 'parsec')).toBeNull()
    expect(toSeconds(30, '')).toBeNull()
    expect(toSeconds(30, undefined)).toBeNull()
  })
})

describe('frameSeconds', () => {
  it('reads the interval off the image', () => {
    expect(frameSeconds(img({ timeIncrement: 30, timeIncrementUnit: 'second' }))).toBe(30)
    expect(frameSeconds(img({ timeIncrement: 0.5, timeIncrementUnit: 'min' }))).toBe(30)
  })

  it('is null when the interval is missing, zero, or in an unknown unit', () => {
    expect(frameSeconds(img({ timeIncrementUnit: 'second' }))).toBeNull()          // no value
    expect(frameSeconds(img({ timeIncrement: 0, timeIncrementUnit: 'second' }))).toBeNull()
    expect(frameSeconds(img({ timeIncrement: 30 }))).toBeNull()                    // no unit
    expect(frameSeconds(null)).toBeNull()
  })
})

describe('frameSecondsByImage', () => {
  const lookup = (m: Record<string, CciaImage>) => (uid: string) => m[uid] ?? null

  it('maps every image to its OWN interval — two movies need not share one', () => {
    const m = {
      a: img({ uid: 'a', timeIncrement: 30, timeIncrementUnit: 'second' }),
      b: img({ uid: 'b', timeIncrement: 1, timeIncrementUnit: 'min' }),
    }
    expect(frameSecondsByImage(['a', 'b'], lookup(m))).toEqual({ a: 30, b: 60 })
  })

  // THE RULE: one unknown interval poisons the whole axis. Converting some series to seconds while
  // leaving others as frame indices would put incomparable numbers on one axis with nothing saying so
  // — so the caller must fall back to frames for ALL of them.
  it('is null when ANY image has no usable interval', () => {
    const m = {
      a: img({ uid: 'a', timeIncrement: 30, timeIncrementUnit: 'second' }),
      b: img({ uid: 'b' }),                                    // never calibrated
    }
    expect(frameSecondsByImage(['a', 'b'], lookup(m))).toBeNull()
    expect(frameSecondsByImage(['a'], lookup(m))).toEqual({ a: 30 })   // ...but 'a' alone is fine
  })

  it('is null for an unknown uid, and for no images at all', () => {
    expect(frameSecondsByImage(['missing'], lookup({}))).toBeNull()
    expect(frameSecondsByImage([], lookup({}))).toBeNull()
  })
})

describe('sharedFrameSeconds', () => {
  const lookup = (m: Record<string, CciaImage>) => (uid: string) => m[uid] ?? null

  it('returns the one interval when every movie agrees', () => {
    const m = {
      a: img({ uid: 'a', timeIncrement: 30, timeIncrementUnit: 'second' }),
      b: img({ uid: 'b', timeIncrement: 0.5, timeIncrementUnit: 'min' }),   // same 30 s, other unit
    }
    expect(sharedFrameSeconds(['a', 'b'], lookup(m))).toBe(30)
  })

  // A pooled/summarised curve has ONE x axis. Picking either interval would put a 30 s/frame movie
  // and a 60 s/frame one on the same seconds axis, one of them off by 2× — with nothing saying so.
  it('is null when the movies disagree', () => {
    const m = {
      a: img({ uid: 'a', timeIncrement: 30, timeIncrementUnit: 'second' }),
      b: img({ uid: 'b', timeIncrement: 60, timeIncrementUnit: 'second' }),
    }
    expect(sharedFrameSeconds(['a', 'b'], lookup(m))).toBeNull()
  })

  it('is null when any interval is unknown, or there are no images', () => {
    const m = { a: img({ uid: 'a', timeIncrement: 30, timeIncrementUnit: 'second' }), b: img({ uid: 'b' }) }
    expect(sharedFrameSeconds(['a', 'b'], lookup(m))).toBeNull()
    expect(sharedFrameSeconds([], lookup(m))).toBeNull()
  })
})

describe('frameAxisLabel', () => {
  it('names the temporal column as frames, never as bare time', () => {
    // the unit must stay: "Time" alone is the claim the SECONDS axis makes, and this axis is the
    // one that could not be converted
    expect(frameAxisLabel('centroid_t')).toBe('Time (frames)')
    expect(frameAxisLabel('CENTROID_T')).toBe('Time (frames)')
  })

  it('leaves a non-temporal groupBy as its own name', () => {
    expect(frameAxisLabel('hmm.state')).toBe('hmm.state')
    expect(frameAxisLabel('clusters.0')).toBe('clusters.0')
  })

  it('falls back to t when there is no groupBy at all', () => {
    for (const g of ['', undefined, null]) expect(frameAxisLabel(g)).toBe('t')
  })
})
