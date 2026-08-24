import { describe, it, expect } from 'vitest'
import { spanAnchorRate, secondsLabel, withDurationLabels } from './frameDuration'

const img = (uid: string, t: number | null, unit: string | null = 's') =>
  ({ uid, timeIncrement: t, timeIncrementUnit: unit })

describe('spanAnchorRate', () => {
  // COARSEST, matching `train_run.reference_interval`. The spans must be representable on every
  // selected movie: anchor on the coarsest and every finer one scales the lags UP, so none falls
  // below a frame. The finest anchor does the opposite — on lags [1,2,4] across 5/10/15 s/frame it
  // leaves 2 of 3 movies unable to carry the shortest span.
  it('is the coarsest interval, not the first or the mean', () => {
    const r = spanAnchorRate([img('slow', 15), img('fast', 5), img('mid', 10)])
    expect(r?.seconds).toBe(15)
    expect(r?.uid).toBe('slow')
    expect(r?.mixed).toBe(true)
  })

  it('is not mixed when every image agrees', () => {
    expect(spanAnchorRate([img('a', 15), img('b', 15)])?.mixed).toBe(false)
  })

  it('counts how many images can be read at durations at all', () => {
    const r = spanAnchorRate([img('a', 15), img('b', null), img('c', 30)])
    expect([r?.known, r?.total]).toEqual([2, 3])
  })

  // Converted, not refused — but marked, because the RUNNER refuses it. The form printing a rate the
  // run will skip is the drift worth catching, so the fact travels rather than the value being hidden.
  it('converts a stated non-second unit and marks it', () => {
    const r = spanAnchorRate([img('a', 250, 'ms')])
    expect([r?.seconds, r?.converted]).toEqual([0.25, true])
  })

  // An ABSENT unit reads as seconds: that is OME's spec default and `im_time_increment_unit`'s, so
  // the runner trains such a movie and the form must not claim it cannot.
  it('reads an absent unit as seconds, like the runner does', () => {
    const r = spanAnchorRate([img('a', 15, null)])
    expect([r?.seconds, r?.converted]).toEqual([15, false])
  })

  it('does not mark a rate that needed no conversion', () => {
    expect(spanAnchorRate([img('a', 15, 'second')])?.converted).toBe(false)
    expect(spanAnchorRate([img('a', 15, 's')])?.converted).toBe(false)
  })

  it('is null when nothing records an interval', () => {
    expect(spanAnchorRate([img('a', null), img('b', 0)])).toBeNull()
    expect(spanAnchorRate([img('a', 15, 'furlongs')])).toBeNull()   // a unit nobody can read
    expect(spanAnchorRate([])).toBeNull()
    expect(spanAnchorRate(undefined)).toBeNull()
  })
})

describe('secondsLabel', () => {
  it('prints a whole number of seconds as one', () => {
    expect(secondsLabel(60)).toBe('60s')
    expect(secondsLabel(7.5)).toBe('7.5s')
    expect(secondsLabel(2.75)).toBe('2.75s')
    // Two decimals is the cap — a lag times an interval can carry float noise, and `2.4000000000000004s`
    // on a chip reads as a bug.
    expect(secondsLabel(2.4000000000000004)).toBe('2.4s')
  })
})

describe('withDurationLabels', () => {
  const OPTS = [{ value: '1', label: '1' }, { value: '2', label: '2' }, { value: '8', label: '8' }]

  it('appends what each lag spans at this rate', () => {
    const rate = spanAnchorRate([img('a', 15)])
    expect(withDurationLabels(OPTS, rate).map(o => o.label))
      .toEqual(['1 · 15s', '2 · 30s', '8 · 120s'])
  })

  // The chip VALUE is what gets submitted and what `validate_params` checks against the spec's
  // options, so relabelling must never touch it.
  it('never changes the value', () => {
    const rate = spanAnchorRate([img('a', 15)])
    expect(withDurationLabels(OPTS, rate).map(o => o.value)).toEqual(['1', '2', '8'])
  })

  it('leaves the labels alone when no rate is known', () => {
    // `4 · 4s` off a defaulted 1.0 interval would be a measurement nobody made.
    expect(withDurationLabels(OPTS, null).map(o => o.label)).toEqual(['1', '2', '8'])
  })

  it('leaves a non-numeric option alone rather than printing NaN', () => {
    const rate = spanAnchorRate([img('a', 15)])
    expect(withDurationLabels([{ value: 'auto', label: 'auto' }], rate)[0]!.label).toBe('auto')
  })
})
