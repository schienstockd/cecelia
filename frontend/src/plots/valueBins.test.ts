import { describe, expect, it } from 'vitest'
import { valueBins } from './valueBins'
import { heatCss } from './flowColors'

describe('valueBins', () => {
  it('bins the finite range low → high across n steps; the max lands in the top bin', () => {
    const b = valueBins(new Float32Array([0, 5, 10]), 'grey', 4)
    expect([...b.cats]).toEqual([0, 2, 3])
    expect(b.extent).toEqual([0, 10])
    expect(b.palette).toEqual([heatCss(0.125), heatCss(0.375), heatCss(0.625), heatCss(0.875), 'grey'])
    expect(b.ticks.map(t => t.label)).toEqual(['0', '10'])
  })
  it('a point with no value gets the no-value slot, not the low end', () => {
    const b = valueBins(new Float32Array([NaN, 1, 2]), 'grey', 4)
    expect(b.cats[0]).toBe(4)
    expect(b.palette[4]).toBe('grey')
  })
  it('no values at all → no extent, every point "no value"', () => {
    const b = valueBins(new Float32Array([NaN, NaN]), 'grey', 4)
    expect(b.extent).toBeNull()
    expect([...b.cats]).toEqual([4, 4])
    expect(b.ticks).toEqual([])
  })
  it('one value everywhere → a flat middle colour and one tick', () => {
    const b = valueBins(new Float32Array([3, 3]), 'grey', 4)
    expect([...b.cats]).toEqual([2, 2])
    expect(b.ticks).toEqual([{ pos: 3, label: '3' }])
  })
})
