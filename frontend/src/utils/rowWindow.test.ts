import { describe, expect, it } from 'vitest'
import { nextRange, overscanPx, rowOffsets, windowRange } from './rowWindow'

describe('rowOffsets', () => {
  it('is the prefix sum: row tops, then the total', () => {
    expect([...rowOffsets([10, 20, 30])]).toEqual([0, 10, 30, 60])
    expect([...rowOffsets([])]).toEqual([0])
  })
})

describe('windowRange', () => {
  const off = rowOffsets([10, 10, 10, 10, 10])   // rows at 0,10,20,30,40; total 50
  it('takes every row that overlaps the band', () => {
    expect(windowRange(off, 15, 25)).toEqual({ start: 1, end: 3 })
  })
  it('a row ending exactly at y0 is out, one starting exactly at y1 is out', () => {
    expect(windowRange(off, 10, 20)).toEqual({ start: 1, end: 2 })
  })
  it('clamps a band past either end', () => {
    expect(windowRange(off, -100, 5)).toEqual({ start: 0, end: 1 })
    expect(windowRange(off, 45, 1000)).toEqual({ start: 4, end: 5 })
    expect(windowRange(off, 60, 80)).toEqual({ start: 5, end: 5 })
    expect(windowRange(off, -80, -60)).toEqual({ start: 0, end: 0 })
  })
  it('uneven heights: a tall row in view is rendered whole', () => {
    const o = rowOffsets([10, 200, 10])
    expect(windowRange(o, 100, 120)).toEqual({ start: 1, end: 2 })
  })
  it('no rows', () => {
    expect(windowRange(rowOffsets([]), 0, 100)).toEqual({ start: 0, end: 0 })
  })
})

describe('nextRange', () => {
  // 1000 rows of 20px; a 400px view → overscan 200px each side
  const off = rowOffsets(Array(1000).fill(20))
  it('fresh: the band plus a full overscan each side', () => {
    expect(overscanPx(400)).toBe(200)
    expect(nextRange(off, null, 2000, 2400)).toEqual({ start: 90, end: 130 })
  })
  it('keeps the rendered range while a small scroll stays inside half the overscan', () => {
    const prev = nextRange(off, null, 2000, 2400)
    expect(nextRange(off, prev, 2040, 2440)).toBe(prev)
  })
  it('re-windows once the band nears the rendered edge', () => {
    const prev = nextRange(off, null, 2000, 2400)
    expect(nextRange(off, prev, 2200, 2600)).toEqual({ start: 100, end: 140 })
  })
  it('re-windows when the rendered range is far taller than needed', () => {
    const all = { start: 0, end: 1000 }
    expect(nextRange(off, all, 0, 400)).toEqual({ start: 0, end: 30 })
  })
  it('a range for rows that are gone is replaced', () => {
    const short = rowOffsets(Array(5).fill(20))
    expect(nextRange(short, { start: 90, end: 130 }, 0, 400)).toEqual({ start: 0, end: 5 })
  })
})
