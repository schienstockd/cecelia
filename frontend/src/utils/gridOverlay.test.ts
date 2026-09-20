import { describe, it, expect } from 'vitest'
import {
  GRID_DENSITY_MIN, GRID_DENSITY_MAX, GRID_DENSITY_DEFAULT,
  clampDensity, columnLetters, cellLabel, gridCells,
} from './gridOverlay'

describe('density bounds', () => {
  it('exposes the plan-locked default and range (Decision 13)', () => {
    expect(GRID_DENSITY_DEFAULT).toBe(8)
    expect(GRID_DENSITY_MIN).toBe(4)
    expect(GRID_DENSITY_MAX).toBe(32)
  })
  it('clamps + rounds arbitrary input to the range', () => {
    expect(clampDensity(1)).toBe(4)
    expect(clampDensity(100)).toBe(32)
    expect(clampDensity(8.4)).toBe(8)
    expect(clampDensity(8.6)).toBe(9)
    expect(clampDensity(-5)).toBe(4)
  })
})

describe('columnLetters', () => {
  it('single letters for the first 26 columns', () => {
    expect(columnLetters(0)).toBe('A')
    expect(columnLetters(7)).toBe('H')           // "A1..H8" — H is column 7
    expect(columnLetters(15)).toBe('P')          // max single-letter under GRID_DENSITY_MAX
    expect(columnLetters(25)).toBe('Z')
  })
  it('spreadsheet-style AA..ZZ once past Z (defensive; slider clamp keeps us in A..P today)', () => {
    expect(columnLetters(26)).toBe('AA')
    expect(columnLetters(27)).toBe('AB')
    expect(columnLetters(51)).toBe('AZ')
    expect(columnLetters(52)).toBe('BA')
  })
  it('rejects negative columns', () => {
    expect(columnLetters(-1)).toBe('')
  })
})

describe('cellLabel + gridCells', () => {
  it('top-left is A1', () => {
    expect(cellLabel(0, 0)).toBe('A1')
  })
  it('the 8×8 case names H8 as bottom-right', () => {
    expect(cellLabel(7, 7)).toBe('H8')
  })
  it('gridCells is row-major and has the right length', () => {
    const g = gridCells(4, 3)
    expect(g).toHaveLength(12)
    expect(g[0]).toEqual({ row: 0, col: 0, label: 'A1' })
    expect(g[1]).toEqual({ row: 0, col: 1, label: 'B1' })
    expect(g[4]).toEqual({ row: 1, col: 0, label: 'A2' })    // row 2 starts after 4 cols
    expect(g[11]).toEqual({ row: 2, col: 3, label: 'D3' })
  })
})
