import { describe, it, expect } from 'vitest'
import { gateEditGestures } from './gateGestures'

describe('gateEditGestures', () => {
  it('names vertex edits only for a polygon', () => {
    expect(gateEditGestures('polygon').join(' ')).toMatch(/double-click an edge/)
    for (const k of ['rectangle', 'range', 'quadrant'] as const)
      expect(gateEditGestures(k).join(' ')).not.toMatch(/double-click|right-click/)
  })
  it('always leads with the move gesture', () => {
    for (const k of ['polygon', 'rectangle', 'range', 'slice'] as const) expect(gateEditGestures(k)[0]).toBe('drag to move')
  })
})
