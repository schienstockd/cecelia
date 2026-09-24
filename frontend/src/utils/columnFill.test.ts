import { describe, it, expect } from 'vitest'
import { fillColumnKey, lengthPx, fillExtraPx } from './columnFill'

describe('columnFill', () => {
  it('the spare width goes to the last resizable column, never a fixed one', () => {
    expect(fillColumnKey([{ key: 'n', fixed: true }, { key: 'text' }, { key: 'refs' }])).toBe('refs')
    expect(fillColumnKey([{ key: 'a' }, { key: 'icon', fixed: true }])).toBe('a')
    expect(fillColumnKey([{ key: 'icon', fixed: true }])).toBeNull()
  })
  it('reads the lengths the table declares', () => {
    expect(lengthPx('120px')).toBe(120)
    expect(lengthPx('1.6rem')).toBe(25.6)
    expect(lengthPx('10%')).toBeNull()
  })
  it('extra is what the table has beyond its columns, and 0 when it cannot know', () => {
    expect(fillExtraPx(500, [22, 200, 120])).toBe(158)
    expect(fillExtraPx(300, [22, 200, 120])).toBe(0)            // narrower: the browser scales down
    expect(fillExtraPx(500, [22, null])).toBe(0)
  })
})
