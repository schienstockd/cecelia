import { describe, it, expect } from 'vitest'
import { soloVisibility, stepChannel } from './channelSolo'

describe('soloVisibility', () => {
  it('keeps only the given channel', () => {
    expect(soloVisibility(3, 1)).toEqual([false, true, false])
  })
  it('falls back to channel 0 when nothing was visible', () => {
    expect(soloVisibility(3, -1)).toEqual([true, false, false])
    expect(soloVisibility(3, 7)).toEqual([true, false, false])
  })
})

describe('stepChannel', () => {
  it('steps and wraps both ways', () => {
    expect(stepChannel(0, 1, 3)).toBe(1)
    expect(stepChannel(2, 1, 3)).toBe(0)
    expect(stepChannel(0, -1, 3)).toBe(2)
  })
  it('starts at 0 when nothing is shown', () => {
    expect(stepChannel(-1, 1, 3)).toBe(0)
    expect(stepChannel(-1, -1, 3)).toBe(0)
  })
  it('wraps within the drawable cap, not past it', () => {
    expect(stepChannel(5, 1, 6)).toBe(0)
  })
  it('has nowhere to go with no channels', () => {
    expect(stepChannel(-1, 1, 0)).toBe(-1)
  })
})
