import { describe, expect, it } from 'vitest'
import { effectiveFold, foldWrite } from './narrowLayout'

describe('effectiveFold', () => {
  it('a wide window shows the saved choice', () => {
    expect(effectiveFold(false, false, null)).toBe(false)
    expect(effectiveFold(true, false, null)).toBe(true)
  })
  it('a wide window ignores an override left from a narrow spell', () => {
    expect(effectiveFold(false, false, true)).toBe(false)
  })
  it('a narrow window folds by default, even when the saved choice is open', () => {
    expect(effectiveFold(false, true, null)).toBe(true)
  })
  it('a narrow window follows a click made while narrow', () => {
    expect(effectiveFold(false, true, false)).toBe(false)
    expect(effectiveFold(true, true, false)).toBe(false)
  })
})

describe('foldWrite', () => {
  it('a click on a narrow window never touches the saved choice', () => {
    expect(foldWrite(true, false)).toEqual({ override: false })
  })
  it('a click on a wide window is the saved choice', () => {
    expect(foldWrite(false, true)).toEqual({ saved: true })
  })
})
