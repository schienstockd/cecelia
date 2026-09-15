import { describe, it, expect } from 'vitest'
import { cleanPathInput } from './pathInput'

describe('cleanPathInput', () => {
  it('trims surrounding whitespace', () => {
    expect(cleanPathInput('  /a/b  ')).toBe('/a/b')
  })

  it('strips a matched pair of single quotes (macOS Copy as Pathname)', () => {
    expect(cleanPathInput("'/Users/mardi1/Documents/CAR T cell analysis/projects/7v1tTA'"))
      .toBe('/Users/mardi1/Documents/CAR T cell analysis/projects/7v1tTA')
  })

  it('strips a matched pair of double quotes', () => {
    expect(cleanPathInput('"C:\\Users\\me\\proj"')).toBe('C:\\Users\\me\\proj')
  })

  it('strips curly quotes (autocorrect paste)', () => {
    expect(cleanPathInput('\u2018/a/b\u2019')).toBe('/a/b')
    expect(cleanPathInput('\u201C/a/b\u201D')).toBe('/a/b')
  })

  it('trims first, then strips', () => {
    expect(cleanPathInput("  '/a/b'  ")).toBe('/a/b')
  })

  it('leaves mismatched quotes alone', () => {
    expect(cleanPathInput("'/a/b\"")).toBe('\'/a/b"')
    expect(cleanPathInput("'/a/b")).toBe("'/a/b")
  })

  it('leaves embedded quotes alone', () => {
    expect(cleanPathInput("/a/it's/b")).toBe("/a/it's/b")
  })

  it('handles empty and single-char inputs safely', () => {
    expect(cleanPathInput('')).toBe('')
    expect(cleanPathInput('   ')).toBe('')
    expect(cleanPathInput("'")).toBe("'")
  })

  it('strips only ONE pair (paths with literal doubled quotes are exotic; keep behaviour predictable)', () => {
    expect(cleanPathInput("''/a/b''")).toBe("'/a/b'")
  })
})
