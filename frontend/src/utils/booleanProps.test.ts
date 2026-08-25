import { describe, it, expect } from 'vitest'
import { propsBlock, optionalBooleanProps, booleanUndefinedChecks } from './booleanProps'

const SFC = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

describe('optionalBooleanProps', () => {
  it('finds a bare optional boolean and skips the unions that suppress the cast', () => {
    const src = `defineProps<{ a: string; open?: boolean; shut?: boolean | null; s?: boolean | string }>()`
    expect(optionalBooleanProps(src)).toEqual(['open'])
  })
  it('skips a REQUIRED boolean — the parent always passes it, so there is nothing to detect', () => {
    expect(optionalBooleanProps('defineProps<{ on: boolean }>()')).toEqual([])
  })
  it('reads the whole literal, braces and all', () => {
    const src = 'defineProps<{ a?: boolean; cb?: { x: number }; b?: boolean }>()'
    expect(propsBlock(src).endsWith('}')).toBe(true)
    expect(optionalBooleanProps(src)).toEqual(['a', 'b'])
  })
})

describe('booleanUndefinedChecks', () => {
  it('catches the check that is always false', () => {
    const src = `defineProps<{ open?: boolean }>()
      const v = props.open === undefined ? inner.value : props.open`
    expect(booleanUndefinedChecks(src)).toEqual(['open'])
  })
  it('leaves a union alone — that one really can be undefined', () => {
    const src = `defineProps<{ open?: boolean | null }>()
      const v = props.open === undefined`
    expect(booleanUndefinedChecks(src)).toEqual([])
  })

  // The ratchet. An absent optional Boolean prop is `false`, so this comparison never fires — and when
  // a component uses it to tell "the parent is controlling me" from "I manage myself", every uncontrolled
  // instance in the app flips to controlled and answers to nobody.
  it('no SFC compares a bare optional boolean prop to undefined', () => {
    const bad: string[] = []
    for (const [path, src] of Object.entries(SFC))
      for (const n of booleanUndefinedChecks(src)) bad.push(`${path}: ${n}`)
    expect(bad).toEqual([])
  })
})
