// RESULT-REVIEW: result-985892bf
// A chain node's chosen label set is the plan for the images it runs on. Run on a set where only the
// exemplar has `Tcells` and every image has `default`, the intersection is `['default']` — the chain
// editor (`keepChosen`) keeps `Tcells`; a module page still moves off a name the images lack.
import { describe, it, expect } from 'vitest'
import { autoValueName } from './paramValues'

describe('autoValueName in the chain editor', () => {
  it('keeps a chosen label set the other images lack', () => {
    expect(autoValueName('Tcells', 'default', ['default'], 'labelPropsNames', null, true)).toBeNull()
  })
  it('a module page still moves to an offered name', () => {
    expect(autoValueName('Tcells', 'default', ['default'], 'labelPropsNames')).toBe('default')
  })
})
