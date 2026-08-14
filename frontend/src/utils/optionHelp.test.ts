import { describe, it, expect } from 'vitest'
import { selectedOptionHelp } from './optionHelp'

const OPTS = [
  { label: 'Median', value: 'median', help: 'Photon-limited data.' },
  { label: 'Gated', value: 'gated', help: 'Well-exposed movies with moving cells.' },
  { label: 'Mean', value: 'mean' },
]

describe('selectedOptionHelp', () => {
  it('returns the help of the selected option', () => {
    expect(selectedOptionHelp(OPTS, 'gated')).toBe('Well-exposed movies with moving cells.')
  })

  it('is empty when the selected option declares none, so no note renders', () => {
    expect(selectedOptionHelp(OPTS, 'mean')).toBe('')
  })

  it('is empty for an unknown or absent value rather than throwing', () => {
    expect(selectedOptionHelp(OPTS, 'nope')).toBe('')
    expect(selectedOptionHelp(OPTS, undefined)).toBe('')
    expect(selectedOptionHelp(OPTS, null)).toBe('')
    expect(selectedOptionHelp(undefined, 'gated')).toBe('')
    expect(selectedOptionHelp([], 'gated')).toBe('')
  })

  it('matches a non-string value against a string option value', () => {
    // a spec default can be a number or a bool while a select's DOM value is always a string, so a
    // strict === would silently never match — leaving the note missing for exactly those params
    expect(selectedOptionHelp([{ label: 'Two', value: '2', help: 'two' }], 2)).toBe('two')
    expect(selectedOptionHelp([{ label: 'Yes', value: 'true', help: 'yes' }], true)).toBe('yes')
  })
})
