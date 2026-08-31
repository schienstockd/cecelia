import { describe, it, expect } from 'vitest'
import { quitTaskPhrase, quitConfirmTooltip, quitConfirmLabel } from './quitWarning'

describe('quitTaskPhrase', () => {
  it('is empty when idle, so the common case gains no noise', () => {
    expect(quitTaskPhrase(0)).toBe('')
    expect(quitTaskPhrase(-1)).toBe('')
  })

  it('singularises one task', () => {
    expect(quitTaskPhrase(1)).toBe('1 task running')
    expect(quitTaskPhrase(2)).toBe('2 tasks running')
  })
})

describe('quit tooltips', () => {
  it('keep the plain description when nothing is running', () => {
    expect(quitConfirmTooltip(0)).toBe('Confirm quit — stops notebooks and the backend')
    expect(quitConfirmLabel(0)).toBe('Quit everything')
  })

  it('say what will be lost when work is in flight', () => {
    expect(quitConfirmTooltip(1)).toBe('Confirm quit — kills 1 task running')
    expect(quitConfirmTooltip(3)).toContain('3 tasks running')
  })

  it('put the count in the button label where there is room for text', () => {
    expect(quitConfirmLabel(0)).toBe('Quit everything')
    expect(quitConfirmLabel(1)).toBe('Quit — kills 1 task')
    expect(quitConfirmLabel(4)).toBe('Quit — kills 4 tasks')
  })

  // UI copy budget (docs/UI.md): a phrase, not a sentence. Guard against it growing into prose.
  it('stay short', () => {
    for (const n of [0, 1, 12]) {
      expect(quitConfirmTooltip(n).split(' ').length).toBeLessThanOrEqual(11)
      expect(quitConfirmLabel(n).split(' ').length).toBeLessThanOrEqual(11)
    }
  })
})
