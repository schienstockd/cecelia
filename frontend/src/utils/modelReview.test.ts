import { describe, expect, it } from 'vitest'
import { parseCategoryLabels, parseReview, reviewGroups } from './modelReview'

describe('modelReview', () => {
  it('parses a stored review: points, the interpretation apart, a failed run kept with its error', () => {
    expect(parseReview(null)).toBeNull()
    const r = parseReview({ turnId: 'kt', status: 'done', model: 'sonnet', abstain: false, claims: [
      { kind: 'question', category: 'shared', text: 'Is m1 a no-bead control?', sectionId: 's02' },
      { kind: 'observation', text: 'no category' },
      { kind: 'interpretation', text: 'Counting beads works.' }, 'junk'] })!
    expect(r.items.map(x => [x.kind, x.category, x.action])).toEqual([['question', 'shared', ''], ['observation', 'other', '']])
    expect(r.supports).toBe('Counting beads works.')
    expect(r.lastError).toBe('')
    expect(parseReview({ status: 'done', claims: [], lastError: 'limit' })!.lastError).toBe('limit')
  })
  it('groups points in the labels’ order, unknown categories last under their id', () => {
    const labels = parseCategoryLabels({ settings: 'Settings', drift: 'Drift', other: 'Other' })
    const r = parseReview({ claims: [
      { kind: 'observation', category: 'mystery', text: 'a' },
      { kind: 'observation', category: 'drift', text: 'b' },
      { kind: 'observation', category: 'settings', text: 'c' },
      { kind: 'observation', category: 'drift', text: 'd' }] })
    expect(reviewGroups(r, labels).map(g => [g.label, g.items.length])).toEqual([['Settings', 1], ['Drift', 2], ['mystery', 1]])
    expect(reviewGroups(null, labels)).toEqual([])
    expect(parseCategoryLabels(undefined)).toEqual({})
  })
})
