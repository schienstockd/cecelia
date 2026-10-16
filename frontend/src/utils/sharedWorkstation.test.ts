import { describe, it, expect } from 'vitest'
import { SHARED_WORKSTATION_ITEMS, sharedWorkstationRows } from './sharedWorkstation'

describe('sharedWorkstationRows', () => {
  it('joins the catalogue with server values', () => {
    const rows = sharedWorkstationRows({ oneClaudeLogin: true })
    expect(rows.map(r => [r.key, r.value])).toEqual([['oneClaudeLogin', true]])
    expect(rows[0].label).toBeTruthy()
  })

  it('drops an item the server does not report, ignores one it has no label for', () => {
    expect(sharedWorkstationRows({})).toEqual([])
    expect(sharedWorkstationRows({ oneClaudeLogin: 'yes', somethingNew: true })).toEqual([])
  })

  it('every catalogue entry has a label, tooltip and hint', () => {
    for (const it of SHARED_WORKSTATION_ITEMS) {
      expect(it.label && it.tip && it.hint).toBeTruthy()
    }
  })
})
