import { describe, it, expect } from 'vitest'
import { boardGroupKey, relBoardKey, rekeyBoards } from './boardKeys'

describe('boardKeys — project-relative analysis-board keys', () => {
  it('relBoardKey strips any analysis:<uid>: prefix and is idempotent', () => {
    expect(relBoardKey('analysis:ABCdef:tab:3')).toBe('tab:3')   // legacy (old uid)
    expect(relBoardKey('tab:3')).toBe('tab:3')                   // already relative
    expect(relBoardKey('analysis:NEWuid:tab:3')).toBe('tab:3')
    expect(relBoardKey(relBoardKey('analysis:X:tab:1'))).toBe('tab:1') // idempotent
  })

  it('rekeyBoards re-applies the CURRENT group to legacy AND relative keys', () => {
    const gk = boardGroupKey('NEWuid')
    const loaded = {
      'analysis:OLDuid:tab:1': { a: 1 },   // legacy on-disk (baked old uid) → must survive a uid change
      'tab:2': { a: 2 },                    // new relative on-disk form
    }
    expect(rekeyBoards(gk, loaded)).toEqual({
      'analysis:NEWuid:tab:1': { a: 1 },
      'analysis:NEWuid:tab:2': { a: 2 },
    })
  })

  it('rekeyBoards tolerates null/empty', () => {
    expect(rekeyBoards('analysis:X', null)).toEqual({})
    expect(rekeyBoards('analysis:X', undefined)).toEqual({})
  })
})
