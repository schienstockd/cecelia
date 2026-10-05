import { describe, it, expect } from 'vitest'
import { parseBoardRenderQuery, findBoardTab, captureSignature } from './boardRender'

describe('board render query', () => {
  it('reads the project and an optional image list', () => {
    expect(parseBoardRenderQuery({ project: 'RkJd6s', images: 'a, b,,c' }))
      .toEqual({ projectUid: 'RkJd6s', imageUids: ['a', 'b', 'c'] })
    expect(parseBoardRenderQuery({ project: ['x'] })).toEqual({ projectUid: 'x', imageUids: [] })
    expect(parseBoardRenderQuery({})).toEqual({ projectUid: '', imageUids: [] })
  })
})

describe('findBoardTab', () => {
  const tabs = [{ id: 1, name: 'Board 1' }, { id: 4, name: 'Run d12 · clustering' }, { id: 5, name: 'dup' }, { id: 6, name: 'DUP ' }]
  it('matches exactly, then loosely', () => {
    expect(findBoardTab(tabs, 'Run d12 · clustering')).toBe(4)
    expect(findBoardTab(tabs, ' board 1')).toBe(1)
  })
  it('refuses a missing or ambiguous name rather than rendering the wrong board', () => {
    expect(findBoardTab(tabs, 'nope')).toBeNull()
    expect(findBoardTab(tabs, 'Dup')).toBeNull()
    expect(findBoardTab(tabs, 'dup')).toBe(5)        // the exact match wins over the loose pair
  })
})

describe('captureSignature', () => {
  it('changes when any slot image changes, and not otherwise', () => {
    const a = [{ png: 'data:image/png;base64,AAAA' }, { png: null }]
    expect(captureSignature(a)).toBe(captureSignature([{ png: 'data:image/png;base64,AAAA' }, { png: null }]))
    expect(captureSignature(a)).not.toBe(captureSignature([{ png: 'data:image/png;base64,AAAB' }, { png: null }]))
    expect(captureSignature(a)).not.toBe(captureSignature([{ png: 'data:image/png;base64,AAAA' }, { png: 'x' }]))
    // same length, same tail (every PNG ends in its IEND chunk) — still told apart
    expect(captureSignature([{ png: 'Xa' + 'IEND'.repeat(20) }])).not.toBe(captureSignature([{ png: 'Ya' + 'IEND'.repeat(20) }]))
  })
})
