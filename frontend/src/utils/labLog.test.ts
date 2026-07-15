import { describe, it, expect } from 'vitest'
import {
  authorKind, correctionPrefill, draftToLines, unseenClaudeCount,
  type LabLogEntry,
} from './labLog'

const entry = (author: string, raw = author): LabLogEntry =>
  ({ date: '2026-07-15', author, lines: ['x'], raw })

describe('labLog.authorKind', () => {
  it('classifies correction before user (correction contains "user")', () => {
    expect(authorKind('User — correction')).toBe('correction')
    expect(authorKind('User')).toBe('user')
    expect(authorKind('Claude')).toBe('claude')
    expect(authorKind('Cecelia')).toBe('cecelia')
    expect(authorKind('Someone else')).toBe('other')
  })
  it('is case/whitespace-insensitive', () => {
    expect(authorKind('  CLAUDE ')).toBe('claude')
    expect(authorKind('user')).toBe('user')
  })
})

describe('labLog.correctionPrefill', () => {
  it('references the original by date and author', () => {
    expect(correctionPrefill({ date: '2026-07-12', author: 'Claude' }))
      .toBe('Corrects 2026-07-12 [Claude]: ')
  })
})

describe('labLog.draftToLines', () => {
  it('splits on newlines and drops blank lines', () => {
    expect(draftToLines('a\n\n  b  \n')).toEqual(['a', 'b'])
    expect(draftToLines('   ')).toEqual([])
    expect(draftToLines('single')).toEqual(['single'])
  })
})

describe('labLog.unseenClaudeCount', () => {
  const entries = [           // newest-first
    entry('Claude', 'c2'),
    entry('User', 'u1'),
    entry('Claude', 'c1'),
  ]
  it('counts Claude entries newer than the last seen raw', () => {
    expect(unseenClaudeCount(entries, null)).toBe(2)     // nothing seen → both Claude entries
    expect(unseenClaudeCount(entries, 'c2')).toBe(0)     // seen the newest → none unseen
    expect(unseenClaudeCount(entries, 'u1')).toBe(1)     // only c2 is newer than u1
  })
})
