import { describe, it, expect } from 'vitest'
import {
  authorKind, correctionPrefill, draftToLines, unseenClaudeCount,
  entryId, decisionPrefill, isRatable, muteGroups, muteCategoryLabel, visibleEntries,
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

describe('labLog.muteGroups', () => {
  it('splits into all pages + a general operations group, keeping order', () => {
    const g = muteGroups(['Segment', 'Gating', 'Clustering'], ['Edit', 'Manage images'], ['Gating'])
    expect(g.pages).toEqual(['Segment', 'Gating', 'Clustering'])   // all pages, always
    expect(g.operations).toEqual(['Edit', 'Manage images'])
  })
  it('folds an orphaned mute (in neither list) into operations so it can be un-muted', () => {
    const g = muteGroups(['Segment'], ['Edit'], ['OldCustom', 'Segment'])
    expect(g.pages).toEqual(['Segment'])
    expect(g.operations).toEqual(['Edit', 'OldCustom'])            // orphan appended to operations
  })
  it('handles empty / missing inputs', () => {
    expect(muteGroups([], [], [])).toEqual({ pages: [], operations: [] })
    expect(muteGroups(undefined as any, undefined as any, undefined as any))
      .toEqual({ pages: [], operations: [] })
  })
})

describe('labLog.muteCategoryLabel', () => {
  it('capitalises the first letter so chips read uniformly', () => {
    expect(muteCategoryLabel('import')).toBe('Import')      // task-spec lower-case tag
    expect(muteCategoryLabel('Segment')).toBe('Segment')    // already title-case → unchanged
    expect(muteCategoryLabel('Manage images')).toBe('Manage images')
  })
  it('handles empty / missing input', () => {
    expect(muteCategoryLabel('')).toBe('')
    expect(muteCategoryLabel(undefined as any)).toBe('')
  })
})

describe('labLog.visibleEntries', () => {
  it('filters out entries whose id is dismissed, keeps the rest', () => {
    const a = entry('Cecelia', 'raw-a'), b = entry('User', 'raw-b'), c = entry('Claude', 'raw-c')
    const out = visibleEntries([a, b, c], [entryId(b.raw)])
    expect(out).toEqual([a, c])          // b dropped
  })
  it('returns all entries when nothing dismissed; handles empty/missing', () => {
    const a = entry('User', 'raw-a')
    expect(visibleEntries([a], [])).toEqual([a])
    expect(visibleEntries([a], undefined as any)).toEqual([a])
    expect(visibleEntries(undefined as any, [])).toEqual([])
  })
})

describe('labLog.entryId', () => {
  it('is stable and distinct per content', () => {
    expect(entryId('## 2026-07-15 [Cecelia]\n- a')).toBe(entryId('## 2026-07-15 [Cecelia]\n- a'))
    expect(entryId('a')).not.toBe(entryId('b'))
    expect(entryId('x')).toMatch(/^[0-9a-f]{8}$/)   // 8-hex, zero-padded
  })
})

describe('labLog.isRatable', () => {
  it('only app/AI entries are ratable', () => {
    expect(isRatable('Cecelia')).toBe(true)
    expect(isRatable('Claude')).toBe(true)
    expect(isRatable('User')).toBe(false)
    expect(isRatable('User — correction')).toBe(false)
  })
})

describe('labLog.decisionPrefill', () => {
  it('carries the verdict + a reference to complete', () => {
    expect(decisionPrefill({ date: '2026-07-15', author: 'Cecelia' }, 'up'))
      .toBe('👍 re 2026-07-15 [Cecelia]: ')
    expect(decisionPrefill({ date: '2026-07-15', author: 'Cecelia' }, 'down'))
      .toBe('👎 re 2026-07-15 [Cecelia]: ')
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
