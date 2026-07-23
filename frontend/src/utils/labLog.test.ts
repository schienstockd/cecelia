import { describe, it, expect } from 'vitest'
import {
  authorKind, correctionPrefill, draftToLines, unseenClaudeCount,
  entryId, decisionPrefill, isRatable, resolveImageRefs, visibleEntries,
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

describe('labLog.resolveImageRefs', () => {
  const map = { ab12cd: 'M1c-MERTK', ef34gh: 'M2b-WT' }
  it('swaps known UID tokens for names, in-place', () => {
    expect(resolveImageRefs('cellpose on 2 images (ab12cd, ef34gh)', map))
      .toBe('cellpose on 2 images (M1c-MERTK, M2b-WT)')
  })
  it('resolves a UID inside a detail line', () => {
    expect(resolveImageRefs('↳ may have a hot pixel — ch 0-3 (ab12cd)', map))
      .toBe('↳ may have a hot pixel — ch 0-3 (M1c-MERTK)')
  })
  it('leaves unknown UIDs and other text untouched', () => {
    expect(resolveImageRefs('excluded zz99xx', map)).toBe('excluded zz99xx')
    expect(resolveImageRefs('3 images', map)).toBe('3 images')
  })
  it('matches whole tokens only (a UID as a substring of a longer word is left alone)', () => {
    expect(resolveImageRefs('ab12cdef is not the image', map)).toBe('ab12cdef is not the image')
  })
  it('returns text unchanged for an empty map', () => {
    expect(resolveImageRefs('(ab12cd)', {})).toBe('(ab12cd)')
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
