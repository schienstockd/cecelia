import { describe, it, expect } from 'vitest'
import {
  logGroup, isVisible, matchesQuery, gapBefore, formatEntry,
  LOG_GROUPS, DEFAULT_GROUPS, SERVER_LOG_SOURCES, restoreGroups, storeGroups, type LogGroup,
} from './logFilter'

const entry = (o: Partial<Parameters<typeof isVisible>[0]> = {}) =>
  ({ level: 'info' as const, message: 'hello', ...o })

describe('logGroup', () => {
  it('maps each backend source to its own chip', () => {
    for (const s of SERVER_LOG_SOURCES) expect(logGroup(s)).toBe(s)
  })

  it('folds the aliases that mean the same component', () => {
    expect(logGroup('server')).toBe('backend')   // the pre-rework tag, still in old ring entries
    expect(logGroup('viewer')).toBe('viewer')    // its own chip: the napari half is going
    expect(logGroup('chain')).toBe('tasks')
    expect(logGroup('task')).toBe('tasks')
  })

  it("treats an unknown source as the UI's own", () => {
    // The 19 ad-hoc frontend tags all land here, and a NEW one must too — needing a table edit before
    // a message is reachable is how a source ends up invisible.
    for (const s of ['manageImages', 'gating', 'movies', 'whiteboard', 'brandNewPanel'])
      expect(logGroup(s)).toBe('app')
    expect(logGroup(undefined)).toBe('app')
  })

  it('gives every group a chip', () => {
    const chips = new Set(LOG_GROUPS.map(g => g.value))
    for (const s of [...SERVER_LOG_SOURCES, 'manageImages', 'chain'])
      expect(chips.has(logGroup(s))).toBe(true)
  })
})

describe('default groups', () => {
  it('starts with the app-side groups on and the chatty children off', () => {
    expect(DEFAULT_GROUPS).toEqual(['app', 'backend', 'tasks', 'viewer'])
    for (const g of ['napari', 'preview', 'runner', 'notebooks'] as LogGroup[])
      expect(DEFAULT_GROUPS).not.toContain(g)
  })
})

describe('isVisible', () => {
  const groups: LogGroup[] = ['app', 'backend', 'tasks']

  it('hides a group that is switched off', () => {
    expect(isVisible(entry({ source: 'napari' }), { groups, level: 'all' })).toBe(false)
    expect(isVisible(entry({ source: 'backend' }), { groups, level: 'all' })).toBe(true)
  })

  it('NEVER hides an error behind a group chip', () => {
    // The rule the whole rework rests on: turning a chatty child off means "stop narrating", not
    // "stop telling me when you break". A console that can silently withhold a stacktrace is the bug.
    expect(isVisible(entry({ source: 'napari', level: 'error' }), { groups, level: 'all' })).toBe(true)
    expect(isVisible(entry({ source: 'preview', level: 'error' }), { groups: [], level: 'all' })).toBe(true)
    // a warning from a hidden child is still hidden — only errors get the override
    expect(isVisible(entry({ source: 'napari', level: 'warn' }), { groups, level: 'all' })).toBe(false)
  })

  it('still applies the level filter to that error', () => {
    expect(isVisible(entry({ source: 'napari', level: 'error' }), { groups, level: 'warn' })).toBe(false)
  })

  it('still applies the search to that error', () => {
    const e = entry({ source: 'napari', level: 'error', message: 'BoundsError' })
    expect(isVisible(e, { groups, level: 'all', query: 'bounds' })).toBe(true)
    expect(isVisible(e, { groups, level: 'all', query: 'zarr' })).toBe(false)
  })
})

describe('matchesQuery', () => {
  it('is case-insensitive across message, source and detail', () => {
    const e = entry({ message: 'Open failed', source: 'napari', detail: 'at zarr_utils.py:120' })
    expect(matchesQuery(e, 'OPEN')).toBe(true)
    expect(matchesQuery(e, 'napari')).toBe(true)
    // the file name lives in the STACK TRACE — searching only the row would miss the thing you
    // actually search for
    expect(matchesQuery(e, 'zarr_utils')).toBe(true)
    expect(matchesQuery(e, 'nothing here')).toBe(false)
  })

  it('an empty query matches everything', () => {
    expect(matchesQuery(entry(), '')).toBe(true)
    expect(matchesQuery(entry(), '   ')).toBe(true)
    expect(matchesQuery(entry(), undefined)).toBe(true)
  })
})

describe('gapBefore', () => {
  it('is silent on a contiguous stream', () => {
    expect(gapBefore(5, 4)).toBeNull()
  })

  it('reports the cursor to refetch from when frames were dropped', () => {
    // broadcast_ws drops a frame rather than blocking a worker thread, and until seq existed nothing
    // could notice. 4 → 9 means 5..8 exist and this tab never got them.
    expect(gapBefore(9, 4)).toBe(4)
  })

  it('does not fire on the first frame of a session', () => {
    expect(gapBefore(1, 0)).toBeNull()
    expect(gapBefore(412, 0)).toBeNull()   // joined mid-stream; the connect backfill covers it
  })

  it('does not fire when the counter went backwards', () => {
    // A restarted backend counts from 1 again — its seq is not comparable with the old one, and
    // "refetch from 400" against a fresh ring would be meaningless.
    expect(gapBefore(2, 400)).toBeNull()
    expect(gapBefore(400, 400)).toBeNull()  // duplicate
  })
})

describe('formatEntry', () => {
  it('writes a paste-ready line with the detail underneath', () => {
    const out = formatEntry({
      level: 'error', message: 'Open failed', source: 'napari',
      detail: 'Stacktrace:\n  at x', timestamp: new Date(2026, 0, 2, 13, 45, 6),
    })
    expect(out).toContain('13:45:06')
    expect(out).toContain('ERROR')
    expect(out).toContain('[napari]')
    expect(out).toContain('Open failed')
    expect(out).toContain('Stacktrace:')
  })

  it('omits what is absent rather than printing empty brackets', () => {
    expect(formatEntry({ level: 'info', message: 'plain' })).toBe('INFO plain')
  })
})

describe('restoreGroups', () => {
  it('turns a chip that did not exist yet ON, without disturbing the rest', () => {
    // The v1 shape — a bare array — cannot tell "the user turned this off" from "this chip did not
    // exist". A new chip arriving switched off is how a feature ships and appears not to work.
    const v1 = JSON.stringify(['app', 'backend'])
    expect(restoreGroups(v1)).toEqual(['app', 'backend', 'viewer'])
  })

  it('leaves a chip the user turned off in the newer shape alone', () => {
    const saved = storeGroups(['app', 'backend'])
    expect(restoreGroups(saved)).toEqual(['app', 'backend'])
  })

  it('never turns a quiet chip on by itself', () => {
    for (const g of LOG_GROUPS.filter(x => x.quiet).map(x => x.value)) {
      expect(restoreGroups(JSON.stringify(['app']))).not.toContain(g)
    }
  })

  it('falls back to the defaults rather than hiding the console', () => {
    expect(restoreGroups(null)).toEqual(DEFAULT_GROUPS)
    expect(restoreGroups('not json')).toEqual(DEFAULT_GROUPS)
    expect(restoreGroups('42')).toEqual(DEFAULT_GROUPS)
  })

  it('returns chip order, so a stored value is comparable between saves', () => {
    const order = LOG_GROUPS.map(g => g.value)
    const got = restoreGroups(storeGroups(['tasks', 'app']))
    expect(got).toEqual(order.filter(v => got.includes(v)))
  })
})
