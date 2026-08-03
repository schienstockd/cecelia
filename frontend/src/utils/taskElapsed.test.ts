import { describe, it, expect } from 'vitest'
import { parseRailTime, formatTaskDuration, taskElapsed } from './taskElapsed'

describe('parseRailTime', () => {
  it('parses the rail format as UTC', () => {
    const d = parseRailTime('2026-08-03T05:16:45.123Z')
    expect(d?.toISOString()).toBe('2026-08-03T05:16:45.123Z')
  })

  it('treats a missing or unparseable value as not-known, never an Invalid Date', () => {
    // '' is what the backend sends for "this never started / nobody noted it" — it must not become
    // epoch zero, or the UI shows a duration of decades.
    expect(parseRailTime('')).toBeUndefined()
    expect(parseRailTime(undefined)).toBeUndefined()
    expect(parseRailTime(null)).toBeUndefined()
    expect(parseRailTime(0)).toBeUndefined()
    expect(parseRailTime('not a date')).toBeUndefined()
  })
})

describe('formatTaskDuration', () => {
  it('scales seconds → minutes → hours', () => {
    expect(formatTaskDuration(0)).toBe('0s')
    expect(formatTaskDuration(42_400)).toBe('42s')
    expect(formatTaskDuration(59_400)).toBe('59s')
    expect(formatTaskDuration(60_000)).toBe('1m 00s')
    expect(formatTaskDuration(252_000)).toBe('4m 12s')
    expect(formatTaskDuration(3_600_000)).toBe('1h 00m')
    expect(formatTaskDuration(5_430_000)).toBe('1h 30m')
  })

  it('never prints a negative (clock skew between server and browser)', () => {
    expect(formatTaskDuration(-5_000)).toBe('0s')
  })
})

describe('taskElapsed', () => {
  const started  = new Date('2026-08-03T05:00:00.000Z')
  const finished = new Date('2026-08-03T05:04:12.000Z')

  it('is undefined until the task starts', () => {
    expect(taskElapsed(undefined, undefined, Date.now())).toBeUndefined()
  })

  it('runs to `now` while in flight and freezes at finishedAt', () => {
    expect(taskElapsed(started, undefined, started.getTime() + 90_000)).toBe('1m 30s')
    expect(taskElapsed(started, finished, started.getTime() + 999_999)).toBe('4m 12s')
  })
})
