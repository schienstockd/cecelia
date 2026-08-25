import { describe, it, expect } from 'vitest'
import {
  parseUiLogRing, serialiseUiLogRing, uiLogFromStorageEvent, uiLogPayload,
  type UiLogLine,
} from './uiLogChannel'

// Only the pure halves, the same split as `openProjectChannel`: the listener and the write are the
// untestable ends, and everything that can be wrong is in the decision between them.
const ev = (key: string | null, newValue: string | null) => ({ key, newValue })

describe('uiLogFromStorageEvent', () => {
  it('reads a published line', () => {
    expect(uiLogFromStorageEvent(ev('cc.uiLog', JSON.stringify(
      { level: 'error', message: 'GPU: bad pipeline', source: 'viewer', ts: '2026-08-25T01:02:03.000Z' }))))
      .toEqual({
        level: 'error', message: 'GPU: bad pipeline', source: 'viewer', detail: undefined,
        ts: '2026-08-25T01:02:03.000Z',
      })
  })

  // Each ignore really happens: the settings store writes a dozen other keys, a `clear()` arrives as
  // key null, and a half-upgraded second window during a dev reload writes something unreadable.
  it('ignores another key, a clear, and an unreadable value', () => {
    expect(uiLogFromStorageEvent(ev('cc.openProject', 'zolIMa'))).toBeNull()
    expect(uiLogFromStorageEvent(ev(null, 'x'))).toBeNull()
    expect(uiLogFromStorageEvent(ev('cc.uiLog', 'not json'))).toBeNull()
    expect(uiLogFromStorageEvent(ev('cc.uiLog', '{"level":"info"}'))).toBeNull()   // no message
    expect(uiLogFromStorageEvent(ev('cc.uiLog', null))).toBeNull()
  })

  it('falls back rather than dropping a line with an odd level or no source', () => {
    const line = uiLogFromStorageEvent(ev('cc.uiLog', '{"message":"hi","level":"trace"}'))
    expect(line?.level).toBe('info')
    expect(line?.source).toBe('app')
  })
})

describe('parseUiLogRing', () => {
  const line = (over: Partial<UiLogLine> = {}): UiLogLine => ({
    level: 'info', message: 'hi', source: 'app', ts: '2026-08-25T01:02:03.000Z', ...over,
  })

  it('returns the stored lines', () => {
    const raw = JSON.stringify([line({ message: 'a' }), line({ message: 'b', level: 'error' })])
    expect(parseUiLogRing(raw)).toEqual([
      line({ message: 'a' }), line({ message: 'b', level: 'error' }),
    ])
  })

  // Same reasoning as uiLogFromStorageEvent: private mode gives null, a hand-edited value fails to
  // parse, a half-upgraded write is an object not an array. None of these should hide the whole ring.
  it('returns empty rather than throwing on absent, unparseable, or non-array input', () => {
    expect(parseUiLogRing(null)).toEqual([])
    expect(parseUiLogRing('not json')).toEqual([])
    expect(parseUiLogRing('{"not":"an array"}')).toEqual([])
  })

  it('skips a bad row rather than the whole ring', () => {
    const good = line({ message: 'good' })
    const raw = JSON.stringify([good, { level: 'info' /* no message */ }, null, good])
    expect(parseUiLogRing(raw)).toEqual([good, good])
  })

  it('normalises an odd level or missing source, mirroring the storage-event reader', () => {
    const raw = JSON.stringify([{ message: 'hi', level: 'trace', ts: '2026-08-25T01:02:03.000Z' }])
    expect(parseUiLogRing(raw)[0]).toMatchObject({ level: 'info', source: 'app' })
  })
})

describe('serialiseUiLogRing', () => {
  const line = (i: number): UiLogLine => ({
    level: 'info', message: `line ${i}`, source: 'app', ts: '2026-08-25T01:02:03.000Z',
  })

  it('appends the new line at the end so hydration replays in order', () => {
    const out = parseUiLogRing(serialiseUiLogRing([line(1), line(2)], line(3)))
    expect(out.map(l => l.message)).toEqual(['line 1', 'line 2', 'line 3'])
  })

  // Eviction is why the ring exists as a bounded thing: a runaway UI storm ("GPU: Draw failed", every
  // frame) must not fill localStorage and start rejecting the settings store's writes.
  it('evicts the oldest lines when over cap', () => {
    const existing = Array.from({ length: 5 }, (_, i) => line(i + 1))
    const out = parseUiLogRing(serialiseUiLogRing(existing, line(99), 3))
    expect(out.map(l => l.message)).toEqual(['line 4', 'line 5', 'line 99'])
  })
})

describe('uiLogPayload', () => {
  it('differs between two identical messages, so both deliver', () => {
    // `storage` fires only when the value CHANGES. Without the counter the second of two identical
    // lines — which is what a repeating GPU error looks like — would arrive once and then go silent.
    const line = { level: 'error' as const, message: 'same', source: 'viewer' }
    const at = () => '2026-08-25T01:02:03.000Z'
    expect(uiLogPayload(line, 1, at)).not.toBe(uiLogPayload(line, 2, at))
  })

  it('round-trips through the reader', () => {
    const payload = uiLogPayload(
      { level: 'warn', message: 'Integrated GPU', source: 'viewer', detail: 'maxTextureDimension3D=2048' }, 7)
    expect(uiLogFromStorageEvent(ev('cc.uiLog', payload))).toMatchObject({
      level: 'warn', message: 'Integrated GPU', source: 'viewer',
      detail: 'maxTextureDimension3D=2048',
    })
  })
})
