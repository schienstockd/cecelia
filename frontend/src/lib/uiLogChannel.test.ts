import { describe, it, expect } from 'vitest'
import { uiLogFromStorageEvent, uiLogPayload } from './uiLogChannel'

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
