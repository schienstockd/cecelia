import { describe, it, expect } from 'vitest'
import { parsePushTarget, pushChipLabel } from './pushTarget'

describe('parsePushTarget', () => {
  it('unpaired for anything that is not { paired: true }', () => {
    expect(parsePushTarget(null)).toEqual({ paired: false })
    expect(parsePushTarget(undefined)).toEqual({ paired: false })
    expect(parsePushTarget({})).toEqual({ paired: false })
    expect(parsePushTarget({ paired: false })).toEqual({ paired: false })
    expect(parsePushTarget('nope')).toEqual({ paired: false })
  })

  it('paired reads all four public fields, tolerating missing strings', () => {
    expect(parsePushTarget({
      paired: true,
      sessionLabel: 'probe-t',
      socketPath: '/tmp/x.sock',
      pairedAt: '2026-09-19T14:00:00',
      pairedFromPid: '12345',
    })).toEqual({
      paired: true,
      sessionLabel: 'probe-t',
      socketPath: '/tmp/x.sock',
      pairedAt: '2026-09-19T14:00:00',
      pairedFromPid: '12345',
    })
    // partial record ⇒ missing strings default to '' (the record on disk may have been
    // written by an older PR before all fields were populated)
    expect(parsePushTarget({ paired: true })).toEqual({
      paired: true, sessionLabel: '', socketPath: '', pairedAt: '', pairedFromPid: '',
    })
  })

  it('never exposes a token field — that credential stays server-side', () => {
    // Even if the server accidentally sends one, the parser drops it.
    const parsed = parsePushTarget({ paired: true, token: 'SHOULD_NOT_APPEAR' }) as Record<string, unknown>
    expect(parsed.token).toBeUndefined()
  })
})

describe('pushChipLabel', () => {
  it('names the session when one is set', () => {
    expect(pushChipLabel({
      paired: true, sessionLabel: 'probe-t', socketPath: '', pairedAt: '', pairedFromPid: '',
    })).toBe('paired ✓ probe-t')
  })
  it('bare tick when the pairing has no session label', () => {
    expect(pushChipLabel({
      paired: true, sessionLabel: '', socketPath: '', pairedAt: '', pairedFromPid: '',
    })).toBe('paired ✓')
  })
  it('unpaired reads plainly', () => {
    expect(pushChipLabel({ paired: false })).toBe('not paired')
  })
})
