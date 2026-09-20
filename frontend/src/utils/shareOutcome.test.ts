import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { announceShareOutcome, shareClipboardPrompt, shareFailMessage } from './shareOutcome'

// Both surfaces that call announceShareOutcome (viewer Save + plot-canvas Save) rely on the
// exact push/clipboard branch — a shipped regression on the plot side was the whole reason this
// helper exists. Pin the branches so the two callers can't drift apart.

// `copyText` is stubbed per-test — we do not exercise the actual clipboard API.
vi.mock('./clipboard', () => ({ copyText: vi.fn() }))
import { copyText } from './clipboard'

describe('shareClipboardPrompt', () => {
  it('returns the bare prompt when notes is empty/undefined', () => {
    expect(shareClipboardPrompt(undefined)).toBe('Read my shared frame in cecelia.')
    expect(shareClipboardPrompt('')).toBe('Read my shared frame in cecelia.')
  })
  it('appends notes on a new line prefixed with "User said"', () => {
    expect(shareClipboardPrompt('look at the T cells'))
      .toBe('Read my shared frame in cecelia.\nUser said: look at the T cells')
  })
})

describe('announceShareOutcome', () => {
  beforeEach(() => { vi.mocked(copyText).mockReset() })
  afterEach(() => { vi.mocked(copyText).mockReset() })

  it('on "sent" skips the clipboard entirely and reports delivery', async () => {
    const out = await announceShareOutcome('sent', 'unused')
    expect(out).toEqual({ kind: 'ok',
      message: 'Sent to your Claude session — check for the incoming message.' })
    expect(copyText).not.toHaveBeenCalled()
  })

  it('on non-"sent" copies the prompt and reports paste-ready when clipboard succeeded', async () => {
    vi.mocked(copyText).mockResolvedValue(true)
    const out = await announceShareOutcome('not_paired', 'my notes')
    expect(copyText).toHaveBeenCalledWith('Read my shared frame in cecelia.\nUser said: my notes')
    expect(out).toEqual({ kind: 'ok',
      message: 'Capture saved — prompt in your clipboard. Switch to Claude and paste.' })
  })

  it('falls back to a "copy manually" message when clipboard access fails', async () => {
    vi.mocked(copyText).mockResolvedValue(false)
    const out = await announceShareOutcome('fallback', undefined)
    expect(out.kind).toBe('ok')
    expect(out.message).toContain('copy manually')
    // manual line quotes the exact prompt the user must copy — no notes appended for undefined.
    expect(out.message).toContain('"Read my shared frame in cecelia."')
  })

  it('treats an unknown push value as clipboard fallback (opaque string)', async () => {
    vi.mocked(copyText).mockResolvedValue(true)
    const out = await announceShareOutcome('some_future_value', 'notes')
    expect(copyText).toHaveBeenCalled()
    expect(out.kind).toBe('ok')
  })
})

describe('shareFailMessage', () => {
  it('uses Error.message when the thrown value is an Error', () => {
    expect(shareFailMessage(new Error('boom'))).toBe('boom')
  })
  it('stringifies non-Error values', () => {
    expect(shareFailMessage('plain string')).toBe('plain string')
    expect(shareFailMessage(42)).toBe('42')
    expect(shareFailMessage(null)).toBe('null')
  })
})
