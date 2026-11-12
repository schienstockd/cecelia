import { describe, it, expect } from 'vitest'
import { dotClass, LOG_LEVEL_TONE, WS_STATUS_TONE } from './statusDot'
import STYLE from '../style.css?raw'

describe('cc-dot tones', () => {
  it('maps a tone to its modifier, and nothing to no modifier', () => {
    expect(dotClass('ok')).toBe('cc-dot-ok')
    expect(dotClass(null)).toBe('')
    expect(dotClass(undefined)).toBe('')
  })

  it('every tone any surface can produce has a rule in style.css', () => {
    const tones = new Set([...Object.values(LOG_LEVEL_TONE), ...Object.values(WS_STATUS_TONE),
                           'ok', 'warn', 'fail', 'idle', 'info'])
    for (const t of tones) expect(STYLE).toContain(`.${dotClass(t as never)} `)
  })

  it('a log error and a dropped sign-in read as failures', () => {
    expect(LOG_LEVEL_TONE.error).toBe('fail')
    expect(WS_STATUS_TONE.signedOut).toBe('fail')
  })
})

describe('cc-dot tones are the severity palette', () => {
  // One "ok" colour in the app: a dot follows the scheme and inherits utils/cvd.test.ts's guarantee.
  const ALIAS: Record<string, string> = { ok: '--cc-sev-ok', warn: '--cc-sev-warn', fail: '--cc-sev-fail', info: '--cc-active' }

  it('each tone aliases its severity token', () => {
    for (const [tone, token] of Object.entries(ALIAS)) {
      expect(STYLE, tone).toMatch(new RegExp(`--cc-dot-${tone}:\\s*var\\(${token}\\)`))
      expect(STYLE).toMatch(new RegExp(`\\.cc-dot-${tone}\\s*\\{\\s*background:\\s*var\\(--cc-dot-${tone}\\)`))
    }
  })
})
