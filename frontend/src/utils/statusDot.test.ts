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
