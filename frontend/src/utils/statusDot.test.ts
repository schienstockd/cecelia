import { describe, it, expect } from 'vitest'
import { dotClass, LOG_LEVEL_TONE, WS_STATUS_TONE } from './statusDot'
import STYLE from '../style.css?raw'
import PALETTE_JSON from '../../../share/console_palette.json?raw'

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

describe('cc-dot tones are the consoles\' CVD-safe swatches', () => {
  // share/console_palette.json is the one source; style.css cannot import it, so it is pinned here.
  const SWATCH: Record<string, string> = {
    ok: 'sky_blue', warn: 'orange', fail: 'vermillion', idle: 'grey', info: 'blue',
  }
  const swatches = JSON.parse(PALETTE_JSON).swatches as Record<string, string>

  it('each tone uses its swatch\'s hex', () => {
    for (const [tone, name] of Object.entries(SWATCH)) {
      const hex = new RegExp(`--cc-dot-${tone}:\\s*(#[0-9a-fA-F]{6})`).exec(STYLE)
      expect(hex?.[1]?.toUpperCase(), tone).toBe(swatches[name].toUpperCase())
      expect(STYLE).toMatch(new RegExp(`\\.cc-dot-${tone}\\s*\\{\\s*background:\\s*var\\(--cc-dot-${tone}\\)`))
    }
  })

  it('good is never a green — it sits beside fail', () => {
    expect(SWATCH.ok).not.toMatch(/green/)
  })
})
