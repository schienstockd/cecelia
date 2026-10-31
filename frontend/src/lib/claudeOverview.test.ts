import { describe, it, expect } from 'vitest'
import {
  KIWI_LOOP, KIWI_MODE, TERMINAL_MODE, CLAUDE_BRIDGE, CLAUDE_PROVISIONAL,
  CLAUDE_TERMINAL, CLAUDE_DATA_NOTICE, claudeChatCommand, type Mode,
} from './claudeOverview'
import { isMultiSentence, isTooLong } from '../utils/uiCopy'

const lines = (m: Mode) => [m.use, m.start, ...(m.note ? [m.note] : []), ...m.only, ...m.ask]
const only = (m: Mode) => m.only.join(' ')

describe('claudeOverview content model', () => {
  // Kiwi's steps are what the guide walks, one per control, so each has to name the control.
  it('is Kiwi’s three steps, each naming its control', () => {
    expect(KIWI_LOOP.map(s => s.title)).toEqual(['Point at it', 'Ask Kiwi', 'Keep it'])
    expect(KIWI_LOOP[0].text).toMatch(/@ Add to Kiwi/)
    expect(KIWI_LOOP[0].text).toMatch(/Share/)
    expect(KIWI_LOOP[1].text).toMatch(/ref/)
    expect(KIWI_LOOP[2].text).toMatch(/Save/)
    expect(KIWI_LOOP[2].text).toMatch(/Blackboard/)        // the record is the product
  })

  // Two modes, not two steps: some people go straight to Claude Code. No Kiwi step may hand off to
  // the terminal (that was the old "Go further" step 4), and the bridge has to say either comes first.
  it('presents Claude Code as the other mode, not a step after Kiwi', () => {
    expect(KIWI_LOOP.some(s => /terminal/i.test(s.text))).toBe(false)
    expect(TERMINAL_MODE.start).toMatch(/Start in terminal/)  // the direct entry, no Kiwi turn needed
    expect(CLAUDE_BRIDGE).toMatch(/Either can come first/)
    expect(CLAUDE_BRIDGE).toMatch(/Continue in terminal/)
  })

  // Kiwi's case is what only it gives: answers checked against the data, a guarantee it changes
  // nothing, no terminal. Its example asks are questions — never something for it to make.
  it('leads Kiwi with what only it gives — checked, changes nothing, no terminal', () => {
    expect(KIWI_MODE.use).toMatch(/check/)
    for (const re of [/Checks every claim/, /Can’t change anything/, /No terminal/]) expect(only(KIWI_MODE)).toMatch(re)
    for (const q of KIWI_MODE.ask) expect(q.endsWith('?')).toBe(true)
  })

  // What only a session can do — the reason to open one. The limit is Cecelia's tools, not Claude
  // Code itself (its own Bash/Edit sit outside Cecelia), so it must name the tools, not promise more.
  it('names what only Claude Code does, and bounds it by Cecelia’s tools', () => {
    for (const re of [/Marks cells/, /chains/, /boards/, /notebooks/, /gates/, /lab-log/]) {
      expect(only(TERMINAL_MODE)).toMatch(re)
    }
    expect(TERMINAL_MODE.note).toBe('You run what it builds')   // no promise its own shell can't keep
  })

  // The cards sit side by side in Kiwi's `?` dialog, ~300 px each: a line that wraps there doubles the
  // card. The line beside the schematic is narrower still.
  it('keeps every card line on one line', () => {
    for (const m of [KIWI_MODE, TERMINAL_MODE]) {
      expect(m.use.length, m.use).toBeLessThanOrEqual(34)
      for (const t of lines(m).slice(1)) expect(t.length, t).toBeLessThanOrEqual(42)
    }
  })

  it('keeps every line to one short sentence', () => {
    for (const t of [CLAUDE_BRIDGE, ...lines(KIWI_MODE), ...lines(TERMINAL_MODE), ...KIWI_LOOP.map(s => s.text)]) {
      expect(isMultiSentence(t)).toBe(false)
      expect(isTooLong(t)).toBe(false)
    }
    expect(CLAUDE_PROVISIONAL).toMatch(/provisional/i)
  })

  it('builds the fallback command from the REAL config path, or nothing at all', () => {
    expect(claudeChatCommand('/home/u/.cecelia/observer-mcp.json'))
      .toBe('claude --mcp-config /home/u/.cecelia/observer-mcp.json')
    expect(claudeChatCommand('C:\\Users\\First Last\\.cecelia\\observer-mcp.json'))
      .toBe('claude --mcp-config "C:\\Users\\First Last\\.cecelia\\observer-mcp.json"')
    // No path yet → empty, so the dialog renders nothing. A placeholder here would be a command a
    // user copies verbatim into a terminal and gets an error from — the exact failure to design out.
    expect(claudeChatCommand('')).toBe('')
  })

  it('keeps the terminal-setup labels short enough for a compact button row', () => {
    for (const s of Object.values(CLAUDE_TERMINAL)) expect(s.length).toBeLessThan(15)
  })
})

describe('CLAUDE_DATA_NOTICE', () => {
  // A standing line in Kiwi: it has to stay inside the copy budget to stay read (docs/ui/COPY.md)
  it('is one short phrase with a one-line action on hover', () => {
    expect(CLAUDE_DATA_NOTICE.short.split(/\s+/).length).toBeLessThanOrEqual(10)
    for (const t of [CLAUDE_DATA_NOTICE.short, CLAUDE_DATA_NOTICE.detail]) {
      expect(isMultiSentence(t)).toBe(false)
      expect(isTooLong(t)).toBe(false)
      expect(t.endsWith('.')).toBe(false)
    }
  })

  it('names who receives the data', () => {
    expect(CLAUDE_DATA_NOTICE.short).toContain('Anthropic')
  })
})
