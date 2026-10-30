import { describe, it, expect } from 'vitest'
import {
  CLAUDE_LOOP, CLAUDE_ACCESS, CLAUDE_PROVISIONAL, claudeAccess,
  CLAUDE_TERMINAL, CLAUDE_DATA_NOTICE, claudeChatCommand,
} from './claudeOverview'
import { isMultiSentence, isTooLong } from '../utils/uiCopy'

describe('claudeOverview content model', () => {
  // The loop is the one explanation of how Claude fits the work — the dialog, the guide and
  // user-docs/README.md all say it, so each step has to name the control it is about.
  it('is the four-step loop, each step naming its control', () => {
    expect(CLAUDE_LOOP.map(s => s.title)).toEqual(['Point at it', 'Ask Kiwi', 'Keep it', 'Go further'])
    expect(CLAUDE_LOOP[0].text).toMatch(/@ Add to Kiwi/)
    expect(CLAUDE_LOOP[0].text).toMatch(/Share/)
    expect(CLAUDE_LOOP[1].text).toMatch(/ref/)
    expect(CLAUDE_LOOP[2].text).toMatch(/Save/)
    expect(CLAUDE_LOOP[2].text).toMatch(/Blackboard/)        // the record is the product
    expect(CLAUDE_LOOP[3].text).toMatch(/Continue in terminal/)
    for (const s of CLAUDE_LOOP) {
      expect(isMultiSentence(s.text)).toBe(false)
      expect(isTooLong(s.text)).toBe(false)
    }
  })

  // The dialog is where someone goes to find out what Claude will do to their project. The limits
  // that make authoring safe — it designs but never launches, and never changes or deletes what the
  // user built — must be a row that neither way in can do.
  it('states that neither way in runs anything or changes the user’s work', () => {
    const none = CLAUDE_ACCESS.filter(r => !r.kiwi && !r.terminal).map(r => r.text).join(' ')
    expect(none).toMatch(/Runs a task or chain/)
    expect(none).toMatch(/changes or deletes/)
  })

  it('separates what only a terminal session can do', () => {
    const termOnly = CLAUDE_ACCESS.filter(r => !r.kiwi && r.terminal).map(r => r.text).join(' ')
    expect(termOnly).toMatch(/chain/)
    expect(termOnly).toMatch(/board/)
    expect(termOnly).toMatch(/notebook/)
    expect(termOnly).toMatch(/Marks cells/)
    // Kiwi only reads: no row lets Kiwi do something a terminal can't
    expect(CLAUDE_ACCESS.some(r => r.kiwi && !r.terminal)).toBe(false)
  })

  // RUBBER_DUCK_FIT_PLAN Decision 1 — the shipped guardrail digest is a proactive warning. Users
  // should be told it will speak up on recurring bad-tagged failures.
  it('says it warns on recurring bad-tagged failures, and that replies are provisional', () => {
    expect(CLAUDE_ACCESS.map(r => r.text).join(' ')).toMatch(/bad-tagged failure keeps coming back/)
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

describe('claudeAccess — hidden connectors', () => {
  it('drops rows that need a connector the user hid, and nothing else', () => {
    const shown = claudeAccess([])
    const hidden = claudeAccess(['LabArchives'])
    expect(JSON.stringify(shown)).toMatch(/LabArchives/)
    expect(JSON.stringify(hidden)).not.toMatch(/LabArchives/)
    expect(hidden.length).toBe(shown.length - 1)
  })

  it('an unknown connector name hides nothing', () => {
    expect(claudeAccess(['NotAThing'])).toEqual(claudeAccess([]))
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
