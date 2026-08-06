import { describe, it, expect } from 'vitest'
import {
  CLAUDE_ENTRY_POINTS, CLAUDE_CAPABILITIES, CLAUDE_EXAMPLES,
  CLAUDE_TERMINAL, claudeChatCommand,
} from './claudeOverview'

describe('claudeOverview content model', () => {
  it('has the two entry points (Ask / Chat), each with how-to steps', () => {
    const names = CLAUDE_ENTRY_POINTS.map(e => e.name)
    expect(names).toEqual(['Ask Claude', 'Chat to Claude'])
    for (const e of CLAUDE_ENTRY_POINTS) {
      expect(e.steps.length).toBeGreaterThanOrEqual(2)   // it's a how-to, not just a blurb
      expect(e.icon).toMatch(/^pi-/)
    }
  })

  it('covers sees / suggests / creates / cant, with the limits set apart', () => {
    const byKey = Object.fromEntries(CLAUDE_CAPABILITIES.map(g => [g.key, g]))
    expect(Object.keys(byKey).sort()).toEqual(['cant', 'creates', 'sees', 'suggests'])
    expect(byKey.cant.tone).toBe('muted')                // the "can't" group reads as recessive
    expect(byKey.creates.items.join(' ')).toMatch(/notebook/i)   // notebooks are the headline "create"
    expect(byKey.creates.items.join(' ')).toMatch(/chain/i)      // …and chains, since it can author one
    for (const g of CLAUDE_CAPABILITIES) expect(g.items.length).toBeGreaterThan(0)
  })

  // The dialog is where someone goes to find out what Claude will do to their project, so the "Can't"
  // list has to state the two limits that make chain authoring safe — it designs but never launches,
  // and it can't overwrite what the user built. A stale reassurance here is worse than none.
  it('states that it cannot run what it builds, or overwrite the user’s work', () => {
    const cant = CLAUDE_CAPABILITIES.find(g => g.key === 'cant')!.items.join(' ')
    expect(cant).toMatch(/run anything|cannot run|can't run/i)
    expect(cant).toMatch(/chain/i)
    expect(cant).toMatch(/overwrite|rename|delete/i)
  })

  it('offers example prompts spanning QC → notebook → chain', () => {
    expect(CLAUDE_EXAMPLES.length).toBeGreaterThanOrEqual(3)
    expect(CLAUDE_EXAMPLES.join(' ')).toMatch(/notebook/i)
    expect(CLAUDE_EXAMPLES.join(' ')).toMatch(/chain/i)
  })

  it('builds the fallback command from the REAL config path, or nothing at all', () => {
    expect(claudeChatCommand('/home/u/.cecelia/observer-mcp.json'))
      .toBe('claude --mcp-config /home/u/.cecelia/observer-mcp.json')
    // No path yet → empty, so the dialog renders nothing. A placeholder here would be a command a
    // user copies verbatim into a terminal and gets an error from — the exact failure to design out.
    expect(claudeChatCommand('')).toBe('')
  })

  it('offers one-click terminal setup, not an instruction to register anything', () => {
    expect(CLAUDE_TERMINAL.note).toMatch(/no setup/i)
    expect(CLAUDE_TERMINAL.action.length).toBeLessThan(30)      // a button label, not a sentence
    expect(CLAUDE_TERMINAL.done).toMatch(/claude/)             // tells them what to type next
    for (const s of Object.values(CLAUDE_TERMINAL)) expect(s.length).toBeLessThan(140)
  })
})
