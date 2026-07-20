import { describe, it, expect } from 'vitest'
import {
  CLAUDE_ENTRY_POINTS, CLAUDE_CAPABILITIES, CLAUDE_EXAMPLES,
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
    for (const g of CLAUDE_CAPABILITIES) expect(g.items.length).toBeGreaterThan(0)
  })

  it('offers example prompts spanning QC → notebook', () => {
    expect(CLAUDE_EXAMPLES.length).toBeGreaterThanOrEqual(3)
    expect(CLAUDE_EXAMPLES.join(' ')).toMatch(/notebook/i)
  })
})
