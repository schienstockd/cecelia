import { describe, it, expect } from 'vitest'
import { buildChatPrompt } from './chatHandoff'

describe('buildChatPrompt', () => {
  it('names the project (name + uid) and the MCP server', () => {
    const p = buildChatPrompt('NRUBxU', 'my-experiment')
    expect(p).toContain('my-experiment (NRUBxU)')
    expect(p).toContain('cecelia-observer')
  })

  it('falls back to the uid when no name', () => {
    expect(buildChatPrompt('NRUBxU')).toContain('project NRUBxU.')
  })

  it('is paste-and-run: no placeholder, no relative doc path, tells it not to self-setup', () => {
    const p = buildChatPrompt('NRUBxU')
    expect(p).not.toContain('<')            // no <describe what you need> placeholder
    expect(p).not.toContain('docs/')        // no unresolvable relative doc reference
    expect(p).toMatch(/do not try to install/i)   // explicitly: don't chase MCP setup
  })
})

// The prompt USED to carry the observer's whole rulebook — every tool name, the grouping discipline,
// the boards/chains rules, the opening menu. That knowledge now lives in the MCP server
// (mcp/cecelia_mcp/guidance.py: SERVER_INSTRUCTIONS + BRIEFING_GUIDANCE), which is what makes a bare
// "check my project in cecelia" work with nothing pasted. Re-growing it here would restore the exact
// bug the move fixed: a second copy of the tool list in a language that cannot import the first, which
// went stale twice without anyone noticing until Dominik read his own pasted prompt.
describe('buildChatPrompt — the rules live in the MCP server, not here', () => {
  const p = buildChatPrompt('NRUBxU', 'my-experiment')

  it('names no MCP tools', () => {
    // any `get_*` / `create_*` / `add_*` / `set_*` / `list_*` snake_case token is a tool name
    expect(p).not.toMatch(/\b(?:get|list|create|revise|add|set|append)_[a-z_]+\b/)
  })

  it('carries no working rules — those are the briefing\'s job', () => {
    expect(p).not.toMatch(/replicates|statUnit|press Run|which direction/i)
  })

  it('stays short enough that nobody has to read it', () => {
    expect(p.length).toBeLessThan(400)     // was ~5000
    expect(p).not.toContain('\n')          // one paragraph, one clipboard line
  })
})
