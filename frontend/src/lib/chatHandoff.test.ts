import { describe, it, expect } from 'vitest'
import { buildChatPrompt } from './chatHandoff'

describe('buildChatPrompt', () => {
  it('names the project (name + uid) and points at the full MCP toolset', () => {
    const p = buildChatPrompt('NRUBxU', 'my-experiment')
    expect(p).toContain('my-experiment (NRUBxU)')
    expect(p).toContain('cecelia-observer MCP')
    expect(p).toContain('get_cohort_qc')
    expect(p).toContain('get_measure_summary')   // the full analysis toolset, not just the QC subset
  })

  it('orients + asks for direction instead of diving straight into QC', () => {
    const p = buildChatPrompt('NRUBxU')
    expect(p).toMatch(/ask me which direction/i)   // the open question, not an auto-QC review
    expect(p).not.toMatch(/review my recent analysis activity and QC/i)
  })

  it('tells Claude to pull the session briefing first', () => {
    const p = buildChatPrompt('NRUBxU')
    expect(p).toContain('get_session_briefing')
    expect(p).toMatch(/get_session_briefing first/i)   // oriented before diving in
  })

  it('is paste-and-run: no placeholder, no relative doc path, tells it not to self-setup', () => {
    const p = buildChatPrompt('NRUBxU')
    expect(p).not.toContain('<')            // no <describe what you need> placeholder
    expect(p).not.toContain('docs/')        // no unresolvable relative doc reference
    expect(p).toMatch(/do not try to install/i)   // explicitly: don't chase MCP setup
  })

  it('falls back to the uid when no name', () => {
    expect(buildChatPrompt('NRUBxU')).toContain('project NRUBxU.')
  })
})
