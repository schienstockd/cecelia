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

  it('offers notebook creation as a direction and names the write tools', () => {
    const p = buildChatPrompt('NRUBxU')
    expect(p).toContain('create_notebook')
    expect(p).toContain('get_repl_api')
    expect(p).toMatch(/edit and run/i)                 // Claude bootstraps; the user owns/iterates it
  })

  it('names the available-plots tool for suggesting visualizations', () => {
    expect(buildChatPrompt('NRUBxU')).toContain('get_available_plots')
  })

  // This prompt and _OBSERVER_RULES (app/src/ai/observer_prompt.jl) are hand-synced copies. create_chain
  // was added to the Julia one only, and the user found the gap in the prompt they pasted — an omitted
  // capability is a tool the assistant never offers. The Julia side has the mirror of this assertion.
  it('offers chain design and states that it cannot run one', () => {
    const p = buildChatPrompt('NRUBxU')
    expect(p).toContain('create_chain')
    expect(p).toContain('get_module_params')       // real param keys/ranges before authoring
    expect(p).toMatch(/cannot run it/i)            // designs, never launches
    expect(p).toMatch(/press Run/i)                // …and says whose job that is
    expect(p).toMatch(/five additive actions/i)    // the write count moved 4 → 5; keep it honest
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
