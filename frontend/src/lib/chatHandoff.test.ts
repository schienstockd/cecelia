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

  it('names the board + attribute tools, and says to check them before proposing a figure', () => {
    const p = buildChatPrompt('NRUBxU')
    expect(p).toContain('get_analysis_boards')     // don't rebuild a board the user already has
    expect(p).toContain('get_image_attributes')    // the axes a cross-image comparison can group by
    expect(p).toContain('not four replicates')     // the discipline, not just the tool name
  })

  it('names the available-plots tool for suggesting visualizations', () => {
    expect(buildChatPrompt('NRUBxU')).toContain('get_available_plots')
  })

  // A capability the OPENING MENU doesn't list is one the user never gets offered, even when the
  // prompt describes it further down: add_analysis_board was named in the write list and in its own
  // paragraph, and the session still opened with six directions and no board among them — the user
  // said so ("it doesn't offer to build a board from the init"). The menu is its own surface; the
  // write count and the tool docs do not cover it.
  it('offers board building among the OPENING directions, not just further down', () => {
    const p = buildChatPrompt('NRUBxU')
    const menu = p.slice(p.indexOf('which direction'))
    expect(menu).toContain('add_analysis_board')
    expect(menu).toMatch(/board of plots/i)
    // the other artefacts stay on the menu beside it
    expect(menu).toContain('get_repl_api')          // notebook
    expect(menu).toMatch(/design a chain/i)         // chain
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
    expect(p).toMatch(/six additive actions/i)     // the write count moved 5 → 6; keep it honest
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
