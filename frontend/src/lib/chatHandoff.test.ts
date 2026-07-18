import { describe, it, expect } from 'vitest'
import { buildChatPrompt } from './chatHandoff'

describe('buildChatPrompt', () => {
  it('names the project (name + uid) and points at the MCP tools', () => {
    const p = buildChatPrompt('NRUBxU', 'my-experiment')
    expect(p).toContain('my-experiment (NRUBxU)')
    expect(p).toContain('cecelia-observer MCP')
    expect(p).toContain('get_cohort_qc')
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
