import { describe, it, expect } from 'vitest'
import { mcpRows, observerRowState, cliRow, ACCOUNT_CONNECTORS, CLAUDE_SETUP_GUIDE } from './mcpConnections'

describe('observerRowState', () => {
  it('only "current" is green', () => {
    expect(observerRowState('current').tone).toBe('ok')
    for (const s of ['missing', 'stale', 'shadowed']) {
      expect(observerRowState(s).tone).toBe('warn')
      // a warn state must say what to DO — a silent stale entry is the worst failure mode
      expect(observerRowState(s).detail.length).toBeGreaterThan(0)
    }
  })

  it('an unknown/empty state is idle, not green', () => {
    expect(observerRowState('').tone).toBe('idle')
    expect(observerRowState(undefined).tone).toBe('idle')
  })
})

describe('mcpRows', () => {
  it('maps the observer row through its registration state', () => {
    const rows = mcpRows([{ name: 'cecelia-observer', scope: 'user', ours: true }], 'current')
    const obs = rows.find(r => r.name === 'cecelia-observer')!
    expect(obs.tone).toBe('ok')
    expect(obs.kind).toBe('machine')
  })

  it('still lists the observer when it is absent from the config', () => {
    const rows = mcpRows([], '')
    const obs = rows.find(r => r.name === 'cecelia-observer')!
    expect(obs).toBeTruthy()
    expect(obs.tone).toBe('warn')          // absent = needs attention, never silently missing
  })

  it('reports a third-party server as registered, without claiming it is healthy', () => {
    const rows = mcpRows([{ name: 'some-other', scope: 'user', transport: 'stdio' }], 'current')
    const other = rows.find(r => r.name === 'some-other')!
    expect(other.label).toBe('registered')  // NOT "connected" — we never health-check it
    expect(other.tone).toBe('ok')
  })

  it('shows the local scope with its directory (local overrides user)', () => {
    const rows = mcpRows([{ name: 'x', scope: 'local', dir: '/tmp/proj' }], 'current')
    expect(rows.find(r => r.name === 'x')!.scope).toContain('/tmp/proj')
  })

  it('lists account connectors but NEVER gives them a green/red dot', () => {
    const rows = mcpRows([], 'current')
    const la = rows.find(r => r.name === 'LabArchives')!
    expect(la).toBeTruthy()                 // listed so people discover it exists
    expect(la.kind).toBe('account')
    expect(la.tone).toBe('idle')            // we cannot see claude.ai account connectors
    expect(la.dismissable).toBe(true)       // plenty of institutes have no LabArchives
    expect(la.detail).toMatch(/\/mcp/)      // says where setup actually happens
  })

  it('keeps every row to ONE line: a short detail, with the why in the tooltip', () => {
    // The row is a status line, not an explanation — a long detail wrapped it onto a second line and
    // made the panel look crowded (docs/ui/COPY.md). Pin it so it can't creep back.
    for (const r of mcpRows([{ name: 'other', scope: 'user', transport: 'stdio' }], 'stale')) {
      expect(r.detail.length).toBeLessThanOrEqual(34)
      if (r.tone === 'warn') expect(r.hint.length).toBeGreaterThan(r.detail.length)
    }
  })

  it('a dismissed account connector is gone entirely', () => {
    const rows = mcpRows([], 'current', ['LabArchives'])
    expect(rows.find(r => r.name === 'LabArchives')).toBeUndefined()
  })

  it('every account connector is dismissable — none of them is detectable', () => {
    const rows = mcpRows([], 'current')
    for (const a of ACCOUNT_CONNECTORS) {
      const row = rows.find(r => r.name === a.name)!
      expect(row.dismissable).toBe(true)
      expect(row.tone).toBe('idle')
    }
  })
})

describe('cliRow — the Claude Code precondition', () => {
  it('leads the list, so a missing CLI is read before the rows that depend on it', () => {
    const rows = mcpRows([], 'current', [], { available: false })
    expect(rows[0]!.name).toBe('Claude Code')
    expect(rows[0]!.kind).toBe('cli')
  })

  it('carries the setup guide link only when there is something to fix', () => {
    expect(cliRow(false).href).toBe(CLAUDE_SETUP_GUIDE)     // not installed
    expect(cliRow(true, true).href).toBe(CLAUDE_SETUP_GUIDE) // installed, not logged in
    expect(cliRow(true).href).toBeUndefined()                // ready → no link to chase
  })

  it('separates "not installed" from "not logged in" — different fixes', () => {
    expect(cliRow(false).label).toBe('not detected')
    expect(cliRow(true, true).label).toBe('not logged in')
    expect(cliRow(true).tone).toBe('ok')
    expect(cliRow(false).tone).toBe('warn')
    expect(cliRow(true, true).tone).toBe('warn')
  })

  it('is omitted when the caller has no CLI state to report', () => {
    expect(mcpRows([], 'current').some(r => r.kind === 'cli')).toBe(false)
  })
})
