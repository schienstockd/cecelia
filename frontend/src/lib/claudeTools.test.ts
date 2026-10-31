import { describe, it, expect } from 'vitest'
import { CAPABILITIES, HIDDEN_TOOLS, TOOL_GROUPS, capabilityGroups } from './claudeTools'
import { isTooLong } from '../utils/uiCopy'

// The two allow-lists, read from source with Vite's raw glob (this tsconfig has no node types — see
// tasks/taskRunnerDrafts.test.ts). The observer's tools are what server.py registers; Kiwi's are
// KIWI_READ_TOOLS in kiwi_turn.jl.
const raw = (g: Record<string, string>) => Object.values(g)[0] ?? ''
const SERVER = raw(import.meta.glob('../../../mcp/cecelia_mcp/server.py', { query: '?raw', import: 'default', eager: true }))
const KIWI_JL = raw(import.meta.glob('../../../api/src/kiwi_turn.jl', { query: '?raw', import: 'default', eager: true }))

// The same registrar pattern api/test/suite/kiwi_turn.jl uses: `_tool` / `_tool_if(…)` / `_gui_tool`,
// then any stacked decorators (`@with_doc`), then the def.
const REGISTERED = new Set([...SERVER.matchAll(/@_(?:gui_)?tool(?:_if\([^\n]*)?\s*\n(?:@[^\n]*\n)*def ([a-z_]+)\(/g)].map(m => m[1]))
const KIWI_BLOCK = KIWI_JL.match(/const KIWI_READ_TOOLS = \[([\s\S]*?)\n\]/)?.[1] ?? ''
const KIWI_TOOLS = new Set([...KIWI_BLOCK.replace(/#[^\n]*/g, '').matchAll(/"([a-z_]+)"/g)].map(m => m[1]))

describe('claudeTools — pinned to the real allow-lists', () => {
  it('found both sources', () => {
    // A glob or pattern that matched nothing would make every check below vacuously pass.
    expect(REGISTERED.size).toBe((SERVER.match(/^@_(?:gui_)?tool/gm) ?? []).length)
    expect(REGISTERED.size).toBeGreaterThan(40)
    expect(KIWI_TOOLS.size).toBeGreaterThan(20)
  })

  it('words every registered tool exactly once — a new tool fails here until it is listed', () => {
    const listed = [...CAPABILITIES.flatMap(c => c.tools), ...Object.keys(HIDDEN_TOOLS)]
    expect(new Set(listed).size).toBe(listed.length)
    expect([...REGISTERED].filter(t => !listed.includes(t)).sort()).toEqual([])
    expect(listed.filter(t => !REGISTERED.has(t)).sort()).toEqual([])
  })

  it('ticks Kiwi exactly where its allow-list has every tool behind the line', () => {
    for (const c of CAPABILITIES.filter(c => c.tools.length)) {
      const allKiwi = c.tools.every(t => KIWI_TOOLS.has(t))
      const noneKiwi = c.tools.every(t => !KIWI_TOOLS.has(t))
      expect(allKiwi || noneKiwi, `${c.label}: mixes Kiwi and terminal-only tools — split it`).toBe(true)
      expect(c.kiwi, c.label).toBe(allKiwi)
      expect(c.terminal, c.label).toBe(true)    // every observer tool reaches a terminal session
    }
  })

  it('keeps every line short and every group populated', () => {
    for (const c of CAPABILITIES) expect(isTooLong(c.label), c.label).toBe(false)
    expect(capabilityGroups().map(g => g.key)).toEqual(TOOL_GROUPS.map(g => g.key))
  })
})
