import { describe, it, expect } from 'vitest'
import { AI_SURFACES, CAPABILITIES, HIDDEN_TOOLS, TOOL_GROUPS, aiDialogRows, capabilityGroups } from './claudeTools'
import { isTooLong } from '../utils/uiCopy'

// The two allow-lists, read from source with Vite's raw glob (this tsconfig has no node types — see
// tasks/taskRunnerDrafts.test.ts). The observer's tools are what server.py registers; Kiwi's are
// KIWI_READ_TOOLS in kiwi_turn.jl.
const raw = (g: Record<string, string>) => Object.values(g)[0] ?? ''
const SERVER = raw(import.meta.glob('../../../mcp/cecelia_mcp/server.py', { query: '?raw', import: 'default', eager: true }))
// …plus the gating toolset server.py registers from gating_views.py (`register_gating_tools`)
const GATING = raw(import.meta.glob('../../../mcp/cecelia_mcp/gating_views.py', { query: '?raw', import: 'default', eager: true }))
const KIWI_JL = raw(import.meta.glob('../../../api/src/kiwi_turn.jl', { query: '?raw', import: 'default', eager: true }))

// The same registrar pattern api/test/suite/kiwi_turn.jl uses: `_tool` / `_tool_if(…)` / `_gui_tool`,
// then any stacked decorators (`@with_doc`), then the def.
const SERVER_TOOLS = [...SERVER.matchAll(/@_(?:gui_)?tool(?:_if\([^\n]*)?\s*\n(?:@[^\n]*\n)*def ([a-z_]+)\(/g)].map(m => m[1])
const GATING_TOOLS = [...GATING.matchAll(/\n {4}@tool\s*\n(?: {4}@[^\n]*\n)* {4}def ([a-z_]+)\(/g)].map(m => m[1])
const REGISTERED = new Set([...SERVER_TOOLS, ...GATING_TOOLS])
const KIWI_BLOCK = KIWI_JL.match(/const KIWI_READ_TOOLS = \[([\s\S]*?)\n\]/)?.[1] ?? ''
const KIWI_TOOLS = new Set([...KIWI_BLOCK.replace(/#[^\n]*/g, '').matchAll(/"([a-z_]+)"/g)].map(m => m[1]))

describe('claudeTools — pinned to the real allow-lists', () => {
  it('found both sources', () => {
    // A glob or pattern that matched nothing would make every check below vacuously pass.
    expect(SERVER_TOOLS.length).toBe((SERVER.match(/^@_(?:gui_)?tool/gm) ?? []).length)
    expect(GATING_TOOLS.length).toBe((GATING.match(/^ {4}@tool$/gm) ?? []).length)
    expect(SERVER).toContain('register_gating_tools(_tool, _client)')
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

// ── The AI switch's registry, pinned to the backend's refusal points ─────────────────────────────
const API_SRC = import.meta.glob('../../../api/src/*.jl', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const APP_AI = import.meta.glob('../../../app/src/**/*.jl', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const rel = (p: string) => p.replace(/^(\.\.\/)+/, '')
const SERVER_JL = API_SRC['../../../api/src/server.jl'] ?? ''

describe('AI_SURFACES — every model path is registered and refused with AI off', () => {
  it('found the sources', () => {
    expect(Object.keys(API_SRC).length).toBeGreaterThan(50)
    expect(SERVER_JL).toContain('_ai_gated(handler::Function)')
  })

  it('lists exactly the routes server.jl wraps in _ai_gated', () => {
    const gated = [...SERVER_JL.matchAll(/"(\/api\/[^"]+)"\s*=>\s*_ai_gated\(/g)].map(m => m[1])
    expect(gated.length).toBeGreaterThan(0)
    const listed = AI_SURFACES.filter(s => s.enforcedBy === 'route').flatMap(s => s.routes)
    expect([...new Set(listed)].sort()).toEqual([...new Set(gated)].sort())
  })

  it('has a turn row for every file that starts a model turn', () => {
    const callers = Object.entries(API_SRC)
      .filter(([, src]) => src.split('\n').some(l => /\bkiwi_start_turn\(/.test(l) && !/^\s*#|function kiwi_start_turn\(/.test(l)))
      .map(([p]) => rel(p)).sort()
    expect(callers).toEqual(AI_SURFACES.filter(s => s.enforcedBy === 'turn').map(s => s.source).sort())
  })

  it('spawns `claude` only through the gated seams', () => {
    const spawners = Object.entries({ ...API_SRC, ...APP_AI })
      .filter(([, src]) => /_agent_spawn_cmd\(`|_agent_spawn_cmd\(_build/.test(src))
      .map(([p]) => rel(p)).sort()
    // agent_runner.jl: the turn (behind kiwi_start_turn) and short CLI commands; claude_auth.jl: login
    // (behind the gated /api/kiwi/profiles/login). Anything new must be gated, then added here.
    expect(spawners).toEqual(['app/src/ai/agent_runner.jl', 'app/src/ai/claude_auth.jl'])
  })

  it('every surface source exists, ids are unique, and the dialog has something to say', () => {
    for (const s of AI_SURFACES) expect(Object.keys({ ...API_SRC }).map(rel)).toContain(s.source)
    expect(new Set(AI_SURFACES.map(s => s.id)).size).toBe(AI_SURFACES.length)
    expect(aiDialogRows().length).toBeGreaterThan(2)
  })
})
