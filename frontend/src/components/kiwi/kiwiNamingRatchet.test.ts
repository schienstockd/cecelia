import { describe, it, expect } from 'vitest'

// Kiwi is the PANEL, not the assistant — see docs/todo/KIWI_PLAN.md → Decision 12
// (naming discipline enforcement). Kiwi's job is to display + control whatever assistant is
// paired, not to be one. Every file in `components/kiwi/` must use provider-neutral wording
// ("the paired assistant", "your assistant session") rather than the specific vendor name.
//
// Sanctioned exceptions:
//   - This test file itself (its whole point is to talk about the rule).
//   - A comment block that explicitly cites this rule.
//   - The `cecelia-observer` MCP server name and the `CLAUDE_CODE_MESSAGING_*` env vars if a
//     future Kiwi row needs to name them literally (add an explicit `// ratchet: cite-technical`
//     comment on the same line so a reviewer can see the exception was deliberate).
//
// If this test fails: rename the offending copy to provider-neutral wording. If the mention is
// genuinely technical and cannot be renamed, add `// ratchet: cite-technical` on that line.
const SOURCES = import.meta.glob('./**/*.{vue,ts}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

const SELF = new Set(['./kiwiNamingRatchet.test.ts'])

const STRIP_COMMENTS = (src: string): string =>
  src
    .replace(/\/\*[\s\S]*?\*\//g, '')       // block comments
    .replace(/<!--[\s\S]*?-->/g, '')        // HTML comments (Vue templates)
    .split('\n')
    .filter(line => !/^\s*\/\//.test(line)) // whole-line // comments
    .map(line => line.replace(/\/\/.*$/, '')) // trailing // comments
    .map(line => /ratchet:\s*cite-technical/.test(line) ? '' : line)
    .join('\n')

describe('kiwi/ naming ratchet', () => {
  it('no "Claude" literal outside sanctioned exceptions', () => {
    const offenders: string[] = []
    for (const [path, src] of Object.entries(SOURCES)) {
      if (SELF.has(path)) continue
      const stripped = STRIP_COMMENTS(src)
      if (/\bClaude\b/.test(stripped)) offenders.push(path)
    }
    expect(offenders).toEqual([])
  })

  it('covers at least one file (glob sanity)', () => {
    // If a refactor moves KiwiCockpit.vue out of this dir the ratchet silently passes; guard it.
    const withoutSelf = Object.keys(SOURCES).filter(p => !SELF.has(p))
    expect(withoutSelf.length).toBeGreaterThan(0)
  })
})
