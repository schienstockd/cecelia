// Parse a [Cecelia] activity-digest block (markdown, lines led by ✅/⚠️/❌ — see qc.jl severity_symbol
// / lab_log_context.jl) for the sidebar badge. PURE → unit-tested. Only ⚠️/❌ are actionable; a purely
// ✅ digest appends to the log silently (no badge). See docs/todo/QC_OBSERVER_PLAN.md (B2).
import type { Severity } from './severity'

// Worst severity present in the block: ❌ → 'fail', else ⚠️ → 'warn', else 'ok'.
export function worstDigestLevel(md: string): Severity {
  if (md.includes('❌')) return 'fail'
  if (md.includes('⚠️')) return 'warn'
  return 'ok'
}

// The first actionable line (⚠️/❌), stripped of the leading symbol + list markers — the badge preview.
export function firstActionableLine(md: string): string {
  const line = md.split('\n').find(l => l.includes('❌') || l.includes('⚠️')) ?? ''
  return line.replace(/[✅⚠️❌]/gu, '').replace(/^[-*\s]+/, '').trim()
}

// The actionable lines (⚠️/❌) of a block, trimmed — the unit we diff to detect a NEW flag.
export function flagLines(md: string): string[] {
  return md.split('\n').map(l => l.trim()).filter(l => l.includes('❌') || l.includes('⚠️'))
}

// Flag lines in `cur` that weren't in `prev`. The rolling daily [Cecelia] block is regenerated on
// every capture and holds ALL of the day's flags, so a later CLEAN task rewrites the block without
// adding any new warning — badging off the block's worst level would then re-alert a standing flag on
// every task. Diffing the flag lines fixes that: only a genuinely new/changed ⚠️/❌ badges. `prev`
// empty (first capture, or after a reload) ⇒ every current flag counts as new (surfaced once).
export function newFlagLines(prev: string, cur: string): string[] {
  const before = new Set(flagLines(prev))
  return flagLines(cur).filter(l => !before.has(l))
}
