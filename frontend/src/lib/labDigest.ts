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
