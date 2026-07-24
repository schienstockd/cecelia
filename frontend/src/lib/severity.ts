// Canonical severity model for QC / traffic-light state (ok/warn/fail) — the ONE place these are
// defined, so the image table, lab log, cohort summaries and badge never disagree. For the FIVE-state
// task/chain lifecycle (adds running/queued/cancelled), see the sibling lib/taskStatus.ts, which reuses
// this palette for its done/failed lights.
//
// Colour-blind-safe by construction: the hues (CSS --cc-sev-*) are the dataviz status palette,
// validated for CVD separation, but colour is NEVER the sole cue — every severity ships with a
// SHAPE-DISTINCT icon + a text label (WCAG 1.4.1). That is why the lab-log emoji are ✅/⚠️/❌
// (three distinct shapes) and NOT 🟢/🟡/🔴 (three identical circles that differ only in hue).
// See docs/todo/QC_OBSERVER_PLAN.md.

export type Severity = 'ok' | 'warn' | 'fail'

export interface SeverityStyle {
  icon: string    // PrimeVue icon class — the primary (non-colour) channel
  color: string   // CSS var reference into the validated severity palette
  emoji: string   // shape-distinct glyph for markdown/lab-log text
  label: string   // text label — colour is never alone
}

export const SEVERITY: Record<Severity, SeverityStyle> = {
  ok:   { icon: 'pi-check-circle',         color: 'var(--cc-sev-ok)',   emoji: '✅', label: 'OK' },
  warn: { icon: 'pi-exclamation-triangle', color: 'var(--cc-sev-warn)', emoji: '⚠️', label: 'Warning' },
  fail: { icon: 'pi-times-circle',         color: 'var(--cc-sev-fail)', emoji: '❌', label: 'Failed' },
}

const RANK: Record<Severity, number> = { ok: 0, warn: 1, fail: 2 }

// Roll several severities up to the worst — the image-table indicator and the lab-log badge both
// reduce a set of findings/outcomes to a single light. Empty ⇒ 'ok'.
export function worstSeverity(sevs: Iterable<Severity>): Severity {
  let worst: Severity = 'ok'
  for (const s of sevs) if (RANK[s] > RANK[worst]) worst = s
  return worst
}

// Map QC finding levels (qc.jl: "info" | "warn") + a run-failure flag to a severity. qc.jl is
// advisory (never "error"), so 🔴/'fail' comes from run status, composed here — not from a qc level.
// "info" does not raise the light above 'ok'. See the traffic-light table in QC_OBSERVER_PLAN.md.
export function severityFor(opts: { levels?: readonly string[]; failed?: boolean }): Severity {
  if (opts.failed) return 'fail'
  return (opts.levels ?? []).some(l => l === 'warn') ? 'warn' : 'ok'
}
