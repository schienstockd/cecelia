// Manual segmentation label correction — the frontend half of `segment.correct`
// (docs/todo/CORRECTION_PLAN.md, P2). Parallel to `lib/trackCorrection.ts` but much smaller: two
// ops today (Merge, Remove), frame-local by design (Decision 6b), no lineage math, no worklist
// engine (label triage is Phase 3 Review).
//
// Everything here is PURE. The cockpit builds ops from the current viewer selection, hands them to
// `stores/labelOpsQueue.ts`, and `utils/labelOpsRun.ts` submits the whole queue as one
// `segment.correct` (or `segment.correct_measures`) task run. Same shape and rules the Julia
// engine takes (`app/src/label_correction.jl`): if these two disagree about what's a legal op, the
// task's ParamValidationError catches it — but a UI that offered impossible ops in the first place
// is a bug, so the validators here are the shared contract.

/** A single label correction op — matches `LABEL_OP_KINDS` in `app/src/label_correction.jl`. */
export interface LabelOp {
  /** the op kind: 'label.merge' or 'label.remove' */
  op: 'label.merge' | 'label.remove'
  /** timepoint index the op applies to (>=0) */
  t: number
  /** the label ids the op touches (>=1; 0 is background and cannot be an input) */
  ids: number[]
  /** merge only: which id survives — must be one of `ids`. */
  into?: number
}

/** Everything the surface needs to render one op as a button — same shape as manualActions'. */
export interface LabelAction {
  key: 'merge' | 'remove'
  label: string
  op: LabelOp | null
  /** why the op is blocked, or '' when it is valid. A blocked action stays VISIBLE with its
   *  reason on hover (see docs/UI.md → "A refusal that names the reason"). */
  blocked: string
}

// ── Op builders ─────────────────────────────────────────────────────────────────

/**
 * Build a merge op from a set of picked labels at a given frame.
 *
 * `into` defaults to `Math.min(...ids)` — the smallest surviving id — which is deterministic and
 * matches the "smallest id wins" convention users tend to expect. Callers that WANT a specific
 * survivor (a Review mode where the user clicks one label first) pass it explicitly.
 */
export function buildMergeOp(t: number, ids: readonly number[], into?: number): LabelOp | null {
  const uniq = Array.from(new Set(ids.map(x => Math.floor(Number(x)))))
    .filter(id => Number.isFinite(id) && id >= 1)
    .sort((a, b) => a - b)
  if (uniq.length < 2) return null
  const target = into !== undefined && uniq.includes(into) ? into : uniq[0]
  return { op: 'label.merge', t: Math.floor(t), ids: uniq, into: target }
}

/** Build a remove op from a set of picked labels at a given frame. */
export function buildRemoveOp(t: number, ids: readonly number[]): LabelOp | null {
  const uniq = Array.from(new Set(ids.map(x => Math.floor(Number(x)))))
    .filter(id => Number.isFinite(id) && id >= 1)
    .sort((a, b) => a - b)
  if (!uniq.length) return null
  return { op: 'label.remove', t: Math.floor(t), ids: uniq }
}

// ── Action derivation for the cockpit tool row ──────────────────────────────────

/**
 * Turn a viewer selection at frame `t` into the two cockpit actions (Merge, Remove).
 *
 * Empty selection: both actions blocked, message tells the user what to do.
 * One id: Merge blocked ("need at least two"), Remove enabled.
 * Two+ ids: both enabled.
 *
 * Same rule as trackCorrection.manualActions: an action stays VISIBLE when blocked, with the
 * reason on hover — a button that vanishes teaches nothing (docs/UI.md → refusal-with-reason).
 */
export function labelActions(t: number, ids: readonly number[]): LabelAction[] {
  const uniq = Array.from(new Set(ids.map(x => Math.floor(Number(x)))))
    .filter(id => Number.isFinite(id) && id >= 1)
    .sort((a, b) => a - b)
  const merge = buildMergeOp(t, uniq)
  const remove = buildRemoveOp(t, uniq)
  return [
    { key: 'merge',  label: 'Merge',  op: merge,
      blocked: !uniq.length ? 'Pick two or more labels in the viewer first'
             : uniq.length === 1 ? 'Pick a second label to merge into this one' : '' },
    { key: 'remove', label: 'Remove', op: remove,
      blocked: !uniq.length ? 'Pick at least one label in the viewer first' : '' },
  ]
}

// ── Descriptions (tooltip + log line) ───────────────────────────────────────────

/** Short button label — used in the cockpit tool row alongside the icon. */
export function opLabel(op: LabelOp): string {
  return op.op === 'label.merge' ? 'Merge' : 'Remove'
}

/** Human sentence for a tooltip / log line — same voice as `trackCorrection.opDescription`. */
export function opDescription(op: LabelOp): string {
  const t = `frame ${op.t}`
  if (op.op === 'label.merge') {
    const others = op.ids.filter(id => id !== op.into)
    return others.length === 1
      ? `Merge label ${others[0]} into ${op.into} at ${t}`
      : `Merge labels ${others.join(', ')} into ${op.into} at ${t}`
  }
  return op.ids.length === 1
    ? `Remove label ${op.ids[0]} at ${t}`
    : `Remove labels ${op.ids.join(', ')} at ${t}`
}

// ── Queue helper: undo (mirrors trackCorrection.undoLast) ───────────────────────

/** Return a copy of `pending` with the last op removed. Immutable — safe with reactive refs. */
export function undoLast(pending: readonly LabelOp[]): LabelOp[] {
  return pending.slice(0, -1)
}
