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

/** A single label correction op — matches `LABEL_OP_KINDS` in `app/src/label_correction.jl`.
 *
 *  Three op shapes today. Merge/Remove take a plural `ids` list because they operate on many labels
 *  at once (merge folds N labels into one; remove is a batch delete). Split takes a singular `id`
 *  because it operates on ONE label at a time — a batched split with the same cut across many
 *  labels has no coherent meaning (each label needs its own cut geometry). */
export type LabelOp = LabelMergeOp | LabelRemoveOp | LabelSplitOp

export interface LabelMergeOp {
  op: 'label.merge'
  /** timepoint index the op applies to (>=0) */
  t: number
  /** the label ids the op touches (>=2; 0 is background and cannot be an input) */
  ids: number[]
  /** which id survives — must be one of `ids`. */
  into: number
}

export interface LabelRemoveOp {
  op: 'label.remove'
  t: number
  /** the label ids the op removes (>=1 id) */
  ids: number[]
}

export interface LabelSplitOp {
  op: 'label.split'
  t: number
  /** the single label to split */
  id: number
  /** polyline vertices, image-pixel L0 coords. `xs.length === ys.length >= 2`. The runner
   *  rasterises the polyline as a 1-pixel-wide cut and splits by connected components. */
  xs: number[]
  ys: number[]
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

// ── Split-op builder ────────────────────────────────────────────────────────────

/**
 * Build a Split op — cut label `id` at frame `t` along a polyline through (xs, ys). Points are
 * image-pixel L0 coords (matches `buildFocusViewState`'s convention).
 *
 * Returns `null` when the payload is nonsense (empty polyline, mismatched xs/ys) — same "silently
 * refuse malformed" contract as buildMergeOp/buildRemoveOp; the surface's blocked-with-reason
 * button handles the "why not" side.
 */
export function buildSplitOp(t: number, id: number,
                             xs: readonly number[], ys: readonly number[]): LabelSplitOp | null {
  if (!Number.isFinite(id) || id < 1) return null
  if (!xs.length || xs.length !== ys.length) return null
  const xi = xs.map(x => Math.floor(Number(x)))
  const yi = ys.map(y => Math.floor(Number(y)))
  if (xi.some(v => !Number.isFinite(v) || v < 0)) return null
  if (yi.some(v => !Number.isFinite(v) || v < 0)) return null
  if (xi.length < 2) return null
  return { op: 'label.split', t: Math.floor(t), id: Math.floor(id), xs: xi, ys: yi }
}

/**
 * Convenience: a centroid-anchored horizontal or vertical cut through label `id`. Spans ±`halfLen`
 * pixels around (cx, cy) along the chosen axis; the runner clips to the label's mask. `halfLen`
 * defaults to a big number so any reasonable label is fully bisected.
 */
export function buildCentroidSplitOp(t: number, id: number, cx: number, cy: number,
                                     axis: 'horizontal' | 'vertical',
                                     halfLen: number = 4096): LabelSplitOp | null {
  if (!Number.isFinite(cx) || !Number.isFinite(cy)) return null
  const cxi = Math.floor(cx), cyi = Math.floor(cy), h = Math.max(1, Math.floor(halfLen))
  if (axis === 'horizontal') {
    return buildSplitOp(t, id, [Math.max(0, cxi - h), cxi + h], [cyi, cyi])
  }
  return buildSplitOp(t, id, [cxi, cxi], [Math.max(0, cyi - h), cyi + h])
}

// ── Descriptions (tooltip + log line) ───────────────────────────────────────────

/** Short button label — used in the cockpit tool row alongside the icon. */
export function opLabel(op: LabelOp): string {
  if (op.op === 'label.merge')  return 'Merge'
  if (op.op === 'label.remove') return 'Remove'
  return 'Split'
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
  if (op.op === 'label.remove') {
    return op.ids.length === 1
      ? `Remove label ${op.ids[0]} at ${t}`
      : `Remove labels ${op.ids.join(', ')} at ${t}`
  }
  // label.split
  return `Split label ${op.id} along a ${op.xs.length}-point polyline at ${t}`
}

// ── Queue helper: undo (mirrors trackCorrection.undoLast) ───────────────────────

/** Return a copy of `pending` with the last op removed. Immutable — safe with reactive refs. */
export function undoLast(pending: readonly LabelOp[]): LabelOp[] {
  return pending.slice(0, -1)
}
