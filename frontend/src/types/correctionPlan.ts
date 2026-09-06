// Shape of a CorrectionPlan as it comes off /api/correction-plan/* and off disk in plan.json.
// Julia writes both via `_plan_to_dict` (app/src/correction_plan.jl) — one source of truth, one
// shape. Field names match the JSON, not the Julia struct field names, so a save/load round-trip
// via the API needs no adapter.
//
// The plan doc is docs/todo/CORRECTION_QC_PLAN.md; §8 lists what each field is FOR.

export interface CorrectionStep {
  funName: string
  params: Record<string, unknown>
  orderWeight: number
  // How this step got into the plan. Set by `apply_rules` and by the wizard/card layers.
  // Values: `rule_default`, `card`, `wizard`, `computed_qc`, `user_edit`.
  source: string
  // Non-null only for excluded steps — the human-readable reason the plan dropped this task.
  exclusionReason: string | null
}

export interface QCScore {
  metric: string
  // null when the metric could not be computed (JSON has no NaN). Rule engine treats null as
  // "no signal", not as "clean signal" — see qc_score_absent in the Julia layer.
  score: number | null
  level: string
  subs: Record<string, unknown>
  // `image` | `channel` | `pair` | `frame` | `plane` — the reduction axis of the metric.
  scope: string
}

export interface CorrectionPlan {
  planVersion: number
  ceceliaVersion: string
  imageUid: string
  presetId: string
  wizardAnswers: Record<string, string>
  saturationFingerprint: string
  included: CorrectionStep[]
  excluded: CorrectionStep[]
  qcScores: QCScore[]
}

export interface AcquisitionPresetSummary {
  id: string
  name: string
  description: string
  orderHints: string[]
  // `unvalidated` until at least one dev fixture attests to the card's regime (plan doc §5).
  validationStatus: string
}
