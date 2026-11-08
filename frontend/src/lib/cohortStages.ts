// Which task fun_names a module page can run a cohort-consistency check over. Only stages the check can
// answer (Julia `cohort_has_answer`: banked `COHORT_METRICS`, or the gating QC funs) appear here; a module with no entry shows no
// "Check cohort consistency" button. Kept in step with `app/src/qc_cohort.jl` COHORT_METRICS by a
// TEST, not by this comment — see `cohort stages cover their category's cohort metrics` in
// app/test/suite.jl. The comment alone had already let `segment.coastal` and `segment.branching`
// drift out, which shows as a button that quietly checks less than it claims.
// Keyed by the `module` prop ModuleLayout passes (see the module pages).
export const COHORT_STAGES: Record<string, string[]> = {
  segment:           ['segment.cellpose', 'segment.coastal', 'segment.importLabels',
                      'segment.measureLabels', 'segment.branching', 'segment.correct'],
  opticalFlow:       ['opticalFlow.train'],
  tracking:          ['tracking.bayesian_tracking', 'tracking.track_measures', 'tracking.correct',
                      'gating.track'],
  // hand-drawn gates checked across the set (app/src/qc_gating.jl) — computed, not banked metrics
  gate:              ['gating.flow'],
  behaviourAnalysis: ['behaviour.hmm_states', 'behaviour.hmm_transitions'],
  clustPops:         ['clustPops.cluster'],
  clustTracks:       ['clustTracks.cluster'],
}

export function cohortFunsFor(module?: string): string[] {
  return (module && COHORT_STAGES[module]) || []
}
