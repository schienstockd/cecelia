// Which task fun_names a module page can run a cohort-consistency check over. Only stages that bank
// cohort-comparable metrics (Julia `COHORT_METRICS`) appear here; a module with no entry shows no
// "Check cohort consistency" button. Keep in sync with `app/src/qc_cohort.jl` COHORT_METRICS.
// Keyed by the `module` prop ModuleLayout passes (see the module pages).
export const COHORT_STAGES: Record<string, string[]> = {
  segment:           ['segment.cellpose', 'segment.measureLabels'],
  tracking:          ['tracking.bayesian_tracking', 'tracking.track_measures'],
  behaviourAnalysis: ['behaviour.hmm_states', 'behaviour.hmm_transitions'],
  clustPops:         ['clustPops.cluster'],
  clustTracks:       ['clustTracks.cluster'],
}

export function cohortFunsFor(module?: string): string[] {
  return (module && COHORT_STAGES[module]) || []
}
