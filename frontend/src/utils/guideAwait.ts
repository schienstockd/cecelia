// Which run an `awaitTask` step parks on — the task half of the guide runtime, kept out of the store
// for the same reason `guideAnchor.ts` is (the store stays about progression), and pure so it can be
// tested at all: nothing that instantiates the guide store is testable here, since this project's
// vitest has no DOM by design.
//
// The step means "the run you just started". The store took the newest task matching the function,
// full stop — right in the case the comment described (the user has just pressed Run, so the row
// already exists) and wrong in the one case it didn't consider: a matching run that had ALREADY
// FINISHED before the guide began. Next is always available (plan D2), so pressing it past the Run
// step arrives here with no new run, and the guide then acted on the old one — skipping the step
// outright on a `done`, or announcing "That run failed." about work it never asked for on a `failed`.
// Every `awaitTask` in the catalogue matches on a specific `fun`, so the collision is simply "you ran
// this same function earlier in this tab", which is the normal way to end up wanting the guide.
//
// A run qualifies if it is one the guide could plausibly be waiting for:
//   • it started since the guide began — `seq` beats the mark taken at `start()`. This includes a run
//     that finished before the step was even reached, which a fast one does (the click gate holds the
//     Run step for ADVANCE_DELAY_MS), so the fast case must not be excluded;
//   • or it is still in flight, for the user who pressed Run first and opened the guide afterwards.
// A finished run from before the guide is neither.

/** `GuideStep.awaitTask` minus the label — what identifies the run, not what to call it. */
export interface AwaitSpec { fun?: string; module?: string }

/** The fields of a `TaskEntry` this decision reads. Deliberately no more than that. */
export interface AwaitCandidate {
  id: string
  seq: number
  funName: string
  module: string
  status: string
}

// `queued` counts: a run waiting for a pool slot is one the guide should park on, not skip.
const IN_FLIGHT = new Set(['queued', 'running'])

/** Runs a step could be waiting for, in the list's own order. */
export function awaitCandidates<T extends AwaitCandidate>(
  tasks: T[],
  spec: AwaitSpec | undefined,
  sinceSeq: number,
): T[] {
  if (!spec) return []
  return tasks.filter(t =>
    (!spec.fun || t.funName === spec.fun) &&
    (!spec.module || t.module === spec.module) &&
    (t.seq > sinceSeq || IN_FLIGHT.has(t.status)))
}

/** The one to park on — the newest qualifying run, or null to wait for the next that shows up. */
export function awaitedRun<T extends AwaitCandidate>(
  tasks: T[],
  spec: AwaitSpec | undefined,
  sinceSeq: number,
): T | null {
  return awaitCandidates(tasks, spec, sinceSeq)
    .reduce<T | null>((best, t) => (!best || t.seq > best.seq ? t : best), null)
}

/** The mark `awaitedRun` compares against: the highest task number in the list when a guide starts. */
export function highestSeq(tasks: { seq: number }[]): number {
  return tasks.reduce((m, t) => Math.max(m, t.seq), 0)
}
