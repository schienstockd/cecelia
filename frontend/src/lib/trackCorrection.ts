/**
 * The correction worklist's arithmetic — kept out of the SFC so it is testable.
 *
 * The surface this serves inverts what the old R version did. There, you found the wrong track
 * yourself across hundreds of them and then said how to fix it; here the backend ranks what looks
 * wrong and pre-picks the fix (`GET /api/tracking/issues`), and the user only judges it. So the
 * things worth pinning are: what a row's button SAYS, what "done" means for a row, and the pending
 * stack — because a correction is submitted as ONE task run, not one per click
 * (docs/todo/CORRECTION_PLAN.md → Decision 3b).
 */

/** One op, in the vocabulary `tracking.correct` accepts verbatim. */
export interface TrackOp {
  op: 'points.remove' | 'points.add' | 'track.remove' | 'track.join' | 'track.split'
  labels?: number[]
  trackIds?: number[]
  trackId?: number
  atT?: number
}

/** A candidate from the detector. `op` is submittable as-is — nothing translates it. */
export interface TrackIssue {
  kind: 'gap' | 'jump' | 'short' | 'duplicate'
  op: TrackOp
  trackIds: number[]
  atT: number
  centroid: number[]
  severity: number
  reason: string
}

export interface IssuesResponse {
  valueName: string
  tracked: boolean
  nTracks?: number
  stepScale?: number
  timeStep?: number
  total?: number
  counts?: Record<string, number>
  issues: TrackIssue[]
  paths: Record<string, { t: number[]; x: number[]; y: number[]; label: number[] }>
}

/**
 * The button label for a candidate — a WORD, deliberately, not a glyph.
 *
 * PrimeIcons has nothing that reads as "merge these two" or "cut this in half", and every free
 * candidate collides with a meaning the glossary already records (`pi-link` against
 * `pi-external-link`, `pi-expand` against `pi-window-maximize`). Both ops are destructive and
 * asymmetric, so the label has to be exact rather than guessable — see the icon section of
 * CORRECTION_PLAN.md.
 */
export function opLabel(op: TrackOp): string {
  switch (op.op) {
    case 'track.join':    return 'Join'
    case 'track.split':   return 'Split'
    case 'track.remove':  return 'Remove'
    case 'points.remove': return 'Untrack'
    case 'points.add':    return 'Add'
    default:              return 'Apply'
  }
}

/** What the op will do, for the confirm step — the user's words, naming the tracks it touches. */
export function opDescription(op: TrackOp): string {
  switch (op.op) {
    case 'track.join':
      return `Join track ${op.trackIds?.[1] ?? '?'} into ${op.trackIds?.[0] ?? '?'}`
    case 'track.split':
      return `Split track ${op.trackId ?? '?'} at t=${op.atT ?? '?'}`
    case 'track.remove':
      return `Remove track${(op.trackIds?.length ?? 0) > 1 ? 's' : ''} ${(op.trackIds ?? []).join(', ')}`
    case 'points.remove':
      return `Untrack ${op.labels?.length ?? 0} cell(s)`
    case 'points.add':
      return `Add ${op.labels?.length ?? 0} cell(s) to track ${op.trackId ?? 'a new track'}`
    default:
      return 'Apply this edit'
  }
}

/**
 * A stable identity for a candidate, so "I already dealt with this one" survives a refetch.
 *
 * Keyed on the OP, not the reason text or the severity: re-running the detector after an edit
 * renumbers nothing but does re-rank, and a key that moved with the ranking would resurrect rows the
 * user had skipped.
 */
export function issueKey(i: TrackIssue): string {
  const o = i.op
  const parts = [o.op, (o.trackIds ?? []).join('+'), o.trackId ?? '', o.atT ?? '',
                 (o.labels ?? []).join('+')]
  return parts.join('|')
}

/** Human label for the four detector signatures. Short — it sits in a table cell. */
export const KIND_LABEL: Record<string, string> = {
  gap:       'Gap',
  jump:      'Jump',
  short:     'Too short',
  duplicate: 'Duplicate',
}

/**
 * Rows still worth showing: the chosen kinds, minus anything already queued or skipped.
 *
 * Queued rows leave the list rather than being ticked in place. A correction is submitted as one
 * batch, so a row whose op is already in the pending stack is not a decision the user still has to
 * make — leaving it visible invites applying it twice, and the engine would then reject the second
 * (a join whose B no longer exists) as a failed run.
 */
export function visibleIssues(
  issues: readonly TrackIssue[],
  opts: { kinds?: readonly string[]; pending?: readonly TrackOp[]; skipped?: readonly string[] } = {},
): TrackIssue[] {
  const kinds = opts.kinds && opts.kinds.length ? new Set(opts.kinds) : null
  const done = new Set<string>([
    ...(opts.skipped ?? []),
    ...(opts.pending ?? []).map(op => issueKey({ op } as TrackIssue)),
  ])
  return issues.filter(i => (!kinds || kinds.has(i.kind)) && !done.has(issueKey(i)))
}

/**
 * Counts for the header line: how many candidates, of how many tracks, and what is queued.
 *
 * `total` is the detector's own figure rather than `issues.length`, because the endpoint caps what it
 * sends with its geometry — reporting the page size as the finding would quietly under-report the
 * problem.
 */
export function worklistSummary(r: IssuesResponse | null, pendingCount: number): string {
  if (!r) return ''
  if (!r.tracked) return 'Not tracked — run tracking first.'
  const total = r.total ?? r.issues.length
  if (total === 0) return `Nothing to review across ${r.nTracks ?? 0} tracks.`
  const shown = r.issues.length
  const capped = shown < total ? ` (showing ${shown})` : ''
  const queued = pendingCount ? ` · ${pendingCount} queued` : ''
  return `${total} of ${r.nTracks ?? 0} tracks need review${capped}${queued}`
}

/**
 * Drop the last op from the pending stack.
 *
 * Undo is LAST-ONLY and only reaches the uncommitted stack. Ops apply in order and each sees the
 * previous result, so removing one from the middle would change what the ones after it mean — a
 * split that allocated track 21 followed by a join of 21 becomes nonsense if the split is pulled out.
 * Old R offered per-row rollback over committed history; an op journal buys replay-from-the-original
 * instead, and the UI should not imply otherwise (CORRECTION_PLAN.md → Decision 7).
 */
export function undoLast(pending: readonly TrackOp[]): TrackOp[] {
  return pending.slice(0, Math.max(0, pending.length - 1))
}

/**
 * The worklist as CSV rows — what the detector found, and what you decided about each one.
 *
 * Exported because a correction run is a change to the data that a reader of the figure cannot see:
 * this is the record of which candidates were applied, which were dismissed, and which were still
 * open when the export was taken. `decision` is the column that makes it a record rather than a
 * repeat of the scan.
 */
export function worklistCsvRows(
  issues: readonly TrackIssue[],
  pending: readonly TrackOp[],
  skipped: readonly string[],
): Record<string, unknown>[] {
  const queued = new Set(pending.map(o => JSON.stringify(o)))
  const dismissed = new Set(skipped)
  return issues.map(i => ({
    kind: i.kind,
    tracks: i.trackIds.join(' '),
    atT: i.atT,
    x: i.centroid[0] ?? '',
    y: i.centroid[1] ?? '',
    z: i.centroid[2] ?? '',
    severity: i.severity,
    reason: i.reason,
    fix: opLabel(i.op),
    decision: queued.has(JSON.stringify(i.op)) ? 'queued'
            : dismissed.has(issueKey(i)) ? 'dismissed' : 'open',
  }))
}
