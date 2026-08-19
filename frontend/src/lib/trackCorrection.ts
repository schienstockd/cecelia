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
  /** terse: WHAT is wrong (the row) */
  reason: string
  /** one sentence: what to DO about it (the row's tooltip) */
  advice?: string
}

export interface IssuesResponse {
  valueName: string
  tracked: boolean
  nTracks?: number
  stepScale?: number
  timeStep?: number
  total?: number
  counts?: Record<string, number>
  /** the thresholds the server ACTUALLY used — the panel seeds its knobs from these (see P4e) */
  thresholds?: TrackThresholds
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

// ── Authoring an op the DETECTOR did not suggest (CORRECTION_PLAN.md → P4d) ────
//
// The worklist inverts old R for the case a signature catches. For the case the user simply SEES —
// a swap, a mid-track mis-link, a gap wider than `gapFrames` — there was no path at all, which made
// this WORSE than old R rather than better: there you could at least name the tracks. These helpers
// build the same op objects the detector emits, so a hand-authored edit and a suggested one are
// indistinguishable downstream: same queue, same one task run, same journal.

/** One track, summarised for the picker. `t0`/`t1` are frames; `netDistance` is µm. */
export interface TrackRow {
  track: number
  nFrames: number
  t0: number
  t1: number
  netDistance: number
}

/** Per-track rows from the path map, longest first — the picker's list. */
export function trackRows(paths: Record<string, { t: number[]; x: number[]; y: number[] }>): TrackRow[] {
  const out: TrackRow[] = []
  for (const [id, p] of Object.entries(paths)) {
    if (!p.t.length) continue
    const n = p.t.length
    const dx = (p.x[n - 1] ?? 0) - (p.x[0] ?? 0)
    const dy = (p.y[n - 1] ?? 0) - (p.y[0] ?? 0)
    out.push({
      track: Number(id), nFrames: n,
      t0: Math.min(...p.t), t1: Math.max(...p.t),
      netDistance: Math.hypot(dx, dy),
    })
  }
  return out.sort((a, b) => b.nFrames - a.nFrames || a.track - b.track)
}

/**
 * Do two tracks occupy the same frame?
 *
 * The engine REFUSES a join with a temporal overlap — two tracks that both have a cell at one
 * timepoint are not one cell — and it is right to. But finding that out only when the task fails,
 * after Apply, is a bad trade: the user has queued several edits by then. So the same rule is checked
 * here, from the frame ranges the picker already holds, and the button says why it is disabled.
 *
 * Ranges, not exact frames: a gappy pair could interleave without sharing a frame, so this is
 * conservative — it can warn where the engine would have allowed it, never the reverse.
 */
export function tracksOverlap(a: TrackRow, b: TrackRow): boolean {
  return a.t0 <= b.t1 && b.t0 <= a.t1
}

/** Join folds B into A; A must be the EARLIER track, so the result reads forward in time. */
export function joinOrder(a: TrackRow, b: TrackRow): [number, number] {
  return a.t0 <= b.t0 ? [a.track, b.track] : [b.track, a.track]
}

export const buildRemoveOp = (tracks: readonly number[]): TrackOp =>
  ({ op: 'track.remove', trackIds: [...tracks] })
export const buildJoinOp = (a: number, b: number): TrackOp =>
  ({ op: 'track.join', trackIds: [a, b] })
export const buildSplitOp = (track: number, atT: number): TrackOp =>
  ({ op: 'track.split', trackId: track, atT })
export const buildUntrackOp = (labels: readonly number[]): TrackOp =>
  ({ op: 'points.remove', labels: [...labels] })

/**
 * The detector's OWN op for each ticked candidate.
 *
 * Removing the per-row buttons made the pre-picked fix unreachable: a jump candidate knows it should
 * split track 116 at t=5, and the user was left to tick the row, read the frame out of the text and
 * type it into a box. This is the primary path back — tick, then Fix.
 */
export function suggestedOps(picked: readonly string[], issues: readonly TrackIssue[]): TrackOp[] {
  const byKey = new Map(issues.map(i => [issueKey(i), i]))
  return picked.map(k => byKey.get(k)?.op).filter((o): o is TrackOp => !!o)
}

export interface ManualAction {
  key: 'join' | 'split' | 'remove'
  label: string
  /** null when the action can be taken; otherwise WHY it cannot, for the disabled tooltip. */
  blocked: string | null
  op: TrackOp | null
}

/**
 * Which edits the current selection allows, and why the others do not.
 *
 * Returns every action always, with a reason when blocked — a button that vanishes teaches nothing,
 * and "why can't I join these" is the question this surface exists to answer.
 *
 * `splitAt` is a frame; it must fall strictly INSIDE the track, because splitting at the first frame
 * would produce an empty first half and the engine rejects it.
 */
export function manualActions(
  picked: readonly number[], rows: readonly TrackRow[], splitAt: number | null,
): ManualAction[] {
  const byId = new Map(rows.map(r => [r.track, r]))
  const sel = picked.map(id => byId.get(id)).filter((r): r is TrackRow => !!r)

  const join: ManualAction = { key: 'join', label: 'Join', blocked: null, op: null }
  if (sel.length !== 2) {
    join.blocked = 'Pick exactly two tracks'
  } else if (tracksOverlap(sel[0], sel[1])) {
    // the engine's own rule, checked before Apply rather than after
    join.blocked = `Tracks overlap in time (frames ${Math.max(sel[0].t0, sel[1].t0)}–` +
                   `${Math.min(sel[0].t1, sel[1].t1)}) — they are not one cell`
  } else {
    const [a, b] = joinOrder(sel[0], sel[1])
    join.op = buildJoinOp(a, b)
  }

  const split: ManualAction = { key: 'split', label: 'Split', blocked: null, op: null }
  if (sel.length !== 1) {
    split.blocked = 'Pick one track'
  } else if (splitAt === null || !Number.isFinite(splitAt)) {
    split.blocked = 'Set the frame to split at'
  } else if (splitAt <= sel[0].t0 || splitAt > sel[0].t1) {
    split.blocked = `Frame must be inside ${sel[0].t0}–${sel[0].t1}, after the first`
  } else {
    split.op = buildSplitOp(sel[0].track, splitAt)
  }

  const remove: ManualAction = { key: 'remove', label: 'Remove', blocked: null, op: null }
  if (!sel.length) remove.blocked = 'Pick at least one track'
  else remove.op = buildRemoveOp(sel.map(r => r.track))

  return [join, split, remove]
}

// ── Detector thresholds (CORRECTION_PLAN.md → P4e) ────────────────────────────

/**
 * The knobs `GET /api/tracking/issues` accepts. All optional: an absent value means "the server's
 * default", and the server reports back what it used, so the DEFAULTS ARE NEVER DUPLICATED HERE — the
 * numbers live on the Julia constants where they were measured.
 */
export interface TrackThresholds {
  gapFrames?: number
  gapSteps?: number
  jumpFactor?: number
  jumpQuantile?: number
  minLen?: number
}

export const THRESHOLD_FIELDS: { key: keyof TrackThresholds; label: string; tip: string; step: number }[] = [
  { key: 'gapFrames',    label: 'gap frames',  step: 1,
    tip: 'Join candidates: how many frames may be missing between two tracks' },
  { key: 'gapSteps',     label: 'gap steps',   step: 0.5,
    tip: "Join candidates: how far apart the ends may be, in multiples of this image's median step" },
  { key: 'jumpFactor',   label: 'jump ×',      step: 0.5,
    tip: "Split candidates: a step this many times the track's OWN median step is suspect" },
  { key: 'jumpQuantile', label: 'jump top',    step: 0.005,
    tip: 'Split candidates: …and in this top quantile of every step in the image' },
  { key: 'minLen',       label: 'min frames',  step: 1,
    tip: 'Flag tracks shorter than this many timepoints' },
]

/** Only what the user actually changed — so an untouched panel takes the server's own defaults. */
export function thresholdQuery(t: TrackThresholds, defaults: TrackThresholds): string {
  return THRESHOLD_FIELDS
    .filter(f => t[f.key] !== undefined && t[f.key] !== defaults[f.key])
    .map(f => `&${f.key}=${t[f.key]}`)
    .join('')
}

/** Have the knobs been moved off the server's defaults? Drives whether Reset is offered. */
export function thresholdsChanged(t: TrackThresholds, defaults: TrackThresholds): boolean {
  return thresholdQuery(t, defaults).length > 0
}

// ── The napari bridge (CORRECTION_PLAN.md → P4d, the other half) ───────────────

/** `GET /api/tracking/selection` — what is drawn in napari, resolved to tracks. */
export interface TrackSelection {
  valueName: string
  labels: number[]
  tracks: { track: number; nCells: number }[]
  nLabels: number
  nUntracked: number
}

/**
 * The selection in words: what was drawn, and what it resolved to.
 *
 * Says the UNTRACKED count separately because it is the actionable half for `points.add` and the
 * confusing half otherwise — "8 cells, 2 tracks" hides that three of those cells belong to no track.
 */
export function selectionSummary(sel: TrackSelection | null): string {
  if (!sel || !sel.nLabels) return ''
  const bits = [`${sel.nLabels} cell${sel.nLabels === 1 ? '' : 's'}`]
  if (sel.tracks.length) bits.push(`${sel.tracks.length} track${sel.tracks.length === 1 ? '' : 's'}`)
  if (sel.nUntracked) bits.push(`${sel.nUntracked} untracked`)
  return bits.join(' · ')
}

/**
 * The tracks a napari selection touches, most-represented first.
 *
 * Order matters for the two-track ops: the tracks with the most cells inside the drawn region are the
 * ones the user meant, so preselecting the top two makes "draw around the break, hit Join" work.
 */
export const selectedTracks = (sel: TrackSelection | null): number[] =>
  (sel?.tracks ?? []).map(t => t.track)

/**
 * Parse a track-id lookup box: "12, 40 91" → [12, 40, 91].
 *
 * The picker caps how many tracks it lists (longest first), so a specific track can be outside it.
 * Naming it fetches it regardless — better than raising the cap for everyone to serve one lookup.
 */
export function parseTrackIds(text: string): number[] {
  const out: number[] = []
  for (const part of text.split(/[\s,;]+/)) {
    if (!part) continue
    const n = Number(part)
    if (Number.isInteger(n) && n > 0 && !out.includes(n)) out.push(n)
  }
  return out
}
