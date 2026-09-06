// Filling in the log of a task this tab didn't watch start.
//
// An adopted row (`runningTasks.ts`) collects log lines from the moment the socket connected, so everything
// the task printed before that is missing — which for a long segmentation is most of what you want to
// read ("is it on image 12 of 20?"). Those lines are not lost: the scheduler tees every one to
// `{img._dir}/logs/{fun_name}.log` (`_wrap_log_with_file`), and `GET /api/images/tasklog` serves it.
//
// **The file is cumulative** — one per (image, fun_name), appended by every run — so it must be sliced to
// the run in question. The fetch passes the run's `startedAt` as `since` AND, for a history row whose
// same-`(image, fun)` was later re-run, the next run's start as `until`; the server slices half-open
// (`since ≤ ts < until`). Without `until` the slice runs to EOF — fine for a live task and any run with
// no successor; with it, an older history row shows its own output rather than dragging every later
// run's output in too. Slicing is server-side because the file's stamps are LOCAL time and the server
// is the process whose clock wrote them (`_tasklog_slice` in `api/src/routes.jl`).
//
// Fetched LAZILY, when a row's log is first opened — twenty adopted rows must not fire twenty requests on
// connect for output nobody has asked to see.

/** Text → the store's line array. Trailing blank from the file's final newline is dropped, blanks inside kept. */
export function logLines(content: string): string[] {
  if (!content) return []
  const lines = content.split('\n')
  while (lines.length && lines[lines.length - 1] === '') lines.pop()
  return lines
}

export interface BackfillTarget {
  projectUid: string
  imageUid: string
  /** the task's `fun_name` — becomes `logs/{fun}.log` server-side */
  funName: string
  /** the task's start; without it the whole cumulative file would come back, previous runs included */
  startedAt?: Date
  /** upper bound — the start of the NEXT same-`(image, fun)` run, when one exists. Without it the
   *  slice runs to EOF, so a history row from before repeated runs pulled every later run's output
   *  in too. Live rows leave it unset — nothing has started after them yet. Sourced from
   *  `TaskEntry.logSliceUntil`, which is computed by `utils/taskHistoryRows.ts`. */
  until?: Date
}

/**
 * The lines a task wrote before this tab was listening, or `[]` if they can't be had.
 *
 * Fails CLOSED (empty) like the other task backstops: this fills a display gap, so a missing file, an
 * older backend or a transient error must mean "nothing to add", never an exception in a click handler.
 *
 * Returns `[]` without asking when `startedAt` is unknown, rather than fetching the whole cumulative file
 * — showing a previous run's output as this run's would be worse than showing none.
 */
export async function fetchLogBackfill(t: BackfillTarget): Promise<string[]> {
  if (!t.projectUid || !t.imageUid || !t.funName || !t.startedAt) return []
  const qs = new URLSearchParams({
    projectUid: t.projectUid,
    imageUid:   t.imageUid,
    fun:        t.funName,
    since:      t.startedAt.toISOString().replace(/(\.\d{3})Z$/, '$1Z'),
  })
  if (t.until) qs.set('until', t.until.toISOString().replace(/(\.\d{3})Z$/, '$1Z'))
  try {
    const r = await fetch(`/api/images/tasklog?${qs.toString()}`)
    if (!r.ok) return []
    const d = await r.json() as { exists?: boolean; content?: string }
    return d.exists ? logLines(String(d.content ?? '')) : []
  } catch {
    return []
  }
}
