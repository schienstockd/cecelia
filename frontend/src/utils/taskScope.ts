/**
 * Which project's tasks the Task Manager lists — and what a row from elsewhere is called.
 *
 * The task store is deliberately NOT cleared when a project is opened: a run keeps reporting into the
 * tab that launched it, and adopting the backend's in-flight set on connect (`utils/runningTasks.ts`)
 * is what makes a reload work. The consequence is that after a switch the store legitimately holds
 * rows from more than one project, and every view has to say which it means. The per-module views
 * always have (`forModule(module, projectUid)`); the manager listed everything, with nothing on a row
 * to say where it came from — so a Smoothing run from the project you just left sat above the
 * training run you are watching, looking like yours.
 *
 * In a util rather than the SFC because the rule has two exceptions that are easy to get wrong, and
 * both are invisible until the day they matter:
 *
 * - **A row with no project is never out of scope.** A project *import* has none yet (`projectUid`
 *   is `''` — the project it creates does not exist when the job starts), so scoping it away would
 *   hide the progress of the very thing that makes the project.
 * - **An export names a project that is usually NOT the open one.** It is dispatched from the
 *   project panel against any project on disk. That is a real cross-project row, which is why the
 *   scope is a toggle rather than a rule — and why, with the scope off, a foreign row gets labelled
 *   instead of blending in.
 */

/** The fields of a task row this module needs. Structural, so `TaskEntry` satisfies it. */
export interface ScopedTask {
  projectUid: string
}

/**
 * Is this row in scope for the open project?
 *
 * `thisProjectOnly = false` shows everything; `currentUid` empty (no project open) likewise shows
 * everything, because scoping to nothing would empty the list rather than answer the question.
 */
export function taskInScope(
  t: ScopedTask,
  currentUid: string | undefined | null,
  thisProjectOnly: boolean,
): boolean {
  if (!thisProjectOnly || !currentUid) return true
  return !t.projectUid || t.projectUid === currentUid
}

/**
 * What to label a row with, or `''` for no label.
 *
 * Only when the list can actually mix: with the scope ON every row is the open project's, and a chip
 * repeating that on all of them is noise. Falls back to the uid when the project is not in the recent
 * list (it may have been deleted, and a uid is still a lead).
 */
export function taskProjectLabel(
  t: ScopedTask,
  currentUid: string | undefined | null,
  thisProjectOnly: boolean,
  nameOf: (uid: string) => string | undefined,
): string {
  if (thisProjectOnly) return ''
  if (!t.projectUid || t.projectUid === currentUid) return ''
  return nameOf(t.projectUid) || t.projectUid
}

/**
 * Should a task frame be allowed to write into the OPEN project's data?
 *
 * `task:result` carries the project the task ran in, and one of its jobs is to fold a newly produced
 * image into the store so it appears without a reload (`cropImage`/`copyImage` report `newImageUid` +
 * `setUid`). That was unconditional: a crop finishing in the project you just left would `ensureSet`
 * ITS set into the project you just opened and add its image to it — another project's data,
 * indistinguishable from your own once it is in the table. Tasks outlive the switch that leaves them
 * running, which is exactly when this fires.
 *
 * `true` when the frame does not name a project, or nothing is open: the frame is then unattributable
 * and the historical behaviour (write it) is the best available — refusing would drop legitimate
 * updates from any backend or path that omits the field.
 */
export function frameTargetsOpenProject(
  frameProjectUid: string | undefined | null,
  openUid: string | undefined | null,
): boolean {
  if (!frameProjectUid || !openUid) return true
  return frameProjectUid === openUid
}
