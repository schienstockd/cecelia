// Analysis-board canvas keys. The board layout/tab stores are keyed in memory by
// `analysis:<projectUid>:tab:<id>` (namespaces the shared store while a project is open). That uid is
// an in-memory detail ONLY — it must NOT be baked into what's persisted to `analysisBoards.json`,
// because that file already lives inside the project, and a uid change (import-as-copy, future rename)
// would then orphan every layout whose key still carries the old uid. So on disk keys are
// project-relative (`tab:<id>`), and we re-apply the CURRENT uid on load. See docs/OBJECTMODEL.md
// ("project identity = directory name; internal files must not embed the uid").

export const boardGroupKey = (uid: string): string => `analysis:${uid}`

// The project-relative key: strip a leading `analysis:<anyUid>:` if present (→ `tab:<id>`); idempotent,
// so it accepts both the on-disk relative form and a legacy `analysis:<oldUid>:tab:<id>` key verbatim.
export const relBoardKey = (k: string): string => k.replace(/^analysis:[^:]+:/, '')

// Re-key a loaded layout map onto the current group: every entry (relative OR legacy old-uid) becomes
// `<groupKey>:<rel>`. This is what makes a project's boards survive a uid change.
export function rekeyBoards<T>(groupKey: string, map: Record<string, T> | null | undefined): Record<string, T> {
  const out: Record<string, T> = {}
  for (const [k, v] of Object.entries(map ?? {})) out[`${groupKey}:${relBoardKey(k)}`] = v
  return out
}
