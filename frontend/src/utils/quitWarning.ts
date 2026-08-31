// Quit tells you what it's about to kill.
//
// `POST /api/app/shutdown` stops the children and then `exit(0)`s the backend ~0.3s later — it does
// NOT wait for in-flight work. So quitting during a 40-minute cellpose run silently throws that run
// away, and the two-click ConfirmButton alone doesn't help: the user is confirming "quit", not
// "abandon 2 running tasks", because nothing told them tasks were running.
//
// Pure string builders so they're testable without mounting the SFC (frontend test scope = src/utils
// only — see docs/DEV.md). Both quit entry points (sidebar footer, Settings) read these, so the
// wording stays identical in the two places; see docs/inventory/FLOWS.md → app shutdown/quit. The COUNT comes
// from `runningTasks.ts` (the backend), not the local task store, which reads 0 after a page reload.
//
// Only the ARMED state reports the count. The idle control keeps its plain description: the count is
// fetched when you arm, so putting it on the idle tooltip too would show a stale number after a cancel.

/** `2 tasks running` / `1 task running` / `''` when idle. The shared fragment, not a full sentence. */
export function quitTaskPhrase(count: number): string {
  if (count <= 0) return ''
  return `${count} task${count === 1 ? '' : 's'} running`
}

/** Tooltip for the armed Quit control — the last thing seen before the work is dropped. */
export function quitConfirmTooltip(count: number): string {
  const phrase = quitTaskPhrase(count)
  return phrase ? `Confirm quit — kills ${phrase}`
                : 'Confirm quit — stops notebooks and the backend'
}

/**
 * Label for the armed Quit *button* where there's room for text (Settings). The sidebar footer is
 * icon-only, so there it lives in the tooltip instead — same numbers, one source.
 */
export function quitConfirmLabel(count: number): string {
  return count > 0 ? `Quit — kills ${count} task${count === 1 ? '' : 's'}` : 'Quit everything'
}
