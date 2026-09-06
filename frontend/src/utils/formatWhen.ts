// ONE way to say WHEN across the app — the Tasks history Date column, the Movies Recorded column, and
// anywhere else a moment needs a compact label in a narrow table cell. The rule is compact by scale:
//   • today            → time only        (`14:32`)
//   • earlier this year → day+month+time   (`5 Sep 14:32`)
//   • prior year        → day+month+year   (`5 Sep 2025`)
//
// Same rule everywhere so two tables side-by-side read the same way. Locale-aware — the tokens above
// are what you see in `en-*`; a user on another locale sees the equivalent under `Intl` rules.
//
// `now` is optional and defaults to `Date.now()` — the "today" comparison only flips at midnight, so a
// caller that isn't already carrying a shared clock (e.g. the movie list) is fine reading the wall
// clock directly. The tasks list passes its own `ctx.now` so the label stays consistent with the
// same-clock elapsed counter next to it.

export function formatWhen(when: Date | undefined, now: number = Date.now()): string {
  if (!when) return ''
  const n = new Date(now)
  const sameDay  = when.getFullYear() === n.getFullYear()
                && when.getMonth()    === n.getMonth()
                && when.getDate()     === n.getDate()
  if (sameDay) return when.toLocaleTimeString(undefined, { hour: '2-digit', minute: '2-digit' })
  const sameYear = when.getFullYear() === n.getFullYear()
  return when.toLocaleString(undefined, sameYear
    ? { day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' }
    : { day: 'numeric', month: 'short', year: 'numeric' })
}
