// Matching + keyboard logic for `SuggestInput` — the "type freely, with what you already use offered"
// combobox. Kept out of the SFC so it is testable without mounting (docs/DEV.md → Tests).
//
// Why this is not a native <datalist>: that popup is browser CHROME, so it renders at the browser's
// own UI font (~16px) and ignores every app token. Next to a 0.82rem input it is enormous, and no
// selector reaches it. See docs/todo/VALUE_NAME_INPUT_PLAN.md → D4.

/**
 * The suggestions to offer for `query`, best match first — case-insensitive SUBSTRING, with entries
 * that START with the query ranked above entries that merely contain it ("cell" offers `cellA`
 * before `Tcell`). Ties keep the caller's order, which is the order the source reports them in.
 *
 * **An empty query offers EVERYTHING.** Clicking into the field shows what you have already used —
 * which is the question being asked ("what did I call the other one?") and is unanswerable if the
 * list only appears once you can already spell it. Typing then narrows, and clearing the box brings
 * the full list back rather than leaving it stuck empty.
 *
 * An exact match is deliberately still offered: seeing the text you just typed in the list is how you
 * know you are about to reuse an EXISTING entry rather than create a new one — which is the whole
 * distinction this input exists to make visible. On a value name that is re-run vs new label set; on
 * an image attribute it is the difference between joining a cohort group and inventing one.
 */
export function filterSuggestions(options: readonly string[], query: string): string[] {
  const q = query.trim().toLowerCase()
  if (!q) return [...options]
  const starts: string[] = []
  const contains: string[] = []
  for (const n of options) {
    const i = n.toLowerCase().indexOf(q)
    if (i === 0) starts.push(n)
    else if (i > 0) contains.push(n)
  }
  return [...starts, ...contains]
}

/**
 * The next highlighted index for an arrow key, wrapping at both ends.
 *
 * `-1` means "nothing highlighted", which is the state after every keystroke: the user is typing
 * something NEW until they say otherwise, so Enter must not silently accept a suggestion they never
 * moved to. Arrowing down from there lands on the first item; arrowing UP from there lands on the
 * last, which is the standard way to reach the bottom of a short menu.
 */
export function moveHighlight(current: number, delta: number, count: number): number {
  if (count <= 0) return -1
  if (current < 0) return delta > 0 ? 0 : count - 1
  return (current + delta + count) % count
}

/**
 * Does `value` already exist in `options`? Case-insensitive, because the things this names are not
 * case-distinct in practice — two images from one experiment shipped `mem-TOM` and `mem-Tom`, and the
 * same trap applies to a label set or an attribute value.
 *
 * Used to TELL the user which of the two things they are doing (reuse vs create), never to rewrite
 * what they typed: the text they entered is the text that gets used, exactly as entered.
 */
export function isExistingOption(options: readonly string[], value: string): boolean {
  const v = value.trim().toLowerCase()
  return v !== '' && options.some(n => n.toLowerCase() === v)
}

// ── Multi-value fields ────────────────────────────────────────────────────────
// A tag field holds several values in one box ("live, qc, redo"). Completing the WHOLE field there
// would replace every tag the user already typed, so the two helpers below scope the suggestion to
// the token the caret is in. `separator` absent = the whole field is one value, which is every
// single-value caller and the reason these are no-ops rather than a second component.

/**
 * `options` minus the ones already in `value` — the tags you have added are not still on offer, so a
 * list you are picking from gets shorter instead of letting you add `live` three times.
 *
 * Only the COMPLETED tokens are dropped. The token at the caret is what you are *typing*, so `live`
 * has to stay offered while `li` is on its way to it — dropping that one would make the list vanish
 * exactly as it became useful. Case-insensitive, matching `isExistingOption`.
 *
 * A single-value field (no `separator`) has no other tokens, so this is a no-op there — which is why
 * it is one function rather than a second component.
 */
export function withoutChosen(
  options: readonly string[], value: string, separator?: string,
): string[] {
  if (!separator) return [...options]
  const i = value.lastIndexOf(separator)
  if (i < 0) return [...options]
  const chosen = new Set(
    value.slice(0, i).split(separator).map(t => t.trim().toLowerCase()).filter(Boolean))
  return options.filter(o => !chosen.has(o.trim().toLowerCase()))
}

/** The token being typed — the text after the last `separator`. The whole value when there is none. */
export function activeToken(value: string, separator?: string): string {
  if (!separator) return value
  const i = value.lastIndexOf(separator)
  return i < 0 ? value : value.slice(i + separator.length)
}

/**
 * `value` with its active token replaced by `choice`, ready for the user to keep typing.
 *
 * Re-emits the earlier tokens verbatim rather than re-joining a parsed list: whatever spacing the
 * user chose is theirs, and a "helpful" normalise here would rewrite the field under the caret.
 * A trailing separator + space is appended so the next tag can be typed straight away.
 */
export function replaceActiveToken(value: string, choice: string, separator?: string): string {
  if (!separator) return choice
  const i = value.lastIndexOf(separator)
  const head = i < 0 ? '' : value.slice(0, i + separator.length)
  return `${head}${head && !head.endsWith(' ') ? ' ' : ''}${choice}${separator} `
}
