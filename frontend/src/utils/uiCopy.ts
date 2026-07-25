// UI copy budget — the enforceable half of `docs/UI.md` → *UI copy — keep it short*.
//
// The rule ("default to no explanatory text; one short line where orientation isn't self-evident")
// was review-only, and review does not hold: 56 of 175 task-parameter tips and 41 of 568 tooltip
// strings had drifted past 90 characters, the worst at 332 and 228, before anyone noticed. These
// helpers make the two mechanical parts of it checkable, in the same shape as `cssScenarios` — an
// exact allow-list rather than a count, so a violation can't be swapped for another silently.
//
// What is NOT checkable, and stays a review question: whether a tooltip should exist at all, and
// whether a short line is the *right* short line.

/** Longest a single piece of UI copy may be. Matches the budget table in `docs/UI.md`. */
export const COPY_MAX = 90

// A trailing dot on one of these is an abbreviation, not a sentence end — "e.g. HMM state" must not
// read as two sentences. This list is why the check can be strict about periods at all.
const ABBREV = /(?:^|[\s(])(?:e\.g|i\.e|etc|vs|cf|approx|fig|no|Dr|St)\.$/i

/** Collapse whitespace and unescape the quote form Vue attributes use, so lengths are comparable. */
export function normalise(text: string): string {
  return text.replace(/\\'/g, "'").split(/\s+/).filter(Boolean).join(' ')
}

/**
 * Whether `text` runs to more than one sentence.
 *
 * A tooltip is one line: what the control does. Two sentences means the second one is explaining,
 * which is what `docs/` is for. Splits on a period followed by whitespace + a capital or `(`,
 * ignoring known abbreviations.
 */
export function isMultiSentence(text: string): boolean {
  const s = normalise(text)
  for (const m of s.matchAll(/\S*\.\s+(?=[A-Z(])/g)) {
    if (!ABBREV.test(m[0].trimEnd())) return true
  }
  return false
}

/** Whether `text` exceeds the budget. */
export function isTooLong(text: string): boolean {
  return normalise(text).length > COPY_MAX
}

// `${…}` is interpolated at render time and its width is unknowable here, so it is dropped rather
// than guessed. A tooltip that is only just inside the budget plus a long uid could still render
// wide; that is a review question, not something this can decide.
const stripInterpolation = (s: string) => s.replace(/\$\{[^}]*\}/g, '')

// Every string literal inside a binding expression: '…' or `…`.
const LITERAL = /'((?:[^'\\]|\\.)*)'|`([^`]*)`/g

/**
 * The tooltip strings an SFC renders.
 *
 * IMPORTANT — this reads the string LITERALS inside each binding, not the binding expression. That
 * distinction is the whole reason this function exists: measuring the expression counts
 * `flaggedActive ? 'Deselect flagged images' : 'Select all N flagged image(s)'` as one 95-character
 * violation when both branches a user actually sees are well inside budget. Doing it the naive way
 * over-reported by ~78% (73 "violations" against a true 41) and would have sent a reader off to
 * "fix" ternaries that were already fine.
 *
 * A binding with no literal at all (`v-tooltip="label"`) contributes nothing — the text lives
 * wherever that variable is built, and is checked there if it is a literal.
 */
export function tooltipStrings(src: string): string[] {
  const out: string[] = []
  for (const bind of src.matchAll(/v-tooltip(?:\.[a-z]+)*\s*=\s*("|')([\s\S]*?)\1/g)) {
    for (const lit of bind[2].matchAll(LITERAL)) {
      const text = normalise(stripInterpolation(lit[1] ?? lit[2] ?? ''))
      if (text) out.push(text)
    }
  }
  return out
}

/**
 * `hint` / `no-set-hint` props an SFC passes to `ModuleLayout` — the first-use callout copy.
 * Static attribute form only (`hint="…"`); a bound `:hint` is an expression and is left to review.
 */
export function hintStrings(src: string): string[] {
  const out: string[] = []
  for (const m of src.matchAll(/(?<![:\w-])(?:no-set-)?hint\s*=\s*"([^"]*)"/g)) {
    const text = normalise(m[1])
    if (text) out.push(text)
  }
  return out
}

// Task-JSON `tip` fields carry the same budget, but they are backend files (`app/src/tasks/**`) and
// the frontend never owns a copy of a task spec. That half of the ratchet lives with them, in
// `app/test/runtests.jl` — keeping this suite to `frontend/src` and free of node builtins (reading
// outside the Vite root would mean adding `@types/node` for one glob).
