// Scanner for CONTINUOUS controls — the ones that fire a burst of events while the user is still
// holding the mouse down (`<input type="range">` today; a drag handle is the same shape).
//
// Why this exists: a slider emits an event per pixel of travel, so a short drag is 20–60 events. That
// is harmless when the handler writes a local value, and pathological when it reaches a slow sink —
// the napari bridge (one command at a time), an API round trip, or a full chart rebuild. The symptom
// is always the same and always reads as a bug: the thing you are dragging keeps moving for seconds
// after you let go, working through requests you already superseded.
//
// The rule (docs/UI.md → *Continuous controls*): a continuous control's `@input` may write state; any
// effect beyond that is coalesced — `debouncedLatest` for a request, `rafCoalesce` for a paint,
// `debouncedSave` for a write — or moved to `@change`, which fires once on release. This module is the
// pure, testable half: the parse and the classification. The enforcement — which files are allowed a
// side-effecting handler, and which timers are allowed to re-arm — is the registry in
// `continuousControls.test.ts`.

/** What a control's `@input` handler does with the value. */
export type SinkKind =
  | 'bind'   // writes a ref / v-model / a plain assignment — the safe default
  | 'emit'   // hands it to the parent; whatever the parent does is the parent's declaration
  | 'call'   // calls something else — must be coalesced, on `@change`, or declared

export interface RangeControl {
  /** the opening tag, verbatim — enough to identify the control in a failure message */
  tag: string
  /** the `@input` (or `v-model`) expression, `''` when the tag binds neither */
  handler: string
  /** the tag also binds `@change` — the "apply on release" escape hatch (PoolThrottle, napari dots) */
  hasChange: boolean
  sink: SinkKind
}

/**
 * The `<template>` block of an SFC (its content), or the whole string when there is none — so the
 * scanner can be fed a bare snippet in a unit test.
 *
 * Scanning the whole file would be wrong, not merely wasteful: `<` in the script block is a comparison
 * or a TS generic, and the quote tracking below would latch onto the next apostrophe in a comment
 * ("doesn't") and swallow the rest of the file. That failure is silent — the scan just stops finding
 * controls — which is the one way a detector like this can rot without anyone noticing.
 */
export function templateBlock(source: string): string {
  const open = source.indexOf('<template>')
  if (open === -1) return source
  const close = source.lastIndexOf('</template>')
  return close === -1 ? source.slice(open + 10) : source.slice(open + 10, close)
}

/**
 * Split markup into its opening tags. Quote-aware, so a `>` inside an attribute value (an arrow
 * function, a TS generic, a template literal) does not end the tag early — which a `<[^>]*>` regex
 * would do silently, and silently is how a scanner starts under-reporting.
 */
export function openingTags(source: string): string[] {
  const out: string[] = []
  let i = 0
  while (i < source.length) {
    const lt = source.indexOf('<', i)
    if (lt === -1) break
    // only a real tag start — otherwise a stray `<` in interpolated text would open a quote-tracking run
    if (!/[A-Za-z/!]/.test(source[lt + 1] ?? '')) { i = lt + 1; continue }
    let j = lt + 1
    let quote: string | null = null
    while (j < source.length) {
      const c = source[j]
      if (quote) { if (c === quote) quote = null }
      else if (c === '"') quote = c
      else if (c === '>') break
      j++
    }
    if (j >= source.length) break
    out.push(source.slice(lt, j + 1))
    i = j + 1
  }
  return out
}

/** Value of `name="…"` on a tag, or undefined. Attribute values in an SFC template are double-quoted. */
function attr(tag: string, name: string): string | undefined {
  const at = tag.indexOf(` ${name}="`)
  if (at === -1) return undefined
  const from = at + name.length + 3
  const to = tag.indexOf('"', from)
  return to === -1 ? undefined : tag.slice(from, to)
}

/** Any `v-model`/`v-model:x`/`v-model.number` binding on the tag. */
function vModel(tag: string): string | undefined {
  const m = /\sv-model[.:][\w.:]*="([^"]*)"|\sv-model="([^"]*)"/.exec(tag)
  return m ? (m[1] ?? m[2]) : undefined
}

/** Classify a handler expression. Conservative: anything that is not clearly a write is a `call`. */
export function sinkOf(handler: string, viaVModel: boolean): SinkKind {
  if (viaVModel) return 'bind'
  const h = handler.trim()
  if (!h) return 'bind'
  if (/^\$?emit\s*\(/.test(h)) return 'emit'
  // a plain assignment to a ref / property / index — `x = …`, `a.b = …`, `m[k] = …`, but not `===`
  if (/^[\w.$]+(\[[^\]]*\])?(\.[\w$]+)*\s*=(?!=)/.test(h)) return 'bind'
  return h.includes('(') ? 'call' : 'bind'
}

/** Every `<input type="range">` in an SFC, with what its handler does. */
export function rangeControls(source: string): RangeControl[] {
  return openingTags(templateBlock(source))
    .filter(t => t.startsWith('<input') && /\stype="range"/.test(t))
    .map(tag => {
      const model = vModel(tag)
      const handler = model ?? attr(tag, '@input') ?? ''
      return {
        tag,
        handler,
        hasChange: /\s@change="/.test(tag),
        sink: sinkOf(handler, model !== undefined),
      }
    })
}

/**
 * Controls whose handler needs a declaration: it calls something, and there is no `@change` to make
 * the effect fire once on release instead.
 */
export function undeclaredControls(source: string): RangeControl[] {
  return rangeControls(source).filter(c => c.sink === 'call' && !c.hasChange)
}

/**
 * Names of timers that are RE-ARMED — `clearTimeout(t)` somewhere and `t = setTimeout(…)` somewhere.
 * That is the shape of a hand-rolled debounce, and it is how the app ended up with five near-copies of
 * the same scheduler, each with its own subtly different answer to "what happens to the superseded
 * run". There are three canonical helpers now (`debouncedLatest` / `rafCoalesce` / `debouncedSave`);
 * a sixth copy is the bug, not a style choice.
 *
 * Deliberately a SHAPE match, not a semantic one: plenty of legitimate timers re-arm (a connect
 * timeout, a flash reset, a confirm-button expiry), so the test that consumes this carries a short
 * allowlist saying what each one actually is. The point is that a new one has to be named.
 */
export function rearmedTimers(source: string): string[] {
  const cleared = new Set<string>()
  for (const m of source.matchAll(/clearTimeout\(\s*([\w$.]+)\s*\)/g)) cleared.add(m[1])
  const out = new Set<string>()
  // an ASSIGNMENT, not a declaration: `const t = setTimeout(…)` armed once and cleared on unmount is
  // an ordinary one-shot, and flagging it would bury the real signal in noise
  for (const m of source.matchAll(/(?:^|[^\w$.])(const|let|var)?\s*([\w$.]+)\s*=\s*setTimeout\(/g)) {
    if (!m[1] && cleared.has(m[2])) out.add(m[2])
  }
  return [...out]
}

/**
 * Text-ish inputs bound with `:value` and committed on `@change` — the shape whose DOM value runs
 * AHEAD of its binding while the field has focus.
 *
 * Vue force-patches an input's `value` on every element patch and compares against the DOM's current
 * text, not the previous binding, so a re-render mid-typing silently replaces what the user typed with
 * the bound value. It reads as "I entered a name and it jumped back to the prefilled one" — reported
 * for the movie filename, and present in five more fields on the plot-styling panel.
 *
 * `v-model` on the draft is the fix (see `composables/useFieldDraft`), not switching to `@input`:
 * commit-on-blur is deliberate for a field whose value is parsed or drives a re-render.
 *
 * A `<select>` is deliberately not matched — its DOM value only changes through a user selection,
 * which fires `change` at the same moment, so it cannot drift.
 */
const DRIFT_PRONE_TYPE = /^(text|number|search|email|url|tel|password)$/

export function driftingTextFields(source: string): string[] {
  return openingTags(templateBlock(source))
    .filter(t => t.startsWith('<input') || t.startsWith('<textarea'))
    .filter(t => {
      const type = attr(t, 'type') ?? 'text'          // an <input> with no type is a text field
      return t.startsWith('<textarea') || DRIFT_PRONE_TYPE.test(type)
    })
    .filter(t => /\s:value="/.test(t) && /\s@change="/.test(t) && !/\sv-model/.test(t))
}
