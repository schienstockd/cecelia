/**
 * Top-level `watch` sources that name a `const` declared further down the same `<script setup>`.
 *
 * **A `watch` getter runs IMMEDIATELY** — before any callback, with or without `immediate: true` —
 * because that first call is how Vue collects the dependencies. So a source expression naming a
 * `const` declared below it hits the temporal dead zone and throws `ReferenceError: can't access
 * lexical declaration 'x' before initialization`, during `setup`.
 *
 * That failure is worth a checker because of how it PRESENTS. The throw takes the component's setup
 * with it, which aborts the parent's patch, so sibling components that are perfectly fine vanish
 * too: one mis-ordered line blanked an entire canvas — plot panels and the model vault together —
 * and the page looked like a data problem, not a syntax one. TypeScript cannot see it (the binding
 * exists, it is just not initialised yet), the dev server serves the module happily, and every test
 * passes. Only the browser console says what happened.
 *
 * Scope, and why each limit is deliberate:
 *
 * - **The SOURCE only, not the callback.** `watch(src, cb)` runs `src` now and `cb` later, so a
 *   callback may freely name anything declared below. Checking the whole call is what a first crude
 *   pass did, and it reported two `SummaryPanel` lines that were entirely correct.
 * - **`watchEffect(fn)` is checked whole** — there is no source/callback split; the effect itself is
 *   what runs immediately.
 * - **Top-level calls only** (column 0). A `watch` inside a function or a lifecycle hook runs when
 *   that runs, by which point every `const` is initialised. Indentation is a reliable proxy here
 *   because `<script setup>` bodies in this codebase are not wrapped in anything.
 * - **`computed` is NOT checked.** Its getter is lazy; naming a later const is fine until something
 *   reads it, by which time setup has finished.
 */

export interface SetupOrderHazard {
  /** The identifier used before it is initialised. */
  name: string
  /** 1-based line of the offending `watch` within the file. */
  line: number
}

const SCRIPT = /<script setup[^>]*>([\s\S]*?)<\/script>/
// A top-level binding: `const x =`, `let x =`, or a destructure `const { a, b: c } = …`.
const TOP_DECL = /^(?:const|let|var)\s+(?:([A-Za-z_$][\w$]*)|\{([^}]*)\})/gm
// A top-level `watch(` / `watchEffect(` — column 0, so nested ones are out of scope by construction.
const TOP_WATCH = /^watch(Effect)?\s*\(/gm

/** Names bound by a top-level declaration, each with the offset it becomes usable at. */
function declarations(script: string): Map<string, number> {
  const out = new Map<string, number>()
  for (const m of script.matchAll(TOP_DECL)) {
    const names = m[1] ? [m[1]] : (m[2] ?? '').split(',')
    for (const raw of names) {
      // `a: b` binds b, `a = 1` binds a, `...rest` binds rest
      const name = raw.split(':').pop()!.split('=')[0]!.replace(/[.\s]/g, '')
      if (name && !out.has(name)) out.set(name, m.index!)
    }
  }
  return out
}

/** The text of a call's arguments, from its `(` to the matching `)`. */
function callArgs(script: string, at: number): string {
  const open = script.indexOf('(', at)
  let depth = 0
  for (let i = open; i < script.length; i++) {
    const c = script[i]
    if (c === '(') depth++
    else if (c === ')' && --depth === 0) return script.slice(open + 1, i)
  }
  return script.slice(open + 1)
}

/** The first argument only — split at the first comma that is not inside brackets or a string. */
function firstArg(args: string): string {
  let depth = 0, quote = ''
  for (let i = 0; i < args.length; i++) {
    const c = args[i]!
    if (quote) { if (c === quote && args[i - 1] !== '\\') quote = '' ; continue }
    if (c === '"' || c === "'" || c === '`') { quote = c; continue }
    if ('([{'.includes(c)) depth++
    else if (')]}'.includes(c)) depth--
    else if (c === ',' && depth === 0) return args.slice(0, i)
  }
  return args
}

export function setupOrderHazards(src: string): SetupOrderHazard[] {
  const script = SCRIPT.exec(src)?.[1]
  if (!script) return []
  const decls = declarations(script)
  const scriptAt = src.indexOf(script)
  const lineAt = (i: number) => src.slice(0, scriptAt + i).split('\n').length

  const out: SetupOrderHazard[] = []
  for (const m of script.matchAll(TOP_WATCH)) {
    const args = callArgs(script, m.index!)
    // watchEffect's whole argument IS the effect; watch's first argument is the source
    const immediate = m[1] ? args : firstArg(args)
    for (const [name, declAt] of decls) {
      if (declAt <= m.index!) continue
      // NOT preceded by a dot: `props.vis` is a property access, not the `vis` binding. Without
      // this, SummaryPanel's fetch watch — which correctly reads `props.vis?.statsEnabled` — is
      // reported against a `const vis` declared 25 lines below it.
      if (new RegExp(`(?<![.$\\w])${name}\\b`).test(immediate))
        out.push({ name, line: lineAt(m.index!) })
    }
  }
  return out
}
