// An absent optional Boolean prop is `false`, never `undefined` — and a check for `undefined` on one
// is therefore dead code that always takes the wrong branch.
//
// This is Vue's Boolean CASTING rule (`resolvePropValue`): a prop whose declared type includes
// `Boolean` and which has no `default` resolves to `false` when the parent does not pass it. So the
// idiom that works for every other type — "`undefined` means the parent did not supply it" — is the one
// thing that cannot work for a boolean.
//
// It shipped once, and the blast radius was the whole app rather than the feature: `CollapsibleSection`
// grew an optional `open` prop to support an accordion, and used `props.open === undefined` to tell
// controlled from uncontrolled. Absent became `false`, so EVERY section in the app — the image table,
// the plots canvas, the chain palette, export/import — silently became controlled by a parent that was
// not listening, and every one of them was stuck shut. The image table showed "1 / 1 image" and no
// rows (Dominik, 2026-08-25).
//
// The fix is a type union that suppresses the cast plus an explicit default: `boolean | null` with
// `default: null`. Verified against Vue's own prop resolution, not reasoned from the docs.

/** The `defineProps<{ … }>` type literal, or '' when the SFC has none. */
export function propsBlock(src: string): string {
  const at = src.indexOf('defineProps<')
  if (at < 0) return ''
  const open = src.indexOf('{', at)
  if (open < 0) return ''
  let depth = 0
  for (let i = open; i < src.length; i++) {
    if (src[i] === '{') depth++
    else if (src[i] === '}') { depth--; if (depth === 0) return src.slice(open, i + 1) }
  }
  return ''
}

/**
 * Optional props whose declared type is boolean ALONE. A union — `boolean | null`, `boolean | string`
 * — is deliberately excluded: that is exactly the shape that suppresses the cast, so those props do
 * come back as their default and comparing them is fine.
 */
export function optionalBooleanProps(src: string): string[] {
  const block = propsBlock(src)
  if (!block) return []
  const out: string[] = []
  for (const m of block.matchAll(/([A-Za-z_$][\w$]*)\s*\?\s*:\s*([^;,\n]+)/g)) {
    // The LAST member of the literal has no `;` or `,` after it, so its type runs up to the closing
    // brace — trim it, or the final prop is the one that silently never matches.
    if (m[2].replace(/\}\s*$/, '').trim() === 'boolean') out.push(m[1])
  }
  return out
}

/** Optional-boolean props the source compares against `undefined`. Empty is the only right answer. */
export function booleanUndefinedChecks(src: string): string[] {
  const names = optionalBooleanProps(src)
  const found: string[] = []
  for (const n of names) {
    // `props.x === undefined`, `x === undefined` after a destructure, and the negated forms.
    // The lookbehind goes BEFORE the optional `props.`, not after it: placed after, it sees the dot it
    // just matched and rejects every `props.x === undefined` — the exact form this exists to catch.
    const re = new RegExp('(?<![\\w.])(?:props\\.)?' + n + '\\s*[!=]==\\s*undefined')
    if (re.test(src)) found.push(n)
  }
  return found
}
