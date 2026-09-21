// Markdown → HTML for BlackboardModule + a helper that names the ```mermaid fences the SFC needs to
// render out-of-band (mermaid is a ~1 MB dep that only pays off when a diagram is actually present,
// so the module dynamic-imports it lazily). Kept out of the SFC so the pure logic is testable
// (frontend rule: pure logic in `utils/*.ts`).
//
// Trusted source — same as WhatNewCard.vue. A blackboard entry is written by (a) Claude via MCP into
// this user's own project on this user's own machine and (b) the user editing the textarea. No third
// party writes it, so we don't sanitize; if a `.ccbundle` from an untrusted source ever becomes a
// distribution surface, revisit this call site.

import { marked, Marked, type Tokens } from 'marked'

marked.setOptions({ gfm: true, breaks: false })

/** `[[bb-<ts>-<hex>]]` and `[[profile]]` — internal wiki-style links to other Blackboard entries.
 *  Claude writes these naturally when cross-referencing (e.g. "see [[bb-…]] for the pipeline");
 *  the resolver rewrites each to `<a href="#bb:<id>">Title</a>` so the SFC can intercept a click
 *  and select the target entry.
 *
 *  Implemented as a marked INLINE EXTENSION rather than a pre-render string replace: extensions
 *  run at the inline lexer level, so a `[[bb-…]]` inside a ```code``` fence or `` `codespan` ``
 *  is already a `code` / `codespan` token by then and never reaches this tokenizer. Absent title
 *  (linked entry deleted) → still linked as "<id> (deleted?)" so the reader knows the reference
 *  existed. The regex matches only well-formed ids; plain `[[wiki]]` bracket pairs are left alone. */
const _BB_ID_RE   = /^(profile|bb-[0-9]{8}T[0-9]{6}-[0-9a-f]{6})/
const _BB_OPEN    = '[['
const _BB_ANCHOR  = _BB_OPEN

interface BbWikiToken extends Tokens.Generic { type: 'bbWiki'; id: string }

function escapeHtml(s: string): string {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;').replace(/'/g, '&#39;')
}

/** Build a Marked instance that resolves `[[bb-…]]` against `titleById`. Instantiated per-render
 *  so the closure over `titleById` stays local — no global state, no cross-render bleed. */
function makeMarkedFor(titleById: Map<string, string>): Marked {
  return new Marked({
    gfm: true,
    breaks: false,
    extensions: [{
      name: 'bbWiki',
      level: 'inline',
      // `start` tells marked's inline lexer where the next possible match begins, so it can jump
      // there instead of walking the whole source. undefined = no more matches in the tail.
      start(src: string) {
        const i = src.indexOf(_BB_ANCHOR)
        return i === -1 ? undefined : i
      },
      tokenizer(src: string) {
        if (!src.startsWith(_BB_OPEN)) return undefined
        const tail = src.slice(_BB_OPEN.length)
        const m = _BB_ID_RE.exec(tail)
        if (!m) return undefined
        const rest = tail.slice(m[0].length)
        if (!rest.startsWith(']]')) return undefined
        const tok: BbWikiToken = {
          type: 'bbWiki',
          raw: _BB_OPEN + m[0] + ']]',
          id:  m[0],
        }
        return tok
      },
      renderer(token) {
        const t = token as BbWikiToken
        const title = titleById.get(t.id)
        const label = title && title.length > 0 ? title : `${t.id} (deleted?)`
        return `<a href="#bb:${escapeHtml(t.id)}">${escapeHtml(label)}</a>`
      },
    }],
  })
}

/** Render a blackboard entry's markdown to HTML for `v-html`. Mermaid fences are left as
 *  `<pre><code class="language-mermaid">…</code></pre>` — the SFC finds them via `mermaidBlocks`
 *  and replaces them with rendered SVG after a dynamic mermaid import. `titleById` is optional
 *  and used ONLY to resolve `[[bb-…]]` cross-references; omit it and those render as raw text. */
export function renderBlackboardMarkdown(
  md: string | undefined | null,
  titleById?: Map<string, string>,
): string {
  if (!md) return ''
  try {
    if (titleById) return makeMarkedFor(titleById).parse(md, { async: false }) as string
    return marked.parse(md, { async: false }) as string
  } catch { return md }
}

/** Return the source text of every ```mermaid fence in a markdown string, in document order. Used to
 *  decide whether to dynamic-import mermaid at all — no fences ⇒ no ~1 MB paid. */
export function mermaidBlocks(md: string | undefined | null): string[] {
  if (!md) return []
  const out: string[] = []
  // Matches ```mermaid\n…\n``` — trailing whitespace on the fence line tolerated to match GFM.
  const re = /```mermaid[^\n]*\n([\s\S]*?)```/g
  let m: RegExpExecArray | null
  while ((m = re.exec(md)) !== null) out.push(m[1])
  return out
}
