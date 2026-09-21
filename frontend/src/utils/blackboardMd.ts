// Markdown → HTML for BlackboardModule + a helper that names the ```mermaid fences the SFC needs to
// render out-of-band (mermaid is a ~1 MB dep that only pays off when a diagram is actually present,
// so the module dynamic-imports it lazily). Kept out of the SFC so the pure logic is testable
// (frontend rule: pure logic in `utils/*.ts`).
//
// Trusted source — same as WhatNewCard.vue. A blackboard entry is written by (a) Claude via MCP into
// this user's own project on this user's own machine and (b) the user editing the textarea. No third
// party writes it, so we don't sanitize; if a `.ccbundle` from an untrusted source ever becomes a
// distribution surface, revisit this call site.

import { marked } from 'marked'

marked.setOptions({ gfm: true, breaks: false })

/** `[[bb-<ts>-<hex>]]` and `[[profile]]` — internal wiki-style links to other Blackboard entries.
 *  Claude writes these naturally when cross-referencing (e.g. "see [[bb-…]] for the pipeline");
 *  we swap them for real Markdown links to a `#bb:<id>` fragment so the SFC can intercept a click
 *  and select the target entry. Absent from the title map (linked entry deleted) → still linked
 *  by id so the reader knows the reference existed. Kept as its own pass over the raw markdown so
 *  the resolution runs BEFORE marked sees `[…]` — no need to fight escape rules mid-render. */
const _BB_WIKI_RE = /\[\[(profile|bb-[0-9]{8}T[0-9]{6}-[0-9a-f]{6})\]\]/g

export function resolveBlackboardWikiLinks(md: string, titleById: Map<string, string>): string {
  return md.replace(_BB_WIKI_RE, (_full, id: string) => {
    const title = titleById.get(id)
    const label = title && title.length > 0 ? title : `${id} (deleted?)`
    // Markdown-escape the closing `]` in the label — a title containing `]` would end the link
    // early. Nothing else needs escaping since marked will process this like any other `[text](url)`.
    const safeLabel = label.replace(/[[\]]/g, '\\$&')
    return `[${safeLabel}](#bb:${id})`
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
  const resolved = titleById ? resolveBlackboardWikiLinks(md, titleById) : md
  try { return marked.parse(resolved, { async: false }) as string }
  catch { return resolved }
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
