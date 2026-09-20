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

/** Render a blackboard entry's markdown to HTML for `v-html`. Mermaid fences are left as
 *  `<pre><code class="language-mermaid">…</code></pre>` — the SFC finds them via `mermaidBlocks`
 *  and replaces them with rendered SVG after a dynamic mermaid import. */
export function renderBlackboardMarkdown(md: string | undefined | null): string {
  if (!md) return ''
  try { return marked.parse(md, { async: false }) as string }
  catch { return md }
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
