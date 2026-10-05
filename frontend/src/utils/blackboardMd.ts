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

// ── Entry sections — each carries its own verdict ─────────────────────────────────────────────
// AGENT_RUN_REVIEW_PLAN P2. A run record has one `### dNN · step · image · …` section per decision
// (`mNN`: a miss a reviewer added); any other entry may have `### sNN · …` sections its author wrote,
// one per claim. The Blackboard renders such an entry part by part so each section carries its own
// verdict control. A section runs from its heading to the next heading of level ≤ 3.

export const RUN_STEPS = ['cleanup', 'segment', 'measure', 'gate', 'track', 'behaviour', 'report'] as const
export type RunStep = typeof RUN_STEPS[number]

export type EntryPart =
  | { kind: 'md'; md: string }
  | { kind: 'section'; id: string; md: string }

const _SECTION_HEAD_RE = /^### ([dms][0-9]{2,3}) · /
const _ANY_HEAD_RE = /^#{1,3} /

/** Split an entry's markdown into plain parts and sections, in order. An entry with no
 *  `### dNN ·` / `mNN` / `sNN` heading comes back as one `md` part. A repeated id is not a second
 *  section — its heading starts a plain part (the server's `_bb_section_ids` counts it once too). */
export function splitEntrySections(md: string | undefined | null): EntryPart[] {
  if (!md) return []
  const parts: EntryPart[] = []
  let cur: EntryPart = { kind: 'md', md: '' }
  let inFence = false
  const seen = new Set<string>()
  for (const line of md.split('\n')) {
    if (line.startsWith('```')) inFence = !inFence
    let sec = inFence ? null : _SECTION_HEAD_RE.exec(line)
    if (sec && seen.has(sec[1])) sec = null
    if (sec) seen.add(sec[1])
    if (sec || (!inFence && cur.kind === 'section' && _ANY_HEAD_RE.test(line))) {
      if (cur.md.trim()) parts.push(cur)
      cur = sec ? { kind: 'section', id: sec[1], md: '' } : { kind: 'md', md: '' }
    }
    cur.md += (cur.md ? '\n' : '') + line
  }
  if (cur.md.trim()) parts.push(cur)
  return parts
}

/** Append a reviewer's missed decision as the next `mNN` section, right after the last decision.
 *  Returns the new markdown and the section id. */
export function appendMissSection(
  md: string, step: RunStep, image: string, text: string,
): { md: string; id: string } {
  const parts = splitEntrySections(md)
  const ids = parts.flatMap(p => p.kind === 'section' && p.id.startsWith('m') ? [Number(p.id.slice(1))] : [])
  const id = `m${String((ids.length ? Math.max(...ids) : 0) + 1).padStart(2, '0')}`
  const section: EntryPart = {
    kind: 'section', id,
    md: `### ${id} · ${step} · ${image.trim() || 'all'} · missed\n- **Should have:** ${text.trim()}\n`,
  }
  let last = -1
  parts.forEach((p, i) => { if (p.kind === 'section') last = i })
  parts.splice(last + 1, 0, section)
  return { md: parts.map(p => p.md.replace(/\n+$/, '')).join('\n\n') + '\n', id }
}
