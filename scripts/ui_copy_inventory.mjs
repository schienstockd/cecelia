#!/usr/bin/env node
//
// Dump every front-facing string in the app into one readable list, plus the drift signals over it.
//
//     pixi run ui-copy              # → UI_COPY_INVENTORY.md at the repo root (git-ignored)
//     node scripts/ui_copy_inventory.mjs [outfile]
//
// WHY THIS EXISTS. UI copy lives in three places with three different structures — inline literals
// in 107 SFCs, keyed `label`/`tip` fields in `app/src/tasks/**/*.json`, and `qc_finding()` arguments
// inside Julia analysis functions. Nothing spanned them, so wording drifted: task specs went Title
// Case while the frontend stayed sentence case, 50 phrases acquired a second spelling, and the same
// action picked up three verbs. The 90-character ratchet (`uiCopy.test.ts` + the task-spec testset in
// `app/test/runtests.jl`) covers three of the thirteen surfaces; this covers all of them, for reading
// rather than for failing a build.
//
// PARSING LIVES IN `frontend/src/utils/uiCopy.ts`, NOT HERE. That module is the canonical UI-copy
// parser and is unit-tested; this script imports it (Node ≥22 strips the types on the fly). The only
// parsing that is local is the Julia one, because nothing else in the repo reads Julia call sites.
// If you need a new extractor, add it there and consume it here — do not grow a second parser.

import { readFileSync, writeFileSync, readdirSync, statSync } from 'node:fs'
import { join, relative, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'
import { execFileSync } from 'node:child_process'

import {
  normalise, isTooLong, isMultiSentence, isTitleCase,
  tooltipStrings, hintStrings, attrStrings, textStrings, uncoveredControls,
} from '../frontend/src/utils/uiCopy.ts'

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..')
const OUT = process.argv[2] ?? join(ROOT, 'UI_COPY_INVENTORY.md')

function walk(dir, pred, out = []) {
  let entries
  try { entries = readdirSync(dir) } catch { return out }
  for (const e of entries) {
    if (e === 'node_modules' || e === '.git' || e === '.pixi') continue
    const p = join(dir, e)
    if (statSync(p).isDirectory()) walk(p, pred, out)
    else if (pred(p)) out.push(p)
  }
  return out
}

const rows = []
const add = (text, kind, file) => {
  const t = normalise(text)
  if (t && !/^[…\s\W]*$/.test(t)) rows.push({ text: t, kind, file })
}

// ── A. frontend SFCs ─────────────────────────────────────────────────────────────────────────────
const EXTRACTORS = [
  [tooltipStrings, 'tooltip'], [hintStrings, 'hint'], [attrStrings, 'attr'], [textStrings, 'text'],
]
// COVERAGE, not copy: the controls with no hover help at all. Collected here rather than in `rows`
// because it is the absence of a string — it has no text to dedupe, group or measure.
const bare = []
const covered = { with: 0, without: 0 }

for (const path of walk(join(ROOT, 'frontend/src'), (p) => p.endsWith('.vue'))) {
  const src = readFileSync(path, 'utf8')
  const rel = relative(ROOT, path)
  for (const [fn, kind] of EXTRACTORS) for (const s of fn(src)) add(s, kind, rel)
  for (const c of uncoveredControls(src, rel)) bare.push(`${rel}:${c.line} <${c.tag}>`)
  for (const m of src.matchAll(/(?:summary|detail|message)\s*:\s*("|'|`)([\s\S]*?)\1/g))
    add(m[2].replace(/\$\{[^}]*\}/g, '…'), 'toast', rel)
}
// Toasts are also built in plain `.ts` stores/helpers, which have no template to parse.
for (const path of walk(join(ROOT, 'frontend/src'), (p) => p.endsWith('.ts') && !p.endsWith('.test.ts'))) {
  const src = readFileSync(path, 'utf8')
  for (const m of src.matchAll(/(?:summary|detail|message)\s*:\s*("|'|`)([\s\S]*?)\1/g))
    add(m[2].replace(/\$\{[^}]*\}/g, '…'), 'toast', relative(ROOT, path))
}

// ── B. task specs + plot definitions (already a keyed catalog) ───────────────────────────────────
//
// `docs/examples/custom-modules` is in here because those ARE task specs — `load_custom_modules!`
// loads them and `ParamRenderer` renders them; they live in `docs/` only because they are the
// template a user copies to write a drop-in module. Coverage below is required for both, which is
// why their 7 tips got swept to the house style along with everything else.
const TIPPED_DIRS = ['app/src/tasks', 'docs/examples/custom-modules']
for (const dir of [...TIPPED_DIRS, 'app/src/plotDefinitions']) {
  for (const path of walk(join(ROOT, dir), (p) => p.endsWith('.json'))) {
    const rel = relative(ROOT, path)
    let spec
    try { spec = JSON.parse(readFileSync(path, 'utf8')) } catch { continue }
    if (spec.label) add(spec.label, 'task:label', rel)
    if (spec.category) add(spec.category, 'task:category', rel)
    // `tip`s nest inside section/group params, so recurse (mirrors `collect_tips!` in the Julia suite).
    const visit = (ps) => {
      for (const p of Array.isArray(ps) ? ps : Object.values(ps ?? {})) {
        if (!p || typeof p !== 'object') continue
        if (p.label) add(p.label, 'param:label', rel)
        if (p.tip) add(p.tip, 'param:tip', rel)
        // Sections and groups are container HEADERS, not inputs — exempt, same as the Julia ratchet.
        // `plotDefinitions` is exempt WHOLESALE: its `params` array is a defaults bag, not a form
        // (`SummaryPanel.vue` reads only `.default`), so a tip there would render to nobody. Its
        // strings still appear in the inventory below — visible, just not required.
        if (p.key && p.type !== 'section' && p.type !== 'group' && TIPPED_DIRS.includes(dir)) {
          if (String(p.tip ?? '').trim()) covered.with++
          else { covered.without++; bare.push(`${rel} — param \`${p.key}\` has no tip`) }
        }
        visit(p.params ?? p.items ?? [])
      }
    }
    visit(spec.params ?? [])
  }
}

// ── C. QC findings (Julia) ───────────────────────────────────────────────────────────────────────
//
// All QC copy lives in the `QC_TEXT` catalog in `app/src/qc.jl` — one `"key" => (short = …, long = …)`
// entry per finding. Parse that, not the `qc_finding()` call sites: the call sites now pass only a
// code, so scraping them silently returned ZERO strings and the inventory quietly lost 40 of them
// (caught only because the count moved). Hence the assertion below — a structural change to the
// catalog should fail loudly here rather than shrink the report.
//
// And do NOT fall back to grepping every `"…"` in the file: that sweeps up docstrings and matches
// from one string's CLOSING quote to the next string's OPENING quote, emitting raw Julia as copy.
const QC_SOURCE = 'app/src/qc.jl'
let qcFound = 0
try {
  const src = readFileSync(join(ROOT, QC_SOURCE), 'utf8')
  const catalog = src.match(/const QC_TEXT[\s\S]*?\n\)\n/)?.[0] ?? ''
  const ENTRY = /short\s*=\s*"((?:[^"\\]|\\.)*)"\s*,\s*\n?\s*long\s*=\s*"((?:[^"\\]|\\.)*)"/g
  for (const m of catalog.matchAll(ENTRY)) {
    // `{name}` is filled at emit time; its width is unknowable here, same as `${…}` in a tooltip.
    for (const t of [m[1], m[2]]) { add(t.replace(/\{\w+\}/g, '…'), 'qc', QC_SOURCE); qcFound++ }
  }
} catch { /* file missing — the assertion below reports it */ }
if (qcFound === 0) {
  console.error(`WARNING: no QC copy parsed from ${QC_SOURCE}. Did QC_TEXT move or change shape?`)
  process.exitCode = 1
}

// ── D. What's-New / tip cards ────────────────────────────────────────────────────────────────────
//
// `frontend/src/lib/tips.ts` is a plain data module, so this IMPORTS it rather than parsing it — there
// is no markup to scrape, and a regex over quoted fields would just reintroduce escaping bugs.
// (`tips.ts` only `import type`s from `whatsNew`, and type imports are erased, so this pulls in no Vue
// runtime.) Copy fields per `WhatNewCard`: title / description / bodyMd / steps.
//
// VISIBILITY ONLY — deliberately not ratcheted, and excluded from the length signal below. These are
// long-form explainers with a sketch; there is no agreed length, tone or punctuation rule to hold them
// to, and a guard without a rule just grows an allow-list until it stops meaning anything. They are
// here so the copy can be READ alongside the rest of the app, and judged by a person.
const CARD_KINDS = new Set(['card:title', 'card:body'])
let cardsFound = 0
try {
  const { TIPS } = await import(join(ROOT, 'frontend/src/lib/tips.ts'))
  for (const card of TIPS ?? []) {
    const at = `frontend/src/lib/tips.ts (${card.id})`
    if (card.title) { add(card.title, 'card:title', at); cardsFound++ }
    for (const body of [card.description, card.bodyMd, ...(card.steps ?? [])]) {
      if (body) { add(body, 'card:body', at); cardsFound++ }
    }
  }
} catch (e) {
  console.error(`WARNING: could not read tip cards — ${e.message.split('\n')[0]}`)
  process.exitCode = 1
}
if (cardsFound === 0) {
  console.error('WARNING: no tip-card copy found. Did TIPS move or change shape?')
  process.exitCode = 1
}

// ── dedupe ───────────────────────────────────────────────────────────────────────────────────────
const uniq = new Map()
for (const r of rows) {
  const key = `${r.kind} ${r.text}`
  if (!uniq.has(key)) uniq.set(key, { ...r, files: new Set() })
  uniq.get(key).files.add(r.file)
}
const all = [...uniq.values()]
const where = (r) => [...r.files].sort()[0]

// ── drift signals ────────────────────────────────────────────────────────────────────────────────
//
// `docs/UI.md` states two mechanical rules (90 characters, one sentence) and enforces them on three
// surfaces: `v-tooltip`, `hint`, task-JSON `tip`. Everything else below is reported as an
// INCONSISTENCY — the minority spelling of a choice the codebase already made implicitly — not as a
// violation of anything written down. Keep that distinction when reading the report.
const ENFORCED = new Set(['tooltip', 'hint', 'param:tip'])
const isLabelKind = (k) => k === 'attr' || k === 'task:label' || k === 'param:label'
const minorityOf = (rs, pred) => {
  const yes = rs.filter(pred)
  return yes.length * 2 <= rs.length ? yes : rs.filter((r) => !pred(r))
}

const signals = {
  'Over budget on an ENFORCED surface (the ratchet holds these — expect zero)':
    all.filter((r) => ENFORCED.has(r.kind) && isTooLong(r.text)).map((r) => `[${r.text.length}] ${r.text} — ${where(r)}`),
  'Second sentence on an ENFORCED surface (expect zero)':
    all.filter((r) => ENFORCED.has(r.kind) && isMultiSentence(r.text)).map((r) => `${r.text} — ${where(r)}`),
  // Tip cards are excluded on purpose: a long-form surface with no agreed length rule, so listing 13
  // permanent entries nobody will ever act on would only erode this signal.
  'Over budget on an UNENFORCED surface (review question, not a build failure)':
    all.filter((r) => !ENFORCED.has(r.kind) && !CARD_KINDS.has(r.kind) && isTooLong(r.text))
       .map((r) => `[${r.text.length}] ${r.text} — ${where(r)}`),
  // The presence half of the rule. Everything else in this report reads copy that EXISTS; this is
  // the copy that doesn't — a settable control or task param a user can change with no explanation
  // anywhere on it. Ratcheted (uiCopy.test.ts + app/test/runtests.jl), so expect zero.
  'Settable control or task param with NO tooltip (ratcheted — expect zero)': bare,
  'Capitalisation — the minority style among labels':
    minorityOf(all.filter((r) => isLabelKind(r.kind)), (r) => isTitleCase(r.text)).map((r) => `${r.text} — ${where(r)}`),
  'Trailing period on tooltips — the minority style':
    minorityOf(all.filter((r) => r.kind === 'tooltip'), (r) => /[^.]\.$/.test(r.text)).map((r) => `${r.text} — ${where(r)}`),
}

// Same words, different rendering — casing, punctuation and word order all normalised away.
const byShape = new Map()
for (const r of all) {
  const k = r.text.toLowerCase().replace(/[^a-z0-9 ]/g, '').split(' ').filter(Boolean).sort().join(' ')
  if (!k) continue
  if (!byShape.has(k)) byShape.set(k, [])
  byShape.get(k).push(r)
}
signals['Same phrase written more than one way'] = [...byShape.values()]
  .filter((g) => new Set(g.map((r) => r.text)).size > 1)
  .map((g) => [...new Set(g.map((r) => `"${r.text}" (${r.kind})`))].join('  vs  '))

// Competing verbs for one action. Counts, not locations — the fix is a vocabulary decision.
const VERBS = [['Select', 'Choose', 'Pick'], ['Delete', 'Remove', 'Discard'], ['Edit', 'Change', 'Modify'],
               ['Show', 'Display', 'View'], ['Add', 'Create', 'New'], ['Run', 'Start', 'Execute', 'Launch']]
signals['Competing verbs for the same action'] = VERBS.flatMap((group) => {
  const hits = group
    .map((v) => ({ v, n: all.filter((r) => new RegExp(`\\b${v}\\b`, 'i').test(r.text)).length }))
    .filter((h) => h.n > 0)
  return hits.length > 1 ? [hits.map((h) => `${h.v} ×${h.n}`).join('  /  ')] : []
})

// ── report ───────────────────────────────────────────────────────────────────────────────────────
const byKind = new Map()
for (const r of all) {
  if (!byKind.has(r.kind)) byKind.set(r.kind, [])
  byKind.get(r.kind).push(r)
}
const kinds = [...byKind].sort((a, b) => b[1].length - a[1].length)

// Stamp WHAT this reflects, not just when. The report is git-ignored and regenerated on demand, so
// the failure mode is reading a stale one and thinking it is current — a commit + dirty flag makes
// that visible at a glance.
const stamp = (() => {
  const git = (...a) => execFileSync('git', a, { cwd: ROOT, encoding: 'utf8' }).trim()
  try {
    const dirty = git('status', '--porcelain').length > 0
    return `\`${git('rev-parse', '--short', 'HEAD')}\`${dirty ? ' + uncommitted changes' : ''}`
  } catch {
    return '_unknown revision_'
  }
})()

let md = '# Front-facing copy inventory\n\n'
md += 'Generated by `pixi run ui-copy` (`scripts/ui_copy_inventory.mjs`). Do not edit — regenerate.\n\n'
md += `Reflects ${stamp}. Regenerate before reviewing; the ratchet in \`uiCopy.test.ts\` +\n`
md += '`app/test/runtests.jl` is what runs automatically in CI.\n\n'
md += '| kind | unique strings |\n|---|---|\n'
for (const [k, v] of kinds) md += `| ${k} | ${v.length} |\n`
md += `| **total** | **${all.length}** |\n\n`
md += `Task-param tip coverage: **${covered.with}/${covered.with + covered.without}** `
md += `(sections and groups are container headers and exempt).\n\n## Drift signals\n\n`
md += 'Two of these are advisory and run noisy on purpose — read them, do not treat them as a list of\n'
md += 'bugs. *Same phrase written more than one way* normalises case away, which is what catches a\n'
md += 'genuine `Cecelia`/`cecelia` slip but also flags a heading against the same word used mid-sentence.\n'
md += '*Competing verbs* counts the verb in prose as well as in labels. Both also see string literals\n'
md += 'that sit inside a binding expression without being copy (`kind === \'cecelia\'`), because\n'
md += '`tooltipStrings` extracts literals rather than evaluating the expression.\n\n'
for (const [name, items] of Object.entries(signals)) {
  md += `### ${name} — ${items.length}\n\n`
  md += items.length ? items.map((i) => `- ${i}`).join('\n') + '\n\n' : '_none_\n\n'
}
md += '## Every string, by kind\n\n'
for (const [kind, items] of kinds) {
  md += `### ${kind} (${items.length})\n\n`
  for (const r of [...items].sort((a, b) => a.text.localeCompare(b.text)))
    md += `- ${r.text}  \`${where(r)}\`\n`
  md += '\n'
}

writeFileSync(OUT, md)
console.log(`${all.length} unique strings → ${relative(process.cwd(), OUT)}`)
for (const [k, v] of kinds) console.log(`  ${String(v.length).padStart(5)}  ${k}`)
console.log('')
for (const [n, i] of Object.entries(signals)) console.log(`  ${String(i.length).padStart(5)}  ${n}`)
