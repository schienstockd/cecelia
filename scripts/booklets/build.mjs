#!/usr/bin/env node
//
// Build the user booklets: specs → checks → HTML → PDF.
//
//     pixi run booklets [-- --only c1-using-ai] [--out DIR] [--html]
//
// Specs live in user-docs/booklets/ (one .ts per booklet, listed in index.ts); the frame they render
// into and the rules they follow are user-docs/booklets/STYLE.md. The build FAILS on a frame rule
// (`checkBooklets`), so a booklet can't ship with an empty "If it goes wrong" or a dangling "Next".
// Every page is stamped with the app version, commit and build date: screenshots go stale, and a
// reader holding a printout needs to know which app it describes.
//
// Output (default `booklets-out/`, git-ignored): <id>.pdf per booklet, plus <id>.html with --html for
// looking at the layout in a browser. PDFs are published to the cecelia-booklets repo, not committed
// here (docs/todo/BOOKLETS_PLAN.md → *Where things live*).

import { execFileSync } from 'node:child_process'
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium, findChromium, repo } from '../lib/chromium.mjs'

const dir = join(repo, 'user-docs', 'booklets')
const { BOOKLETS } = await import(pathToFileURL(join(dir, 'index.ts')).href)
const { checkBooklets, bookletNumbers } = await import(pathToFileURL(join(dir, 'spec.ts')).href)

function parseArgs(argv) {
  const a = { out: join(repo, 'booklets-out'), only: '', html: false }
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i]
    if (k === '--html') a.html = true
    else if (k === '--out') a.out = resolve(argv[++i])
    else if (k === '--only') a.only = argv[++i]
    else throw new Error(`unknown argument ${k}`)
  }
  return a
}

// ── stamp ────────────────────────────────────────────────────────────────────────────────────────
function stamp() {
  const toml = readFileSync(join(repo, 'app', 'Project.toml'), 'utf-8')
  const version = toml.match(/^version\s*=\s*"([^"]+)"/m)?.[1] ?? '?'
  let commit = '?'
  try { commit = execFileSync('git', ['-C', repo, 'rev-parse', '--short=9', 'HEAD'], { encoding: 'utf-8' }).trim() } catch {}
  const date = new Date().toISOString().slice(0, 10)
  return { version, commit, date }
}

// ── inline text ──────────────────────────────────────────────────────────────────────────────────
const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')
/** `**App label**` → a UI-label chip; `` `cmd` `` → code. Nothing else is markup. */
const inline = s => esc(s)
  .replace(/`([^`]+)`/g, '<code>$1</code>')
  .replace(/\*\*([^*]+)\*\*/g, '<b class="ui">$1</b>')

// ── blocks ───────────────────────────────────────────────────────────────────────────────────────
function block(b) {
  switch (b.kind) {
    case 'para': return `<p>${inline(b.text)}</p>`
    case 'list': return `<ul>${b.items.map(i => `<li>${inline(i)}</li>`).join('')}</ul>`
    case 'note': return `<div class="note ${b.tone}">${inline(b.text)}</div>`
    case 'table': return `<table><thead><tr>${b.head.map(h => `<th>${esc(h)}</th>`).join('')}</tr></thead>
      <tbody>${b.rows.map(r => `<tr>${r.map(c => `<td>${inline(c)}</td>`).join('')}</tr>`).join('')}</tbody></table>`
    case 'terms': return `<dl class="terms">${b.terms.map(t => `<dt>${esc(t.term)}</dt><dd>${inline(t.means)}</dd>`).join('')}</dl>`
    case 'cards': return `<div class="cards">${b.cards.map(c => `
      <div class="card ${c.tone ?? 'neutral'}"><h3>${esc(c.title)}</h3>
        ${c.lead ? `<p class="lead">${inline(c.lead)}</p>` : ''}
        <ul>${c.lines.map(l => `<li>${inline(l)}</li>`).join('')}</ul>
        ${c.foot ? `<p class="foot">${inline(c.foot)}</p>` : ''}</div>`).join('')}</div>`
    default: throw new Error(`unknown block kind ${b.kind}`)
  }
}

function step(s, n) {
  return `<section class="step"><h2><span class="n">${n}</span>${esc(s.title)}${s.optional ? '<span class="opt">optional</span>' : ''}</h2>
    <dl>
      <dt>Do</dt><dd>${inline(s.do)}</dd>
      <dt>You'll see</dt><dd>${inline(s.see)}</dd>
      ${s.why ? `<dt>Why</dt><dd>${inline(s.why)}</dd>` : ''}
      <dt class="wrong">If it goes wrong</dt><dd>${inline(s.ifWrong)}</dd>
    </dl>
    ${s.shot ? `<figure class="shot" data-shot="${esc(s.shot)}"></figure>` : ''}
    ${(s.more ?? []).map(block).join('')}</section>`
}

function page(b, num, nums, byId, css) {
  const next = b.next ? byId.get(b.next) : null
  return `<!doctype html><html lang="en"><head><meta charset="utf-8"><title>${esc(num)} ${esc(b.title)}</title>
<style>${css}</style></head><body>
<header class="cover">
  <div class="num">${esc(num)}</div>
  <div><h1>${esc(b.title)}</h1>${b.subtitle ? `<p class="sub">${inline(b.subtitle)}</p>` : ''}</div>
</header>
<div class="facts">
  <div><h4>For</h4><p>${inline(b.audience)}</p></div>
  <div><h4>You get</h4><p>${inline(b.get)}</p></div>
  <div><h4>You need</h4><ul>${b.need.map(x => `<li>${inline(x)}</li>`).join('')}</ul></div>
  <div><h4>Ends at</h4><p>${inline(b.endsAt)}</p></div>
  ${b.sample ? `<div class="wide"><h4>Sample</h4><p>${inline(b.sample)}</p></div>` : ''}
</div>
${b.beforeYouImage?.length ? `<section class="before"><h2>Before you image</h2>
  <table><thead><tr><th>At the microscope</th><th>What it limits later</th><th>Where Cecelia shows it</th></tr></thead>
  <tbody>${b.beforeYouImage.map(r => `<tr><td>${inline(r.setting)}</td><td>${inline(r.limits)}</td><td>${inline(r.shows)}</td></tr>`).join('')}</tbody></table></section>` : ''}
${(b.intro ?? []).map(block).join('\n')}
${b.steps.map((s, i) => step(s, i + 1)).join('\n')}
${b.askAi ? `<aside class="askai"><h4>Ask AI <span>optional · AI is off until you turn it on</span></h4>
  <p class="prompt">“${inline(b.askAi.prompt)}”</p><p class="where">${inline(b.askAi.where)}</p></aside>` : ''}
${next ? `<p class="next">Next: <b>${esc(nums.get(next.id))} ${esc(next.title)}</b></p>` : ''}
</body></html>`
}

// ── main ─────────────────────────────────────────────────────────────────────────────────────────
const a = parseArgs(process.argv.slice(2))
const issues = checkBooklets(BOOKLETS)
if (issues.length) {
  for (const i of issues) console.error(`✗ ${i.booklet}: ${i.problem}`)
  process.exit(1)
}
const nums = bookletNumbers(BOOKLETS)
const byId = new Map(BOOKLETS.map(b => [b.id, b]))
const todo = a.only ? BOOKLETS.filter(b => b.id === a.only) : BOOKLETS
if (a.only && todo.length === 0) throw new Error(`no booklet "${a.only}"`)
const css = readFileSync(join(dir, 'booklet.css'), 'utf-8')
const st = stamp()
mkdirSync(a.out, { recursive: true })

const browser = await chromium.launch({ executablePath: findChromium(), headless: true })
try {
  for (const b of todo) {
    const html = page(b, nums.get(b.id), nums, byId, css)
    if (a.html) writeFileSync(join(a.out, `${b.id}.html`), html, 'utf-8')
    const pg = await browser.newPage()
    await pg.setContent(html, { waitUntil: 'load' })
    const foot = `<div style="font: 7pt system-ui, sans-serif; color: #635d57; width: 100%; padding: 0 14mm;
      display: flex; justify-content: space-between;"><span>Cecelia ${esc(st.version)} · ${esc(st.commit)} · ${esc(st.date)}</span>
      <span>${esc(nums.get(b.id))} ${esc(b.title)} · <span class="pageNumber"></span>/<span class="totalPages"></span></span></div>`
    await pg.pdf({
      path: join(a.out, `${b.id}.pdf`), format: 'A4', printBackground: true,
      displayHeaderFooter: true, headerTemplate: '<span></span>', footerTemplate: foot,
      margin: { top: '14mm', bottom: '16mm', left: '14mm', right: '14mm' },
    })
    await pg.close()
    console.log(`✓ ${nums.get(b.id)} ${b.id} → ${join(a.out, `${b.id}.pdf`)}`)
  }
} finally {
  await browser.close()
}
