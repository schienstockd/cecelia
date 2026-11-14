#!/usr/bin/env node
//
// Build the user booklets: specs → checks → HTML → PDF.
//
//     pixi run booklets [-- --only c1-using-ai] [--out DIR] [--shots DIR] [--html] [--strict]
//
// Specs live in user-docs/booklets/ (one .ts per booklet, listed in index.ts); the frame they render
// into and the rules they follow are user-docs/booklets/STYLE.md. The build FAILS on a frame rule
// (`checkBooklets`), so a booklet can't ship with an empty "If it goes wrong" or a dangling "Next".
// Every page is stamped with the app version, commit and build date: screenshots go stale, and a
// reader holding a printout needs to know which app it describes.
//
// Screenshots come from `pixi run booklet-shots` (scripts/booklets/capture.mjs): a PNG plus the boxes
// of the anchors it named. Each figure is composed HERE — cropped to the union of its crop anchors at
// one print scale, the pointer drawn on top as SVG in the in-app guide bubble's look — so restyling never
// means recapturing. A step whose shot is missing prints a placeholder; --strict fails instead.
//
// Output (default `booklets-out/`, git-ignored): <id>.pdf per booklet, plus <id>.html with --html for
// looking at the layout in a browser. PDFs are published to the cecelia-booklets repo, not committed
// here (docs/todo/BOOKLETS_PLAN.md → *Where things live*).

import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium, commit, findChromium, repo } from '../lib/chromium.mjs'

const dir = join(repo, 'user-docs', 'booklets')
const { BOOKLETS } = await import(pathToFileURL(join(dir, 'index.ts')).href)
const { checkBooklets, bookletNumbers } = await import(pathToFileURL(join(dir, 'spec.ts')).href)
const { figureGeometry } = await import(pathToFileURL(join(dir, 'figure.ts')).href)

function parseArgs(argv) {
  const a = { out: join(repo, 'booklets-out'), shots: '', only: '', html: false, strict: false }
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i]
    if (k === '--html') a.html = true
    else if (k === '--strict') a.strict = true
    else if (k === '--shots') a.shots = resolve(argv[++i])
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
  const date = new Date().toISOString().slice(0, 10)
  return { version, commit: commit(), date }
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

// ── figures ──────────────────────────────────────────────────────────────────────────────────────
// Crop and pointer placement: user-docs/booklets/figure.ts (pure, tested). Drawn here as SVG over the
// screenshot in the in-app guide bubble's look: a dark ring with a light halo, a dark label.
const missingShots = []
const tooWide = []

function figure(id, shotsDir) {
  const meta = join(shotsDir, `${id}.json`), png = join(shotsDir, `${id}.png`)
  if (!existsSync(meta) || !existsSync(png)) {
    missingShots.push(id)
    return `<figure class="shot missing">Screenshot <code>${esc(id)}</code> not captured yet</figure>`
  }
  const m = JSON.parse(readFileSync(meta, 'utf-8'))
  const { x0, y0, w, h, k, legible, target: t, label: l, arrow: ar } = figureGeometry(m)
  if (!legible) tooWide.push(`${id} (${Math.round(w)} px wide → text at ${Math.round(100 * k / 0.19)}%)`)
  const img = `data:image/png;base64,${readFileSync(png).toString('base64')}`
  const svg = !t ? '' : `<svg class="pointer" viewBox="0 0 ${w} ${h}" preserveAspectRatio="none">
      <defs><marker id="ah-${esc(id)}" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
        <path d="M0,0 L10,5 L0,10 z" fill="#1a1918"/></marker></defs>
      <rect x="${t.x - 5}" y="${t.y - 5}" width="${t.width + 10}" height="${t.height + 10}" rx="7" fill="none" stroke="#fff" stroke-width="7" opacity=".9"/>
      <rect x="${t.x - 5}" y="${t.y - 5}" width="${t.width + 10}" height="${t.height + 10}" rx="7" fill="none" stroke="#1a1918" stroke-width="3"/>
      <line x1="${ar.x1}" y1="${ar.y1}" x2="${ar.x2}" y2="${ar.y2}" stroke="#fff" stroke-width="6" opacity=".9"/>
      <line x1="${ar.x1}" y1="${ar.y1}" x2="${ar.x2}" y2="${ar.y2}" stroke="#1a1918" stroke-width="2.5" marker-end="url(#ah-${esc(id)})"/>
      <rect x="${l.x}" y="${l.y}" width="${l.width}" height="${l.height}" rx="6" fill="#1a1918" stroke="rgba(255,255,255,.6)"/>
      <text x="${l.x + l.width / 2}" y="${l.y + l.height / 2 + 4.5}" text-anchor="middle" font-size="13" font-weight="600" fill="#ece8e3"
        font-family="system-ui, sans-serif">${esc(m.point.label)}</text></svg>`
  return `<figure class="shot" style="width:${(w * k).toFixed(2)}mm;height:${(h * k).toFixed(2)}mm">
    <img src="${img}" alt="" style="left:${(-x0 * k).toFixed(2)}mm;top:${(-y0 * k).toFixed(2)}mm;width:${(m.viewport.width * k).toFixed(2)}mm">
    ${svg}</figure>`
}

function step(s, n, shotsDir) {
  return `<section class="step"><h2><span class="n">${n}</span>${esc(s.title)}${s.optional ? '<span class="opt">optional</span>' : ''}</h2>
    <dl>
      <dt>Do</dt><dd>${inline(s.do)}</dd>
      <dt>You'll see</dt><dd>${inline(s.see)}</dd>
      ${s.why ? `<dt>Why</dt><dd>${inline(s.why)}</dd>` : ''}
      <dt class="wrong">If it goes wrong</dt><dd>${inline(s.ifWrong)}</dd>
    </dl>
    ${s.shot ? figure(s.shot, shotsDir) : ''}
    ${(s.more ?? []).map(block).join('')}</section>`
}

function page(b, num, nums, byId, css, shotsDir) {
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
${b.steps.map((s, i) => step(s, i + 1, shotsDir)).join('\n')}
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
    const html = page(b, nums.get(b.id), nums, byId, css, a.shots || join(a.out, 'shots'))
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
if (tooWide.length) {
  console.warn(`${a.strict ? '✗' : '!'} crop too wide to read in print, crop tighter: ${tooWide.join(', ')}`)
  if (a.strict) process.exitCode = 1
}
if (missingShots.length) {
  console.warn(`${a.strict ? '✗' : '!'} ${missingShots.length} screenshot(s) not captured: ${missingShots.join(', ')} (pixi run booklet-shots)`)
  if (a.strict) process.exit(1)
}
