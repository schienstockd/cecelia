#!/usr/bin/env node
//
// Compose the user docs' screenshots: each capture → one image a page shows.
//
//     pixi run doc-images [-- --only <shot or set id>] [--shots DIR] [--out DIR] [--strict]
//
// Reads what `pixi run doc-shots` (capture.mjs) left in --shots (the capture's --out, default
// `docshots-out/shots`): a PNG plus the boxes of the anchors it named. Each
// image is cropped to the union of its crop anchors at the app's own size (user-docs/shots/figure.ts),
// the pointer drawn on top as SVG in the in-app guide bubble's look, so restyling never means
// recapturing. Writes `<out>/<id>.png` (default `user-docs/images/`, committed: the pages link them) at
// 2× for sharp text, and `<out>/images.json` (id → width in CSS px), which the page generators use
// for the `<img width>` (e.g. frontend/src/utils/usingAiDoc.ts).
//
// Warns (--strict: fails) when a shot is not captured, when the spec changed its crop or pointer target
// since the capture (recapture), or when a crop is too wide for the page column to read.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium, findChromium, repo } from '../lib/chromium.mjs'

const dir = join(repo, 'user-docs', 'shots')
const { SHOT_SETS } = await import(pathToFileURL(join(dir, 'index.ts')).href)
const { checkShotSets } = await import(pathToFileURL(join(dir, 'spec.ts')).href)
const { figureGeometry } = await import(pathToFileURL(join(dir, 'figure.ts')).href)

function parseArgs(argv) {
  const a = { shots: join(repo, 'docshots-out', 'shots'), out: join(repo, 'user-docs', 'images'), only: '', strict: false }
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i]
    if (k === '--strict') a.strict = true
    else if (k === '--shots') a.shots = resolve(argv[++i])
    else if (k === '--out') a.out = resolve(argv[++i])
    else if (k === '--only') a.only = argv[++i]
    else throw new Error(`unknown argument ${k}`)
  }
  return a
}

const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')

/** One composed figure as HTML, `w × h` CSS px; the pointer is the in-app guide bubble's look: a dark
 *  ring with a light halo, a dark label. */
function figureHtml(m, png, g) {
  const { x0, y0, w, h, k, target: t, label: l, arrow: ar } = g
  const img = `data:image/png;base64,${readFileSync(png).toString('base64')}`
  const svg = !t ? '' : `<svg viewBox="0 0 ${w} ${h}" preserveAspectRatio="none" style="position:absolute;inset:0;width:100%;height:100%">
      <defs><marker id="ah" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
        <path d="M0,0 L10,5 L0,10 z" fill="#1a1918"/></marker></defs>
      <rect x="${t.x - 5}" y="${t.y - 5}" width="${t.width + 10}" height="${t.height + 10}" rx="7" fill="none" stroke="#fff" stroke-width="7" opacity=".9"/>
      <rect x="${t.x - 5}" y="${t.y - 5}" width="${t.width + 10}" height="${t.height + 10}" rx="7" fill="none" stroke="#1a1918" stroke-width="3"/>
      ${!l ? '' : `<line x1="${ar.x1}" y1="${ar.y1}" x2="${ar.x2}" y2="${ar.y2}" stroke="#fff" stroke-width="6" opacity=".9"/>
      <line x1="${ar.x1}" y1="${ar.y1}" x2="${ar.x2}" y2="${ar.y2}" stroke="#1a1918" stroke-width="2.5" marker-end="url(#ah)"/>
      <rect x="${l.x}" y="${l.y}" width="${l.width}" height="${l.height}" rx="6" fill="#1a1918" stroke="rgba(255,255,255,.6)"/>
      <text x="${l.x + l.width / 2}" y="${l.y + l.height / 2 + 4.5}" text-anchor="middle" font-size="13" font-weight="600" fill="#ece8e3"
        font-family="system-ui, sans-serif">${esc(m.point.label)}</text>`}</svg>`
  return `<!doctype html><html><head><meta charset="utf-8"><style>html,body{margin:0;background:#fff}</style></head><body>
    <figure id="f" style="margin:0;position:relative;overflow:hidden;width:${(w * k).toFixed(2)}px;height:${(h * k).toFixed(2)}px">
    <img src="${img}" alt="" style="position:absolute;left:${(-x0 * k).toFixed(2)}px;top:${(-y0 * k).toFixed(2)}px;width:${(m.viewport.width * k).toFixed(2)}px">
    ${svg}</figure></body></html>`
}

const a = parseArgs(process.argv.slice(2))
const issues = checkShotSets(SHOT_SETS)
if (issues.length) {
  for (const i of issues) console.error(`✗ ${i.set}: ${i.problem}`)
  process.exit(1)
}
const shots = SHOT_SETS.flatMap(s => s.shots.filter(x => !a.only || a.only === x.id || a.only === s.id))
if (a.only && shots.length === 0) throw new Error(`no shot or set "${a.only}"`)
mkdirSync(a.out, { recursive: true })
const metaPath = join(a.out, 'images.json')
const widths = existsSync(metaPath) ? JSON.parse(readFileSync(metaPath, 'utf-8')) : {}
const missing = [], stale = [], tooWide = []

const browser = await chromium.launch({ executablePath: findChromium(), headless: true })
try {
  const page = await browser.newPage({ deviceScaleFactor: 2 })
  for (const spec of shots) {
    const meta = join(a.shots, `${spec.id}.json`), png = join(a.shots, `${spec.id}.png`)
    if (!existsSync(meta) || !existsSync(png)) { missing.push(spec.id); continue }
    const m = JSON.parse(readFileSync(meta, 'utf-8'))
    // The pointer's label and side come from the spec, so rewording one never needs a recapture; only
    // the boxes are the capture's. A different target or crop does need one.
    if (spec.point?.anchor !== m.point?.anchor || spec.crop.join() !== m.crop.join()) stale.push(spec.id)
    else if (spec.point) m.point = spec.point
    const g = figureGeometry(m)
    if (!g.legible) tooWide.push(`${spec.id} (${Math.round(g.w)} px wide → text at ${Math.round(100 * g.k)}%)`)
    await page.setContent(figureHtml(m, png, g), { waitUntil: 'load' })
    await page.locator('#f').screenshot({ path: join(a.out, `${spec.id}.png`) })
    widths[spec.id] = Math.round(g.w * g.k)
    console.log(`✓ ${spec.id} → ${join(a.out, `${spec.id}.png`)}`)
  }
} finally {
  await browser.close()
}
writeFileSync(metaPath, JSON.stringify(Object.fromEntries(Object.entries(widths).sort()), null, 1) + '\n')
const warn = (list, what) => {
  if (!list.length) return
  console.warn(`${a.strict ? '✗' : '!'} ${what}: ${list.join(', ')}`)
  if (a.strict) process.exitCode = 1
}
warn(tooWide, 'crop too wide to read on the page, crop tighter')
warn(stale, 'spec changed the pointer target or crop since capture, recapture')
warn(missing, 'not captured (pixi run doc-shots)')
