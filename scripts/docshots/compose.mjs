#!/usr/bin/env node
//
// Compose the user docs' screenshots: each capture → one image a page shows.
//
//     pixi run doc-images [-- --only <shot or set id>] [--shots DIR] [--out DIR] [--strict]
//
// Reads what `pixi run doc-shots` (capture.mjs) left in --shots (the capture's --out, default
// `docshots-out/shots`): a PNG per shot and colour scheme, plus the boxes of the anchors it named. Each
// image is cropped to the union of its crop anchors at the app's own size (user-docs/shots/figure.ts),
// the pointer drawn on top as SVG in the in-app guide bubble's look, in the image's own scheme (a dark
// ring on a light image, a light one on a dark), so restyling never means recapturing. Writes
// `<out>/<file>` (spec.ts → shotFile; default `user-docs/images/`, committed: the pages and the app
// show them) at 2× for sharp text, `<out>/images.json` (file stem → width in CSS px), which the page
// generators use for the `<img width>` (e.g. frontend/src/utils/usingAiDoc.ts), and `<out>/facts.json`
// (set → the fixture's measured facts), which fill the captions.
//
// Fails when a capture is in another scheme than its set asks for: one set never mixes schemes.
// Warns (--strict: fails) when a shot is not captured, when the spec changed its crop or pointer target
// since the capture (recapture), or when a crop is too wide for the page column to read.

import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium, findChromium, repo } from '../lib/chromium.mjs'

const dir = join(repo, 'user-docs', 'shots')
const { SHOT_SETS } = await import(pathToFileURL(join(dir, 'index.ts')).href)
const { checkShotSets, imageFile, imageStem, isPicture, setImages, shotFile, shotThemes } = await import(pathToFileURL(join(dir, 'spec.ts')).href)
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

/** The pointer's colours per scheme: ink on the image's own paper would vanish, so a light image gets a
 *  dark ring with a light halo and a dark label, a dark image the reverse. */
const POINTER = {
  light: { ink: '#1a1918', halo: '#fff', labelText: '#ece8e3', labelEdge: 'rgba(255,255,255,.6)', paper: '#fff' },
  dark: { ink: '#ece8e3', halo: '#1a1918', labelText: '#1a1918', labelEdge: 'rgba(0,0,0,.6)', paper: '#1a1918' },
}

/** One composed figure as HTML, `w × h` CSS px; the pointer is the in-app guide bubble's look. */
function figureHtml(m, png, g, theme) {
  const { x0, y0, w, h, k, target: t, label: l, arrow: ar } = g
  const c = POINTER[theme]
  const img = `data:image/png;base64,${readFileSync(png).toString('base64')}`
  const svg = !t ? '' : `<svg viewBox="0 0 ${w} ${h}" preserveAspectRatio="none" style="position:absolute;inset:0;width:100%;height:100%">
      <defs><marker id="ah" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
        <path d="M0,0 L10,5 L0,10 z" fill="${c.ink}"/></marker></defs>
      <rect x="${t.x - 5}" y="${t.y - 5}" width="${t.width + 10}" height="${t.height + 10}" rx="7" fill="none" stroke="${c.halo}" stroke-width="7" opacity=".9"/>
      <rect x="${t.x - 5}" y="${t.y - 5}" width="${t.width + 10}" height="${t.height + 10}" rx="7" fill="none" stroke="${c.ink}" stroke-width="3"/>
      ${!l ? '' : `<line x1="${ar.x1}" y1="${ar.y1}" x2="${ar.x2}" y2="${ar.y2}" stroke="${c.halo}" stroke-width="6" opacity=".9"/>
      <line x1="${ar.x1}" y1="${ar.y1}" x2="${ar.x2}" y2="${ar.y2}" stroke="${c.ink}" stroke-width="2.5" marker-end="url(#ah)"/>
      <rect x="${l.x}" y="${l.y}" width="${l.width}" height="${l.height}" rx="6" fill="${c.ink}" stroke="${c.labelEdge}"/>
      <text x="${l.x + l.width / 2}" y="${l.y + l.height / 2 + 4.5}" text-anchor="middle" font-size="13" font-weight="600" fill="${c.labelText}"
        font-family="system-ui, sans-serif">${esc(m.point.label)}</text>`}</svg>`
  return `<!doctype html><html><head><meta charset="utf-8"><style>html,body{margin:0;background:${c.paper}}</style></head><body>
    <figure id="f" style="margin:0;position:relative;overflow:hidden;width:${Math.floor(w * k)}px;height:${Math.floor(h * k)}px">
    <img src="${img}" alt="" style="position:absolute;left:${(-x0 * k).toFixed(2)}px;top:${(-y0 * k).toFixed(2)}px;width:${(m.viewport.width * k).toFixed(2)}px">
    ${svg}</figure></body></html>`
}

const a = parseArgs(process.argv.slice(2))
const issues = checkShotSets(SHOT_SETS)
if (issues.length) {
  for (const i of issues) console.error(`✗ ${i.set}: ${i.problem}`)
  process.exit(1)
}
// a shot and the one it sits beside are one image: picking either composes both (the hero first)
const picked = (s, x) => !a.only || a.only === s.id || [x.id, x.beside, ...s.shots.filter(o => o.beside === x.id).map(o => o.id)].includes(a.only)
const shots = SHOT_SETS.flatMap(s => s.shots.filter(x => picked(s, x))
  .flatMap(x => shotThemes(s, x).map(theme => [x, theme, s])))
if (a.only && shots.length === 0) throw new Error(`no shot or set "${a.only}"`)
mkdirSync(a.out, { recursive: true })
const metaPath = join(a.out, 'images.json'), factsPath = join(a.out, 'facts.json')
const widths = existsSync(metaPath) ? JSON.parse(readFileSync(metaPath, 'utf-8')) : {}
const facts = existsSync(factsPath) ? JSON.parse(readFileSync(factsPath, 'utf-8')) : {}
for (const s of SHOT_SETS) {
  const f = join(a.shots, `${s.id}.facts.json`)
  if (existsSync(f)) facts[s.id] = JSON.parse(readFileSync(f, 'utf-8'))
}
const missing = [], stale = [], tooWide = [], wrongScheme = []
const halves = {}
// the gap between a pair is the page's own colour (GitHub's README background), so it reads as two windows
const PAGE = { light: '#ffffff', dark: '#0d1117' }
const PAIR_GAP = 12

const browser = await chromium.launch({ executablePath: findChromium(), headless: true })
try {
  const page = await browser.newPage({ deviceScaleFactor: 2 })
  for (const [spec, theme, set] of shots) {
    const capture = shotFile(spec.id, theme), file = imageFile(spec, theme), stem = imageStem(file)
    const meta = join(a.shots, capture.replace(/\.png$/, '.json')), png = join(a.shots, capture)
    if (!existsSync(meta) || !existsSync(png)) { missing.push(stem); continue }
    const m = JSON.parse(readFileSync(meta, 'utf-8'))
    // a capture from before schemes were recorded, or one a profile's own scheme slipped into
    if (m.theme !== theme) { wrongScheme.push(`${stem} (captured ${m.theme ?? 'in an unrecorded scheme'})`); continue }
    // The pointer's label and side come from the spec, so rewording one never needs a recapture; only
    // the boxes are the capture's. A different target or crop does need one.
    if (spec.point?.anchor !== m.point?.anchor || spec.crop.join() !== m.crop.join()) stale.push(stem)
    else if (spec.point) m.point = spec.point
    const g = figureGeometry(m, { column: spec.width ?? set.column, aspect: spec.aspect, pad: isPicture(spec) ? -(spec.inset ?? 0) : undefined })
    // a picture of the image has no app text to read at the app's size; an overview is not meant to be read
    if (!g.legible && !isPicture(spec) && !spec.overview) tooWide.push(`${stem} (${Math.round(g.w)} px wide → text at ${Math.round(100 * g.k)}%)`)
    await page.setContent(figureHtml(m, png, g, theme), { waitUntil: 'load' })
    // a shot beside another is drawn into that one's image below, so its own figure is an intermediate
    const dest = join(spec.beside ? a.shots : a.out, file)
    await page.locator('#f').screenshot(file.endsWith('.jpg') ? { path: dest, type: 'jpeg', quality: 80 } : { path: dest })
    const w = Math.floor(g.w * g.k)   // whole px: a fractional edge would show the page behind
    if (spec.beside) halves[`${spec.id}:${theme}`] = { file: dest, w }
    else widths[stem] = w
    console.log(`✓ ${stem} → ${dest}`)
  }
  // one image per pair: the hero, then what sits beside it, a page-coloured gap between
  for (const [spec, theme, set] of shots) {
    if (!spec.beside) continue
    const half = halves[`${spec.id}:${theme}`]
    const hero = set.shots.find(o => o.id === spec.beside), file = imageFile(hero, theme), stem = imageStem(file)
    const heroW = widths[stem]
    if (!half || !heroW || !existsSync(join(a.out, file))) { missing.push(`${stem} + ${spec.id}`); continue }
    const uri = f => `data:image/${f.endsWith('.jpg') ? 'jpeg' : 'png'};base64,${readFileSync(f).toString('base64')}`
    await page.setContent(`<!doctype html><html><head><meta charset="utf-8"><style>html,body{margin:0;background:${PAGE[theme]}}</style></head><body>
      <div id="f" style="display:inline-flex;gap:${PAIR_GAP}px;align-items:flex-start;background:${PAGE[theme]}">
      <img src="${uri(join(a.out, file))}" style="width:${heroW}px"><img src="${uri(half.file)}" style="width:${half.w}px"></div></body></html>`, { waitUntil: 'load' })
    const out = join(a.out, file)
    await page.locator('#f').screenshot(file.endsWith('.jpg') ? { path: out, type: 'jpeg', quality: 80 } : { path: out })
    widths[stem] = heroW + PAIR_GAP + half.w
    console.log(`✓ ${stem} + ${spec.id} → ${out}`)
  }
} finally {
  await browser.close()
}
// what every shot of every set composes to: a width for anything else is a removed shot's, and an image
// file nothing composes is one to delete
const expected = new Set(SHOT_SETS.flatMap(setImages))
for (const k of Object.keys(widths)) if (![...expected].some(f => imageStem(f) === k)) delete widths[k]
const orphans = readdirSync(a.out).filter(f => /\.(png|jpg)$/.test(f) && !expected.has(f))
if (orphans.length) console.warn(`! no shot composes these, delete them: ${orphans.join(', ')}`)
const sorted = o => JSON.stringify(Object.fromEntries(Object.entries(o).sort()), null, 1) + '\n'
writeFileSync(metaPath, sorted(widths))
writeFileSync(factsPath, sorted(facts))
const warn = (list, what) => {
  if (!list.length) return
  console.warn(`${a.strict ? '✗' : '!'} ${what}: ${list.join(', ')}`)
  if (a.strict) process.exitCode = 1
}
warn(tooWide, 'crop too wide to read on the page, crop tighter')
warn(stale, 'spec changed the pointer target or crop since capture, recapture')
warn(missing, 'not captured (pixi run doc-shots)')
if (wrongScheme.length) {
  console.error(`✗ captured in another colour scheme than the set's themes, recapture: ${wrongScheme.join(', ')}`)
  process.exitCode = 1
}
