#!/usr/bin/env node
//
// A PR's contact sheet: every screenshot it changes, before and after, on one page for review.
//
//     pixi run doc-sheet [-- --ref origin/main] [--after user-docs/images] [--out DIR] [--all]
//
// Before is the image at `--ref` (git show), after is the file in `--after`. Listed: every image that
// is new, changed or removed against the ref (`--all`: unchanged ones too). Written to `--out`
// (default docshots-out/sheet/, git-ignored) as index.html and sheet.png.
//
// The sheet stays on this machine. The fixtures are lab data: their images never go into a PR body
// or comment (docs/todo/PR_CAPTURES_PLAN.md). Only images a page shows are committed, and those under
// user-docs/images/.
//
// A review capture that is not a docs image: capture and compose it outside the repo, then point
// the sheet at it (everything there counts as new, or changed against a docs image of the same name):
//     pixi run doc-shots  -- … --only <id> --out <dir>/shots
//     pixi run doc-images -- --only <id> --shots <dir>/shots --out <dir>/images
//     pixi run doc-sheet  -- --after <dir>/images

import { execFileSync } from 'node:child_process'
import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join, relative, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium, commit, findChromium, repo } from '../lib/chromium.mjs'

const DOCS = 'user-docs/images'
const IMAGE = /\.(png|jpe?g)$/i

function parseArgs(argv) {
  const a = { ref: 'origin/main', after: join(repo, DOCS), out: join(repo, 'docshots-out', 'sheet'), all: false }
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i]
    if (k === '--ref') a.ref = argv[++i]
    else if (k === '--after') a.after = resolve(argv[++i])
    else if (k === '--out') a.out = resolve(argv[++i])
    else if (k === '--all') a.all = true
    else throw new Error(`unknown argument ${k}`)
  }
  return a
}

const git = (...args) => execFileSync('git', args, { cwd: repo, maxBuffer: 1 << 28 })

/** The image at `ref`, or null when the ref has no such file. */
function before(ref, name) {
  try { return git('show', `${ref}:${DOCS}/${name}`) } catch { return null }
}

const a = parseArgs(process.argv.slice(2))
git('rev-parse', '--verify', `${a.ref}^{commit}`)   // fails loudly on a ref that does not exist
const atRef = git('ls-tree', '--name-only', `${a.ref}:${DOCS}`).toString().split('\n').filter(n => IMAGE.test(n))
const now = existsSync(a.after) ? readdirSync(a.after).filter(n => IMAGE.test(n)) : []
// a review folder outside the docs holds only what was captured: what it lacks is not removed
const isDocs = resolve(a.after) === resolve(join(repo, DOCS))
const names = [...new Set([...now, ...(isDocs ? atRef : [])])].sort()

const rows = []
for (const name of names) {
  const old = before(a.ref, name)
  const cur = now.includes(name) ? readFileSync(join(a.after, name)) : null
  const status = !old ? 'new' : !cur ? 'removed' : old.equals(cur) ? 'unchanged' : 'changed'
  if (status === 'unchanged' && !a.all) continue
  rows.push({ name, status, old, cur })
}

mkdirSync(join(a.out, 'before'), { recursive: true })
const src = (r, which) => {
  const buf = which === 'before' ? r.old : r.cur
  if (!buf) return null
  if (which === 'after') return pathToFileURL(join(a.after, r.name)).href
  writeFileSync(join(a.out, 'before', r.name), buf)
  return `before/${r.name}`
}
const kb = b => (b ? `${Math.round(b.length / 1000)} kB` : '')
const cell = (r, which) => {
  const s = src(r, which)
  const buf = which === 'before' ? r.old : r.cur
  return s ? `<td><img src="${s}"><div class="m">${which} · ${kb(buf)}</div></td>` : `<td class="none">${which === 'before' ? 'not at ' + a.ref : 'removed'}</td>`
}
const head = commit()
const branch = git('rev-parse', '--abbrev-ref', 'HEAD').toString().trim()
const html = `<!doctype html><meta charset="utf-8"><title>Contact sheet ${branch}</title>
<style>
body{font:14px system-ui,sans-serif;margin:16px;background:#8a8a8a;color:#111}
h1{font-size:18px;margin:0 0 4px} p{margin:0 0 16px}
table{border-collapse:collapse;margin-bottom:24px;background:#9a9a9a}
th{text-align:left;padding:8px;font-weight:600} td{padding:8px;vertical-align:top}
img{max-width:620px;display:block;outline:1px solid #555} .m{font-size:12px;margin-top:4px}
.none{color:#333;font-style:italic;min-width:200px}
.new{color:#0a4} .changed{color:#a50} .removed{color:#a00} .unchanged{color:#333}
</style>
<h1>${branch} @ ${head} against ${a.ref}</h1>
<p>${rows.length ? rows.length + ' image(s).' : 'No screenshot changes.'} Local only: lab data, never attached to a PR.</p>
${rows.map(r => `<table><tr><th colspan="2">${r.name} · <span class="${r.status}">${r.status}</span></th></tr><tr>${cell(r, 'before')}${cell(r, 'after')}</tr></table>`).join('\n')}`
const index = join(a.out, 'index.html')
writeFileSync(index, html)

const browser = await chromium.launch({ executablePath: findChromium(), headless: true, args: ['--allow-file-access-from-files'] })
try {
  const page = await browser.newPage({ viewport: { width: 1340, height: 800 } })
  await page.goto(pathToFileURL(index).href, { waitUntil: 'load' })
  await page.screenshot({ path: join(a.out, 'sheet.png'), fullPage: true })
} finally {
  await browser.close()
}
for (const r of rows) console.log(`${r.status.padEnd(9)} ${r.name}`)
console.log(`→ ${relative(process.cwd(), index) || index} (and sheet.png)`)
