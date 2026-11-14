#!/usr/bin/env node
//
// Capture the booklets' screenshots from a running, ISOLATED Cecelia.
//
//     pixi run booklet-shots -- --base http://127.0.0.1:5223 --config-dir <dir> [--only <shot or booklet id>] [--out DIR]
//
// Each shot (user-docs/booklets → `Shot`) runs in a FRESH browser context — in one long session,
// state leaked between pages (screenshots plan, first pass) — in the light scheme at 2× scale: sign
// in, open the shot's project, focus its image, push its route, run its actions, wait for the page's
// own loading state, then record the box of every anchor it names and screenshot the viewport.
// The crop and the pointer are NOT baked in: the build draws them from the recorded boxes, so the
// look can change without recapturing (screenshots plan → *Booklet direction*).
//
// The run FAILS a shot (and exits non-zero) when:
//   - an anchor it names is missing or invisible (a renamed control, a panel that never opened);
//   - an `expect` label is not in its anchor's text (the caption cites a label the app no longer shows);
//   - the cropped area shows a relative time, a date or "ago" that no mask covers (it goes stale on the
//     next capture; the review of the first drafts found "77d" and "12h" baked into the pixels).
// Counts in captions are not asserted: a person checks those on the PDF (STYLE.md).
//
// Output per shot: <out>/<id>.png and <out>/<id>.json (viewport, scale, boxes in CSS px, commit, time).
// Isolated instance only: shots click through the app and opening a project writes. Refuses 8080/5173.
//
// Setting one up: an empty CECELIA_DEV_DIR whose custom.toml already names a projects folder inside it
//     [dirs]
//     projects = "<that dir>/projects"
// then `CECELIA_DEV_DIR=<that dir> CECELIA_PORT_SLOT=5 pixi run dev` (frontend :5223). Without the
// [dirs] entry the first-run screen proposes ~/cecelia-projects — outside the isolated dir.
// Then `pixi run booklet-fixture` (fixture.mjs) copies the booklets' lab images and creates their
// profiles; a shot names those by key, and runs as its `profile` (default: `default`).

import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium, commit, findChromium, repo } from '../lib/chromium.mjs'
import { closeDialogs, land, openProject, refuseSlotZero, selectProfile, settle, signIn, vue } from '../lib/app_page.mjs'

const dir = join(repo, 'user-docs', 'booklets')
const { BOOKLETS } = await import(pathToFileURL(join(dir, 'index.ts')).href)
const { checkBooklets, FIXTURE_RECORD } = await import(pathToFileURL(join(dir, 'spec.ts')).href)
const { anchorSelector } = await import(pathToFileURL(join(repo, 'frontend', 'src', 'utils', 'guideAnchor.ts')).href)

const VIEWPORT = { width: 1440, height: 900 }
const SCALE = 2
// Freeze motion and hide what changes on every run: the console rail's last line and unread badge.
const FREEZE = `*,*::before,*::after{animation:none!important;transition:none!important;caret-color:transparent!important}
.console-bar .bar-last,.console-bar .unread-badge{visibility:hidden!important}
.booklet-mask,.booklet-mask *{color:transparent!important;text-shadow:none!important}`
// Text that goes stale between captures: "12h", "77d", "3 min ago", ISO dates, clock times.
const VOLATILE = /\b\d+\s?(?:s|m|h|d|w|mo|y|min|sec|hrs?)\b|\bago\b|\b\d{4}-\d{2}-\d{2}\b|\b\d{1,2}:\d{2}\b/

function parseArgs(argv) {
  const a = { out: join(repo, 'booklets-out', 'shots'), only: '' }
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i]
    if (k === '--allow-default-ports') { a.allowDefaultPorts = true; continue }
    if (!k.startsWith('--')) throw new Error(`unexpected argument ${k}`)
    a[k.slice(2).replace(/-(\w)/g, (_, c) => c.toUpperCase())] = argv[++i]
  }
  if (!a.base || !a.configDir) throw new Error('need --base URL and --config-dir DIR')
  return a
}

/** Anchor → [Playwright selector, nth]. Same scheme as the guides (`anchorSelector`), plus css: and @@n. */
function selector(anchor) {
  const [a, n] = anchor.split('@@')
  if (a.startsWith('text:')) return [`text=${a.slice(5)}`, Number(n ?? 0)]
  return [a.startsWith('css:') ? a.slice(4) : anchorSelector(a), Number(n ?? 0)]
}
const loc = (page, anchor) => { const [s, n] = selector(anchor); return page.locator(s).nth(n) }

async function runAction(page, act) {
  if ('click' in act) await loc(page, act.click).click({ timeout: 10000 })
  else if ('fill' in act) await loc(page, act.fill).fill(act.text, { timeout: 10000 })
  else if ('hover' in act) await loc(page, act.hover).hover({ timeout: 10000 })
  else if ('press' in act) await page.keyboard.press(act.press)
  else if ('waitFor' in act) await loc(page, act.waitFor).waitFor({ state: 'visible', timeout: 15000 })
  else if ('scroll' in act) await loc(page, act.scroll).evaluate(el => el.scrollIntoView({ block: 'end' }))
  else if ('panel' in act) await page.evaluate(a => {
    // FloatingPanel's own key and shape (components/FloatingPanel.vue → load)
    localStorage.setItem(`cc.floating.${a.panel}`, JSON.stringify({ x: 260, y: a.y ?? 70, w: a.w, h: a.h, collapsed: false, maximised: false }))
  }, act)
  await settle(page, 30000)
}

async function capture(browser, a, shot, fixture, out) {
  const ctx = await browser.newContext({ viewport: VIEWPORT, deviceScaleFactor: SCALE })
  const page = await ctx.newPage()
  const errors = []
  page.on('pageerror', e => errors.push(String(e).slice(0, 200)))
  const problems = []
  try {
    await signIn(page, a.base, a.configDir)
    await selectProfile(page, shot.profile ?? 'default')
    await closeDialogs(page)
    await vue(page, g => { const s = g.$pinia._s.get('settings'); s.colourScheme = 'light'; s.plotsMaximised = false; s.kiwiOpen = false })
    const proj = shot.project ? fixture?.projects[shot.project] : null
    if (shot.project && !proj) throw new Error(`fixture project "${shot.project}" is not set up: run pixi run booklet-fixture`)
    if (proj) await openProject(page, proj.uid)
    const image = shot.image ? proj.images[shot.image] : null
    if (shot.route) await land(page, shot.route, shot.scope ?? '', image ? [image] : [])
    else await settle(page)
    await closeDialogs(page)
    for (const act of shot.actions ?? []) await runAction(page, act)
    await page.addStyleTag({ content: FREEZE })

    for (const m of shot.masks ?? []) await loc(page, m).evaluate(el => el.classList.add('booklet-mask')).catch(() => problems.push(`mask ${m} not found`))
    const named = [...new Set([...shot.crop, ...(shot.point ? [shot.point.anchor] : []), ...(shot.expect ?? []).map(e => e.anchor)])]
    const boxes = {}
    for (const an of named) {
      const box = await loc(page, an).boundingBox({ timeout: 3000 }).catch(() => null)
      if (!box || box.width === 0 || box.height === 0) problems.push(`anchor ${an} is missing or not visible`)
      else boxes[an] = box
    }
    for (const e of shot.expect ?? []) {
      const text = await loc(page, e.anchor).innerText({ timeout: 2000 }).catch(() => '')
      if (!text.includes(e.text)) problems.push(`"${e.text}" not in ${e.anchor} (it reads "${text.slice(0, 60)}")`)
    }
    for (const an of shot.crop) {
      const text = await loc(page, an).evaluate(el => {
        // text of the anchor minus anything masked
        const c = el.cloneNode(true); c.querySelectorAll('.booklet-mask').forEach(x => x.remove())
        return el.classList.contains('booklet-mask') ? '' : c.innerText ?? c.textContent ?? ''
      }).catch(() => '')
      const hit = text.match(VOLATILE)
      if (hit) problems.push(`${an} shows "${hit[0]}", which changes between captures: mask it or crop it out`)
    }
    await page.mouse.move(VIEWPORT.width - 2, VIEWPORT.height - 2)   // park the mouse: no stray hover
    await page.waitForTimeout(200)
    await page.screenshot({ path: join(out, `${shot.id}.png`) })
    writeFileSync(join(out, `${shot.id}.json`), JSON.stringify({
      id: shot.id, viewport: VIEWPORT, scale: SCALE, boxes, point: shot.point ?? null, crop: shot.crop,
      commit: commit(), capturedAt: new Date().toISOString(), pageErrors: errors,
    }, null, 1))
  } catch (e) {
    problems.push(String(e.message ?? e).split('\n')[0].slice(0, 240))
  } finally {
    // what the shot saw when it failed, to diagnose without rerunning headed
    if (problems.length) await page.screenshot({ path: join(out, `${shot.id}.fail.png`) }).catch(() => {})
    await ctx.close()
  }
  return problems
}

async function main() {
  const a = parseArgs(process.argv.slice(2))
  refuseSlotZero(a.base, a.allowDefaultPorts)
  const issues = checkBooklets(BOOKLETS)
  if (issues.length) { for (const i of issues) console.error(`✗ ${i.booklet}: ${i.problem}`); process.exit(1) }
  const shots = BOOKLETS.flatMap(b => (b.shots ?? []).filter(s => !a.only || a.only === s.id || a.only === b.id).map(s => [b, s]))
  const recordPath = join(a.configDir, FIXTURE_RECORD)
  const record = existsSync(recordPath) ? JSON.parse(readFileSync(recordPath, 'utf8')) : {}
  if (shots.length === 0) throw new Error(a.only ? `no shot or booklet "${a.only}"` : 'no booklet defines a shot')
  const out = resolve(a.out)
  mkdirSync(out, { recursive: true })
  const browser = await chromium.launch({ executablePath: findChromium(), headless: true })
  let failed = 0
  try {
    for (const [b, s] of shots) {
      const problems = await capture(browser, a, s, record[b.id], out)
      if (problems.length) { failed++; console.log(`✗ ${s.id}\n${problems.map(p => `    ${p}`).join('\n')}`) }
      else console.log(`✓ ${s.id}`)
    }
  } finally {
    await browser.close()
  }
  console.log(`${shots.length - failed}/${shots.length} shots → ${out}`)
  if (failed) process.exit(1)
}

main().catch(e => { console.error(`booklet-shots: ${e.message}`); process.exit(1) })
