#!/usr/bin/env node
//
// Screenshot every page of a running Cecelia and count the controls a user sees on it.
//
//     pixi run gui-capture -- --base http://127.0.0.1:5223 --config-dir <dir> [--project UID[,UID…]]
//     node scripts/gui_capture.mjs --base URL --config-dir DIR [--project UID[,UID…]] [--out DIR]
//                                  [--routes /segment,/gate] [--size 1440x900] [--peek]
//
// WHY THIS EXISTS. The GUI clarity audit (docs/todo/GUI_CLARITY_PLAN.md) needs measured first-paint
// control counts and before/after screenshots, and template parsing can't say what is actually on
// screen. This drives the real app headlessly: sign in with the instance's token, optionally open a
// project through the app's own store, visit each route through the app's router (no reload, so the
// open project stays open), screenshot it, and count visible controls inside `.cc-main` (the page)
// and outside it (header, sidebar, console rail).
//
// RUN IT AGAINST AN ISOLATED INSTANCE, never someone's working one: opening a project is a write
// (it stamps lastOpenedAt). `--peek` opens through `projectMeta.peekProject` instead, which writes
// nothing but skips module canvases and animations, so module pages render without their saved plots;
// the default `openProject` is what a user sees. Start one with its own config dir and port slot, e.g.
//     CECELIA_DEV_DIR=/tmp/gui-dev/config CECELIA_PORT_SLOT=5 pixi run dev
// with `[dirs] projects` in that config's custom.toml pointing at a copy of test-data/projects.
// The script refuses slot 0's ports (8080 / 5173) unless --allow-default-ports is passed.
//
// Browser: scripts/lib/chromium.mjs (playwright-core + a system Chromium; CECELIA_CHROMIUM picks it).

import { mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { chromium, findChromium, repo } from './lib/chromium.mjs'

// ── args ───────────────────────────────────────────────────────────────────────────────────────
function parseArgs(argv) {
  const a = { size: '1440x900', out: join(repo, 'gui-capture'), project: '', routes: '' }
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i]
    if (k === '--allow-default-ports') { a.allowDefaultPorts = true; continue }
    if (k === '--peek') { a.peek = true; continue }
    if (!k.startsWith('--')) throw new Error(`unexpected argument ${k}`)
    a[k.slice(2).replace(/-(\w)/g, (_, c) => c.toUpperCase())] = argv[++i]
  }
  if (!a.base || !a.configDir) throw new Error('need --base URL and --config-dir DIR')
  return a
}

// Every routed page from the router table in frontend/src/main.ts, minus redirects, parameterised
// routes, pop-out windows and first-run screens (those need their own state to mean anything).
const SKIP = new Set(['/console', '/tasks-window', '/viewer-window', '/board-render', '/setup', '/profile-picker'])
function routesFromMain() {
  const src = readFileSync(join(repo, 'frontend', 'src', 'main.ts'), 'utf8')
  return [...src.matchAll(/\{\s*path:\s*'([^']+)',\s*component:/g)]
    .map(m => m[1]).filter(p => !p.includes(':') && !SKIP.has(p))
}

// ── in-page helpers (run in the browser) ───────────────────────────────────────────────────────
// A control: something a user can operate. Visible = rendered with a non-zero box and not hidden
// by visibility/opacity. "Above the fold" = its box starts inside the viewport.
function countControlsInPage() {
  const MODAL = '.cc-modal-overlay, [role=dialog]'
  const SEL = 'button, a[href], input:not([type=hidden]), select, textarea, [role=tab], [role=switch], ' +
              '[role=checkbox], [role=radio], [role=slider], [role=combobox], [contenteditable=true]'
  const main = document.querySelector('.cc-main')
  const vh = window.innerHeight
  const out = { main: 0, mainAboveFold: 0, chrome: 0, modalOpen: !!document.querySelector(MODAL) }
  for (const el of document.querySelectorAll(SEL)) {
    if (el.parentElement?.closest(SEL)) continue            // a control nested in a control counts once
    const r = el.getBoundingClientRect()
    if (r.width === 0 || r.height === 0) continue
    const cs = getComputedStyle(el)
    if (cs.visibility === 'hidden' || Number(cs.opacity) === 0) continue
    if (main && main.contains(el)) { out.main++; if (r.top < vh) out.mainAboveFold++ } else out.chrome++
  }
  return out
}

// ── run ────────────────────────────────────────────────────────────────────────────────────────
// Launch pop-ups (What's new, tips) would cover every shot. BaseModal renders `.cc-modal-overlay`
// with a `.cc-modal-close` button; Escape is the fallback for anything else.
async function closeDialogs(page) {
  for (let i = 0; i < 4; i++) {
    const close = await page.$('.cc-modal-overlay .cc-modal-close')
    if (close) await close.click().catch(() => {})
    else if (await page.$('[role=dialog]')) await page.keyboard.press('Escape')
    else return
    await page.waitForTimeout(400)
  }
}

async function settle(page) {
  await page.waitForLoadState('networkidle', { timeout: 8000 }).catch(() => {})
  await page.waitForTimeout(800)
}

async function captureState(page, state, routes, outDir) {
  mkdirSync(join(outDir, state), { recursive: true })
  const rows = []
  for (const route of routes) {
    await page.evaluate(p => document.querySelector('#app').__vue_app__.config.globalProperties.$router.push(p), route)
    await settle(page)
    await closeDialogs(page)
    const file = join(state, (route === '/' ? 'welcome' : route.slice(1).replaceAll('/', '_')) + '.png')
    await page.screenshot({ path: join(outDir, file) })
    rows.push({ state, route, file, ...(await page.evaluate(countControlsInPage)) })
    process.stdout.write(`${state} ${route}\n`)
  }
  return rows
}

function markdown(rows, meta) {
  const lines = [`# GUI capture — ${meta.when}`, '',
    `Base ${meta.base}, viewport ${meta.size}, project \`${meta.project || '(none)'}\`. ` +
    'Counts are visible controls (buttons, links, inputs, selects, tabs, switches) at first paint, ' +
    'after the page settled; `page` = inside `.cc-main`, `chrome` = header, sidebar and rails.', '',
    '| State | Route | Page controls | …above the fold | Chrome | Screenshot |', '|---|---|---:|---:|---:|---|']
  for (const r of rows) lines.push(`| ${r.state} | \`${r.route}\` | ${r.main} | ${r.mainAboveFold} | ${r.chrome} | [png](${r.file}) |`)
  return lines.join('\n') + '\n'
}

async function main() {
  const a = parseArgs(process.argv.slice(2))
  const port = new URL(a.base).port
  if (!a.allowDefaultPorts && (port === '5173' || port === '8080'))
    throw new Error(`refusing ${a.base}: slot 0 is usually someone's working instance (see header)`)
  const token = readFileSync(join(a.configDir, 'api-token'), 'utf8').trim()
  const [w, h] = a.size.split('x').map(Number)
  const routes = a.routes ? a.routes.split(',') : routesFromMain()
  const outDir = resolve(a.out)
  mkdirSync(outDir, { recursive: true })

  const browser = await chromium.launch({ executablePath: findChromium(), headless: true })
  try {
    const page = await browser.newPage({ viewport: { width: w, height: h } })
    await page.goto(`${a.base}/api/auth?token=${encodeURIComponent(token)}`)
    await page.goto(a.base)
    await settle(page)
    await closeDialogs(page)

    const rows = await captureState(page, 'no-project', routes, outDir)
    for (const uid of a.project.split(',').filter(Boolean)) {
      const ok = await page.evaluate(([u, peek]) => {
        const meta = document.querySelector('#app').__vue_app__.config.globalProperties.$pinia._s.get('projectMeta')
        return peek ? meta.peekProject(u) : meta.openProject(u)
      }, [uid, !!a.peek])
      if (!ok) throw new Error(`could not open project ${uid}`)
      rows.push(...await captureState(page, uid, routes, outDir))
    }
    const meta = { when: new Date().toISOString(), base: a.base, size: a.size, project: a.project }
    writeFileSync(join(outDir, 'census.json'), JSON.stringify({ meta, rows }, null, 2))
    writeFileSync(join(outDir, 'census.md'), markdown(rows, meta))
    console.log(`wrote ${rows.length} screenshots + census.md to ${outDir}`)
  } finally {
    await browser.close()
  }
}

main().catch(e => { console.error(`gui-capture: ${e.message}`); process.exit(1) })
