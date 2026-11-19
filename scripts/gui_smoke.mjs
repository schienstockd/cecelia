#!/usr/bin/env node
//
// GUI route smoke: every page of a running Cecelia renders, with no script error, no `console.error`
// and no ResizeObserver loop — once with no project open, once with one open.
//
//     pixi run test-gui-app -- --base http://127.0.0.1:5223 --config-dir <dir> [--project testpr]
//
// docs/todo/GUI_TESTING_PLAN.md, Decision 8 / P4. One test for every page, so a page that throws on
// load is caught without a test per page (page LAYOUTS are not tested: they change too often to keep
// a test true). The routes come from the app's own router (`$router.getRoutes()`), so a new page is
// covered with no edit here; pop-out windows and first-run screens are skipped by name, with why
// (`NOT_A_PAGE`, scripts/lib/app_page.mjs).
//
// RUN IT AGAINST AN ISOLATED INSTANCE: opening a project stamps its lastOpenedAt. Refuses slot 0's
// ports (8080 / 5173) unless --allow-default-ports — CI passes it, its runner's 8080 is its own.
// Exit 1 with one line per failing (route, project) pair.

import { chromium, findChromium } from './lib/chromium.mjs'
import { NOT_A_PAGE, closeDialogs, openProject, refuseSlotZero, settle, signIn, vue } from './lib/app_page.mjs'

function parseArgs(argv) {
  const a = { project: '' }
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i]
    if (k === '--allow-default-ports') { a.allowDefaultPorts = true; continue }
    if (!k.startsWith('--')) throw new Error(`unexpected argument ${k}`)
    a[k.slice(2).replace(/-(\w)/g, (_, c) => c.toUpperCase())] = argv[++i]
  }
  if (!a.base || !a.configDir) throw new Error('need --base URL and --config-dir DIR')
  return a
}

/** Errors that are not the page's: each entry names its source and why, never just a message. Empty
 *  until a run shows one. */
const ALLOWED = []

const routesOf = page => vue(page, g => g.$router.getRoutes()
  .filter(r => !r.redirect && !r.path.includes(':') && r.path !== '/:pathMatch(.*)*')
  .map(r => r.path))

async function visit(page, path, errors) {
  errors.length = 0
  await vue(page, (g, p) => g.$router.push(p), path)
  await settle(page, 20000)
  await closeDialogs(page)
  const rendered = await page.evaluate(() => {
    const m = document.querySelector('.cc-main')
    return !!m && m.getBoundingClientRect().height > 0 && m.innerText.trim().length > 0
  })
  const landed = await vue(page, g => g.$router.currentRoute.value.path)
  const problems = [...errors]
  if (!rendered) problems.push('nothing rendered in .cc-main')
  if (landed !== path) problems.push(`redirected to ${landed}`)
  return problems
}

const args = parseArgs(process.argv.slice(2))
refuseSlotZero(args.base, args.allowDefaultPorts)
const browser = await chromium.launch({ executablePath: findChromium() })
const failures = []
try {
  const page = await browser.newPage({ viewport: { width: 1440, height: 900 } })
  const errors = []
  page.on('pageerror', e => errors.push(`script error: ${e.message}`))
  page.on('console', m => {
    if (m.type() === 'error' && !ALLOWED.some(r => r.test(m.text()))) errors.push(`console.error: ${m.text().slice(0, 300)}`)
  })
  // a ResizeObserver loop arrives as a bare window `error` event the app's own handler swallows
  // (main.ts) — record it before anything else can
  await page.addInitScript(() => window.addEventListener('error', e => {
    if (/ResizeObserver loop/.test(e.message ?? '')) console.error(`ResizeObserver loop: ${e.message}`)
  }, true))
  await signIn(page, args.base, args.configDir)
  await settle(page)
  await closeDialogs(page)
  const routes = (await routesOf(page)).filter(p => !(p in NOT_A_PAGE)).sort()
  if (!routes.length) throw new Error('the router listed no routes')
  console.log(`routes: ${routes.join(' ')}`)
  console.log(`skipped: ${Object.keys(NOT_A_PAGE).join(' ')}`)
  for (const project of ['', ...(args.project ? [args.project] : [])]) {
    if (project) await openProject(page, project)
    const label = project ? `project ${project}` : 'no project'
    for (const path of routes) {
      const problems = await visit(page, path, errors)
      console.log(`${problems.length ? 'FAIL' : 'ok  '} ${label} ${path}${problems.length ? ` — ${problems.join(' · ')}` : ''}`)
      if (problems.length) failures.push(`${label} ${path}: ${problems.join(' · ')}`)
    }
  }
} finally {
  await browser.close()
}
if (failures.length) {
  console.log(`\n${failures.length} page(s) failed:\n${failures.join('\n')}`)
  process.exit(1)
}
console.log('\nevery page rendered')
