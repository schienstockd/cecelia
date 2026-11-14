// Driving a running Cecelia from a Node script: sign in with the instance token, reach the app's
// stores and router, close launch pop-ups, wait for a page's own loading state, land a page on an
// image, switch profile. Used by scripts/gui_capture.mjs (census) and scripts/docshots/ (the user
// docs' fixtures and shots).
//
// Run against an ISOLATED instance only (own CECELIA_DEV_DIR + CECELIA_PORT_SLOT): opening a project
// writes (lastOpenedAt), and shots may click through the app. `refuseSlotZero` guards 8080 / 5173.

import { readFileSync } from 'node:fs'

export function refuseSlotZero(base, allow = false) {
  const port = new URL(base).port
  if (!allow && (port === '5173' || port === '8080'))
    throw new Error(`refusing ${base}: slot 0 is usually someone's working instance (use an isolated slot)`)
}

/** Run `fn(globalProperties, arg)` in the page: `g.$pinia._s.get(store)`, `g.$router`. */
export const vue = (page, fn, arg) =>
  page.evaluate(`(${fn.toString()})(document.querySelector('#app').__vue_app__.config.globalProperties, ${JSON.stringify(arg)})`)

/** Sign `page` in with the instance's api token and load the app. */
export async function signIn(page, base, configDir) {
  const token = readFileSync(`${configDir}/api-token`, 'utf8').trim()
  await page.goto(`${base}/api/auth?token=${encodeURIComponent(token)}`)
  await page.goto(base)
  await page.waitForLoadState('networkidle', { timeout: 15000 }).catch(() => {})
}

// Launch pop-ups (What's new, tips) would cover every shot. BaseModal renders `.cc-modal-overlay`
// with a `.cc-modal-close` button; Escape is the fallback for anything else.
export async function closeDialogs(page) {
  for (let i = 0; i < 4; i++) {
    const close = await page.$('.cc-modal-overlay .cc-modal-close')
    if (close) await close.click().catch(() => {})
    else if (await page.$('[role=dialog]')) await page.keyboard.press('Escape')
    else return
    await page.waitForTimeout(300)
  }
}

/** Settled = network idle AND no visible "Loading…" or spinner in the page, three polls in a row.
 *  A fixed timer shot the spinner (screenshots plan, T2). Returns ms waited; caps at `capMs`. */
export async function settle(page, capMs = 60000) {
  const t0 = Date.now()
  await page.waitForLoadState('networkidle', { timeout: 15000 }).catch(() => {})
  let quiet = 0
  while (Date.now() - t0 < capMs && quiet < 3) {
    const busy = await page.evaluate(() => {
      const m = document.querySelector('.cc-main') || document.body
      const vis = el => { const r = el.getBoundingClientRect(); return r.width > 0 && r.height > 0 }
      return /Loading…|Loading\.\.\.|still be starting/.test(m.innerText) ||
        [...m.querySelectorAll('.spinner,.cc-spinner,.plot-spinner,[aria-busy=true]')].some(vis)
    })
    quiet = busy ? 0 : quiet + 1
    await page.waitForTimeout(200)
  }
  return Date.now() - t0
}

export async function openProject(page, uid, { peek = false } = {}) {
  const ok = await vue(page, (g, a) => {
    const meta = g.$pinia._s.get('projectMeta')
    return a.peek ? meta.peekProject(a.uid) : meta.openProject(a.uid)
  }, { uid, peek })
  if (!ok) throw new Error(`could not open project ${uid}`)
  await page.waitForLoadState('networkidle', { timeout: 15000 }).catch(() => {})
}

/** Push `route`, first focusing `images` in selection scope `scope` (the page's ModuleLayout `module`). */
export async function land(page, route, scope, images = []) {
  await vue(page, (g, a) => {
    if (a.images.length) {
      const p = g.$pinia._s.get('project')
      p.focusImages(a.scope, p.setUidOfImage(a.images[0]), a.images)
    }
    return g.$router.push(a.route)
  }, { route, scope, images })
  await settle(page)
}

/** Make `name` the server-active profile, then reload so the app reads it (AI state, login, projects). */
export async function selectProfile(page, name) {
  const ok = await page.evaluate(n => fetch('/api/kiwi/profiles/select', {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ name: n }) }).then(r => r.ok), name)
  if (!ok) throw new Error(`could not select profile "${name}"`)
  await page.reload()
  await quietNavigation(page)
  // With two profiles the app opens on "Who's driving?". The server already has `name` active, so
  // finish the pick the way the picker's own row does (AppProfilePicker → pick).
  if (page.url().includes('/profile-picker'))
    // finishing the pick sets off that adopting reload under this very call: expected, not an error
    await vue(page, g => { g.$pinia._s.get('appControl').completeProfilePick(); return g.$router.replace('/') })
      .catch(e => { if (!/context was destroyed/.test(String(e))) throw e })
  await quietNavigation(page)
  await settle(page)
}

/** Wait until the page stops reloading itself: on a profile switch the settings store adopts that
 *  profile's settings with one more reload (stores/settings.ts → adoptLiveProfileBag). */
async function quietNavigation(page, quietMs = 1500, capMs = 20000) {
  let last = Date.now()
  const onNav = f => { if (f === page.mainFrame()) last = Date.now() }
  page.on('framenavigated', onNav)
  const t0 = Date.now()
  try {
    while (Date.now() - last < quietMs && Date.now() - t0 < capMs) await page.waitForTimeout(250)
    await page.waitForLoadState('networkidle', { timeout: 15000 }).catch(() => {})
  } finally { page.off('framenavigated', onNav) }
}
