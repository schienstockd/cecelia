#!/usr/bin/env node
//
// Capture the user docs' screenshots from a running, ISOLATED Cecelia.
//
//     pixi run doc-shots -- --base http://127.0.0.1:5223 --config-dir <dir> [--only <shot or set id>] [--out DIR]
//
// Each shot (user-docs/shots → `Shot`) runs in a FRESH browser context — in one long session,
// state leaked between pages (screenshots plan, first pass) — once per colour scheme its set names
// (`themes`), at 2× scale: sign in, open the shot's project, focus its image, push its route, run its actions, wait for the page's
// own loading state, then record the box of every anchor it names and screenshot the viewport.
// The crop and the pointer are NOT baked in: compose.mjs (`pixi run doc-images`) draws them from the
// recorded boxes, so the look can change without recapturing.
//
// The run FAILS a shot (and exits non-zero) when:
//   - an anchor it names is missing or invisible (a renamed control, a panel that never opened);
//   - an `expect` label is not in its anchor's text (the page cites a label the app no longer shows);
//     an expect may cite a fixture fact (`{{project.image.<gate>.count}}`), so the number on the page is
//     the number in the pixels;
//   - the cropped area shows a relative time, a date or "ago" that no mask covers (it goes stale on the
//     next capture; the review of the first drafts found "77d" and "12h" baked into the pixels);
//   - a viewer shot's state, read back after it was set, is not the one asked for: another image, plane,
//     channels, outlines or gated populations (each shot starts a fresh context, but the viewer keeps
//     some settings per image, and a click that toggles instead of sets leaves the last shot's state);
//   - the page's colour scheme, read back when the screenshot is taken, is not the one asked for (a
//     profile's own scheme adopted on a reload would otherwise slip one dark shot into a light set).
//
// Output per shot and scheme: <out>/<file> (spec.ts → shotFile: `<id>.png` light, `<id>.dark.png`) and
// the same name with .json (viewport, scale, boxes in CSS px, scheme, commit, time), and per shot set <out>/<set>.facts.json: the fixture's measured numbers.
// Isolated instance only: shots click through the app and opening a project writes. Refuses 8080/5173.
//
// Setting one up: an empty CECELIA_DEV_DIR whose custom.toml already names a projects folder inside it
//     [dirs]
//     projects = "<that dir>/projects"
// then `CECELIA_DEV_DIR=<that dir> CECELIA_PORT_SLOT=5 pixi run dev` (frontend :5223). Without the
// [dirs] entry the first-run screen proposes ~/cecelia-projects — outside the isolated dir.
// Then `pixi run doc-fixture` (fixture.mjs) copies the shot sets' lab images and creates their
// profiles; a shot names those by key, and runs as its `profile` (default: `default`).

import { execFileSync } from 'node:child_process'
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium, commit, findChromium, repo } from '../lib/chromium.mjs'
import { closeDialogs, land, openProject, refuseSlotZero, selectProfile, settle, signIn, vue } from '../lib/app_page.mjs'

const dir = join(repo, 'user-docs', 'shots')
const { SHOT_SETS } = await import(pathToFileURL(join(dir, 'index.ts')).href)
const { checkShotSets, fillFacts, FIXTURE_RECORD, shotFile, shotThemes } = await import(pathToFileURL(join(dir, 'spec.ts')).href)
const { isScreenshotChrome, SCREENSHOT_KEEP } = await import(pathToFileURL(join(repo, 'frontend', 'src', 'utils', 'viewerScreenshot.ts')).href)
const { anchorSelector } = await import(pathToFileURL(join(repo, 'frontend', 'src', 'utils', 'guideAnchor.ts')).href)

const VIEWPORT = { width: 1440, height: 900 }
const SCALE = 2
// Freeze motion and hide what changes on every run: the console rail's last line and unread badge.
const FREEZE = `*,*::before,*::after{animation:none!important;transition:none!important;caret-color:transparent!important}
.console-bar .bar-last,.console-bar .unread-badge{visibility:hidden!important}
.doc-mask,.doc-mask *{color:transparent!important;text-shadow:none!important}`
// Text that goes stale between captures: "12h", "77d", "3 min ago", ISO dates, clock times.
const VOLATILE = /\b\d+\s?(?:s|m|h|d|w|mo|y|min|sec|hrs?)\b|\bago\b|\b\d{4}-\d{2}-\d{2}\b|\b\d{1,2}:\d{2}\b/

function parseArgs(argv) {
  const a = { out: join(repo, 'docshots-out', 'shots'), only: '' }
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

// ── the pop-out viewer ───────────────────────────────────────────────────────────────────────────
// WebGPU draws only in a HEADED Chromium with ANGLE on Vulkan (X11 through Xwayland); headless gets no
// adapter or loses the device (GUIDE_SCREENSHOTS_PLAN, T3 and *Viewer shot recipe*). A shot with
// `viewer` therefore runs in its own headed browser: it opens a window on the workstation's screen.
const VIEWER_ARGS = ['--enable-unsafe-webgpu', '--ozone-platform=x11', '--use-angle=vulkan',
  '--enable-features=Vulkan,VulkanFromANGLE,DefaultANGLEVulkan', `--window-size=${VIEWPORT.width},${VIEWPORT.height}`,
  ...secondScreen()]

/** Put the headed window on a non-primary monitor (`xrandr --listmonitors`), out of the way of whoever
 *  is working on the main one; nothing when there is only one screen or no xrandr. */
function secondScreen() {
  try {
    const m = execFileSync('xrandr', ['--listmonitors'], { encoding: 'utf-8' }).split('\n')
      .map(l => l.match(/^\s*\d+:\s+\+(\*?)\S+\s+\d+\/\d+x\d+\/\d+\+(\d+)\+(\d+)/)).filter(Boolean)
    const other = m.find(x => x[1] !== '*')
    return other ? [`--window-position=${other[2]},${other[3]}`] : []
  } catch { return [] }
}

/** Open `image` in the pop-out viewer and bring it to a known state, by the recipe that worked: pause,
 *  a plane with cells, reset view, contrast reset then auto (retried: auto on not-yet-loaded zeros
 *  saturates white), 2 px outlines, Segmentation on. */
async function viewerReady(page, base, projectUid, imageUid, v, problems) {
  await page.goto(`${base}/#/viewer-window?project=${projectUid}&image=${imageUid}`)
  await page.waitForLoadState('networkidle', { timeout: 30000 }).catch(() => {})
  if (!(await page.evaluate(async () => !!(await navigator.gpu?.requestAdapter()))))
    throw new Error('no WebGPU adapter: the viewer needs a headed Chromium on a display')
  // one z plane (2D), set: the viewer may open a stack in 3D, which has no plane to pick. Choosing a
  // segment is idempotent. The image is read before the controls exist ("Reading image…").
  await page.locator('[aria-label="View mode"]').getByText('2D', { exact: true }).click({ timeout: 90000 })
  await page.locator('input[aria-label="z plane"]').waitFor({ state: 'attached', timeout: 90000 })
  // in THIS page: a setting written on the main page and navigated away from is not saved yet, and the
  // viewer would read the last shot's value
  await vue(page, (g, o) => {
    const s = g.$pinia._s.get('settings')
    s.viewerLabelContour = o.contour
    if (o.tail) s.viewerTailLength = o.tail
    // a track drawn in full, whichever planes it crosses (the default reach is a cell's few planes)
    if (o.tracks) s.viewerTrackZTol = 1000
  }, { contour: v.outlines === false ? 0 : 2, tail: v.tail, tracks: !!v.tracks?.length })
  await page.waitForTimeout(1500)
  const pause = page.locator('[aria-label="Pause"]')
  if (await pause.count()) await pause.first().click()
  if (v.plane !== undefined) await page.evaluate(p => {
    const inp = document.querySelector('input[aria-label="z plane"]')
    inp.value = String(p); inp.dispatchEvent(new Event('input', { bubbles: true })); inp.dispatchEvent(new Event('change', { bubbles: true }))
  }, v.plane)
  await page.locator('[aria-label="Reset view"]').click()
  if (v.zoom) {
    // wheel steps at a point given as a fraction of the canvas, so the crop shows cells one by one
    const box = await page.locator('canvas').first().boundingBox()
    const [fx, fy] = v.at ?? [0.5, 0.5]
    await page.mouse.move(box.x + fx * box.width, box.y + fy * box.height)
    for (let i = 0; i < v.zoom; i++) { await page.mouse.wheel(0, -240); await page.waitForTimeout(250) }
    await page.waitForLoadState('networkidle', { timeout: 20000 }).catch(() => {})
  }
  if (!(await page.locator('[aria-label="Auto contrast every channel"]').isVisible()))
    await page.getByText('Channels', { exact: true }).first().click()
  if (v.channels) {
    // exactly these channels on: the rest switched off (AF over everything washes the picture out)
    // only the Channels section's switches: the Segmentation section has "Show <label set>" ones too
    const rows = await page.evaluate(() => {
      const sec = document.querySelector('[aria-label="Auto contrast every channel"]')?.closest('.collapsible-section')
      return [...(sec?.querySelectorAll('input[aria-label^="Show "]') ?? [])]
        .map(el => ({ name: el.getAttribute('aria-label').slice(5), on: el.checked }))
    })
    const missing = v.channels.filter(n => !rows.some(r => r.name === n))
    if (missing.length) throw new Error(`viewer: no channel named ${missing.join(', ')} (has ${rows.map(r => r.name).join(', ')})`)
    for (const r of rows) if (r.on !== v.channels.includes(r.name)) {
      // CcToggle's checkbox is visually hidden under its styled label: click it in the page
      await page.locator('.collapsible-section', { has: page.locator('[aria-label="Auto contrast every channel"]') })
        .locator(`input[aria-label="Show ${r.name}"]`).evaluate(el => el.click())
      await page.waitForTimeout(300)
    }
  }
  // Reset first: the last shot's contrast persists, and Auto is judged against the full range
  await page.locator('[aria-label="Reset every channel contrast"]').click()
  await page.waitForTimeout(1000)
  // Auto has taken when the channels' range readouts ("0–65.5k") are no longer all the full bit range;
  // pressed before the plane's data is in, it does nothing (dark) or works on zeros (white).
  const ranges = () => page.evaluate(() => [...document.querySelectorAll('span,div')]
    .filter(e => e.children.length === 0 && /^\d[\d.]*k?\s*[–-]\s*\d[\d.]*k?$/.test(e.textContent.trim())).map(e => e.textContent.trim()))
  const full = (await ranges()).join('|')
  let ok = false
  for (let k = 0; k < 8 && !ok; k++) {
    await page.waitForLoadState('networkidle', { timeout: 20000 }).catch(() => {})
    await page.waitForTimeout(2500)
    await page.locator('[aria-label="Auto contrast every channel"]').click()
    await page.waitForTimeout(1500)
    const now = await ranges()
    ok = now.length > 0 && now.join('|') !== full
  }
  if (!ok) problems.push(`viewer: auto contrast did not take (channel ranges still ${full})`)
  // On the image: the mask, each segmentation's tracks and the gated populations, each SET (not
  // toggled: all persist). A segmentation not in use folds away under "n more".
  const seg = v.mask ?? 'cells'
  const want = v.outlines !== false
  const onImage = async (label, on) => {
    const b = page.locator(`[aria-label="${label}"]`).first()
    if (!(await b.count()) && on) await page.getByText(/^\d+ more$/).first().click()
    if (!(await b.count())) { if (on) throw new Error(`viewer: no "${label}" on the image`); return }
    if ((await b.getAttribute('aria-pressed')) !== String(on)) await b.click()
  }
  if (want) await onImage(`Mask of ${seg}`, true)
  // outlines off: no segmentation's mask, not only the named one's
  else for (const l of await page.locator('[aria-label^="Mask of "]').evaluateAll(els => els.map(e => e.getAttribute('aria-label'))))
    await onImage(l, false)
  for (const t of v.tracks ?? []) await onImage(`Tracks of ${t}`, true)
  const gated = page.locator('label', { hasText: /^\s*Gated\s*$/ }).locator('input').first()
  if (await gated.count() && (await gated.isChecked()) !== !!v.gated) await gated.evaluate(el => el.click())
  // step the plane away and back: reloads the image and label slices, so a mask turned on before its
  // data arrived is drawn (outlines were missing on some runs without it)
  if (v.plane !== undefined) for (const z of [v.plane + 1, v.plane]) {
    await page.evaluate(p => {
      const inp = document.querySelector('input[aria-label="z plane"]')
      inp.value = String(p); inp.dispatchEvent(new Event('input', { bubbles: true })); inp.dispatchEvent(new Event('change', { bubbles: true }))
    }, z)
    await page.waitForLoadState('networkidle', { timeout: 30000 }).catch(() => {})
    await page.waitForTimeout(1500)
  }
  if (v.time !== undefined) {
    // paused again: switching to 2D or a plane can start playback, which moves the timepoint on
    const pause = page.locator('[aria-label="Pause"]')
    if (await pause.count()) await pause.first().click()
    await page.evaluate(t => {
      const inp = document.querySelector('input[aria-label="Timepoint"]')
      inp.value = String(t); inp.dispatchEvent(new Event('input', { bubbles: true })); inp.dispatchEvent(new Event('change', { bubbles: true }))
    }, v.time)
    await page.waitForLoadState('networkidle', { timeout: 30000 }).catch(() => {})
  }
  await page.waitForTimeout(3000)
  // read the state back: what the caption describes is what was set, not what the last shot left
  const got = await page.evaluate(([seg, tracks]) => {
    const sec = document.querySelector('[aria-label="Auto contrast every channel"]')?.closest('.collapsible-section')
    const gated = [...document.querySelectorAll('label')].find(l => /^\s*Gated\s*$/.test(l.textContent))?.querySelector('input')
    return {
      plane: Number(document.querySelector('input[aria-label="z plane"]')?.value),
      channels: [...(sec?.querySelectorAll('input[aria-label^="Show "]') ?? [])].filter(e => e.checked).map(e => e.getAttribute('aria-label').slice(5)),
      mask: document.querySelector(`[aria-label="Mask of ${seg}"]`)?.getAttribute('aria-pressed') === 'true',
      tracks: tracks.filter(t => document.querySelector(`[aria-label="Tracks of ${t}"]`)?.getAttribute('aria-pressed') === 'true'),
      time: Number(document.querySelector('input[aria-label="Timepoint"]')?.value),
      gated: gated ? gated.checked : null,
      text: document.body.innerText,
    }
  }, [seg, v.tracks ?? []])
  if (v.plane !== undefined && got.plane !== v.plane) problems.push(`viewer: on plane ${got.plane}, not ${v.plane}`)
  if (v.channels && [...got.channels].sort().join() !== [...v.channels].sort().join())
    problems.push(`viewer: channels on are ${got.channels.join(', ')}, not ${v.channels.join(', ')}`)
  if (v.tracks && got.tracks.length !== v.tracks.length) problems.push(`viewer: tracks of ${v.tracks.filter(t => !got.tracks.includes(t)).join(', ')} not drawn`)
  if (v.time !== undefined && got.time !== v.time) problems.push(`viewer: at timepoint ${got.time}, not ${v.time}`)
  if (got.mask !== (v.outlines !== false)) problems.push(`viewer: outlines ${got.mask ? 'on' : 'off'}, asked ${v.outlines !== false ? 'on' : 'off'}`)
  if (got.gated === null ? !!v.gated : got.gated !== !!v.gated) problems.push(`viewer: gated populations ${got.gated ? 'shown' : 'hidden'}, asked ${v.gated ? 'shown' : 'hidden'}`)
  if (v.name && !got.text.includes(v.name)) problems.push(`viewer: does not show image "${v.name}"`)
}

async function runAction(page, act) {
  if ('click' in act) await loc(page, act.click).click({ timeout: 10000 })
  else if ('fill' in act) await loc(page, act.fill).fill(act.text, { timeout: 10000 })
  else if ('hover' in act) await loc(page, act.hover).hover({ timeout: 10000 })
  else if ('press' in act) await page.keyboard.press(act.press)
  else if ('waitFor' in act) await loc(page, act.waitFor).waitFor({ state: 'visible', timeout: 15000 })
  else if ('open' in act) {
    const t = loc(page, act.open)
    if ((await t.getAttribute('aria-expanded', { timeout: 10000 })) !== 'true') await t.click({ timeout: 10000 })
  }
  else if ('scroll' in act) await loc(page, act.scroll).evaluate(el => el.scrollIntoView({ block: 'end' }))
  else if ('panel' in act) await page.evaluate(a => {
    // FloatingPanel's own key and shape (components/FloatingPanel.vue → load)
    localStorage.setItem(`cc.floating.${a.panel}`, JSON.stringify({ x: 260, y: a.y ?? 70, w: a.w, h: a.h, collapsed: false, maximised: false }))
  }, act)
  await settle(page, 30000)
}

async function capture(browser, a, shot, fixture, out, b, theme) {
  const vp = { ...VIEWPORT, height: shot.height ?? VIEWPORT.height }
  const ctx = await browser.newContext({ viewport: vp, deviceScaleFactor: SCALE })
  const page = await ctx.newPage()
  const errors = []
  page.on('pageerror', e => errors.push(String(e).slice(0, 200)))
  const problems = []
  try {
    await signIn(page, a.base, a.configDir)
    await selectProfile(page, shot.profile ?? 'default')
    await closeDialogs(page)
    await vue(page, (g, t) => { const s = g.$pinia._s.get('settings'); s.colourScheme = t; s.plotsMaximised = false; s.kiwiOpen = false }, theme)
    const proj = shot.project ? fixture?.projects[shot.project] : null
    if (shot.project && !proj) throw new Error(`fixture project "${shot.project}" is not set up: run pixi run doc-fixture`)
    if (proj) await openProject(page, proj.uid)
    const image = shot.image ? proj.images[shot.image] : null
    const name = shot.image ? b.fixture.projects[shot.project].images[shot.image] : undefined
    if (shot.viewer) await viewerReady(page, a.base, proj.uid, image, { ...shot.viewer, name }, problems)
    else if (shot.route) await land(page, shot.route, shot.scope ?? '', image ? [image] : [])
    else await settle(page)
    await closeDialogs(page)
    for (const act of shot.actions ?? []) await runAction(page, act)
    await settle(page, 60000)            // what the last action or the landing started (a plot loading)
    await page.addStyleTag({ content: FREEZE })

    for (const m of shot.masks ?? []) await loc(page, m).evaluate(el => el.classList.add('doc-mask')).catch(() => problems.push(`mask ${m} not found`))
    const named = [...new Set([...shot.crop, ...(shot.point ? [shot.point.anchor] : []), ...(shot.expect ?? []).map(e => e.anchor)])]
    const boxes = {}
    for (const an of named) {
      const box = await loc(page, an).boundingBox({ timeout: 3000 }).catch(() => null)
      if (!box || box.width === 0 || box.height === 0) problems.push(`anchor ${an} is missing or not visible`)
      else boxes[an] = box
    }
    for (const e of shot.expect ?? []) {
      const want = fillFacts(e.text, fixture?.facts ?? {})
      if (want.missing.length) { problems.push(`expect cites ${want.missing.join(', ')}, which the fixture did not measure: run pixi run doc-fixture`); continue }
      // the app's label as written (textContent) or as shown (innerText: CSS may upper-case it)
      const text = await loc(page, e.anchor).evaluate(el => `${el.innerText}\n${el.textContent}`, null, { timeout: 2000 }).catch(() => '')
      if (!text.includes(want.text)) problems.push(`"${want.text}" not in ${e.anchor} (it reads "${text.replace(/\s+/g, ' ').slice(0, 80)}")`)
    }
    for (const an of shot.crop) {
      const text = await loc(page, an).evaluate(el => {
        // text of the anchor minus anything masked
        const c = el.cloneNode(true); c.querySelectorAll('.doc-mask').forEach(x => x.remove())
        return el.classList.contains('doc-mask') ? '' : c.innerText ?? c.textContent ?? ''
      }).catch(() => '')
      const hit = text.match(VOLATILE)
      if (hit) problems.push(`${an} shows "${hit[0]}", which changes between captures: mask it or crop it out`)
    }
    // a viewer shot is the image as Save PNG has it: the canvas wrap's chrome (playback bar, minimap, the
    // On the image panel) hidden by the app's own rule (utils/viewerScreenshot.ts → isScreenshotChrome)
    // (the function itself, sent into the page, so the two can never disagree)
    if (shot.viewer) await page.evaluate(([src, keep]) => {
      const chrome = (0, eval)(`(() => { const SCREENSHOT_KEEP = ${JSON.stringify(keep)}; return (${src}) })()`)
      const c = document.querySelector('canvas'), host = c?.parentElement
      for (const el of host?.children ?? []) if (chrome(el, host, c)) el.style.visibility = 'hidden'
    }, [isScreenshotChrome.toString(), SCREENSHOT_KEEP])
    await page.mouse.move(vp.width - 2, vp.height - 2)   // park the mouse: no stray hover
    await page.waitForTimeout(200)
    // the scheme the pixels are in, read off the page (utils/colourScheme.ts: dark is no attribute)
    const shown = await page.evaluate(() => document.documentElement.dataset.theme ?? 'dark')
    if (shown !== theme) throw new Error(`the page is in the ${shown} scheme, not ${theme}`)
    const file = shotFile(shot.id, theme)
    await page.screenshot({ path: join(out, file) })
    writeFileSync(join(out, file.replace(/\.png$/, '.json')), JSON.stringify({
      id: shot.id, viewport: vp, scale: SCALE, boxes, point: shot.point ?? null, crop: shot.crop, theme,
      commit: commit(), capturedAt: new Date().toISOString(), pageErrors: errors,
    }, null, 1))
  } catch (e) {
    problems.push(String(e.message ?? e).split('\n')[0].slice(0, 240))
  } finally {
    // what the shot saw when it failed, to diagnose without rerunning headed
    if (problems.length) await page.screenshot({ path: join(out, shotFile(shot.id, theme).replace(/\.png$/, '.fail.png')) }).catch(() => {})
    await ctx.close()
  }
  return problems
}

async function main() {
  const a = parseArgs(process.argv.slice(2))
  refuseSlotZero(a.base, a.allowDefaultPorts)
  const issues = checkShotSets(SHOT_SETS)
  if (issues.length) { for (const i of issues) console.error(`✗ ${i.set}: ${i.problem}`); process.exit(1) }
  const shots = SHOT_SETS.flatMap(b => b.shots.filter(s => !a.only || a.only === s.id || a.only === b.id).map(s => [b, s]))
  const recordPath = join(a.configDir, FIXTURE_RECORD)
  const record = existsSync(recordPath) ? JSON.parse(readFileSync(recordPath, 'utf8')) : {}
  if (shots.length === 0) throw new Error(a.only ? `no shot or set "${a.only}"` : 'no set defines a shot')
  const out = resolve(a.out)
  mkdirSync(out, { recursive: true })
  const browser = await chromium.launch({ executablePath: findChromium(), headless: true })
  // only when a shot needs it: a headed browser opens a window on the screen
  const headed = shots.some(([, s]) => s.viewer)
    ? await chromium.launch({ executablePath: findChromium(), headless: false, args: VIEWER_ARGS }) : null
  let failed = 0
  // the fixture's numbers, beside the shots they were checked against: compose and the page generators read these
  for (const b of new Set(shots.map(([bk]) => bk)))
    if (record[b.id]?.facts) writeFileSync(join(out, `${b.id}.facts.json`), JSON.stringify(record[b.id].facts, null, 1))
  try {
    for (const [b, s] of shots) for (const theme of shotThemes(b, s)) {
      const problems = await capture(s.viewer ? headed : browser, a, s, record[b.id], out, b, theme)
      if (problems.length) { failed++; console.log(`✗ ${s.id} (${theme})\n${problems.map(p => `    ${p}`).join('\n')}`) }
      else console.log(`✓ ${s.id} (${theme})`)
    }
  } finally {
    await browser.close()
    await headed?.close()
  }
  const n = shots.reduce((k, [b, s]) => k + shotThemes(b, s).length, 0)
  console.log(`${n - failed}/${n} shots → ${out}`)
  if (failed) process.exit(1)
}

main().catch(e => { console.error(`doc-shots: ${e.message}`); process.exit(1) })
