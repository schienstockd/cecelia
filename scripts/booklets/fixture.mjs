#!/usr/bin/env node
//
// Set up the booklets' fixtures (`Booklet.fixture`) on a running, ISOLATED Cecelia, before capture.
//
//     pixi run booklet-fixture -- --base http://127.0.0.1:5223 --config-dir <dir> --source-projects <lab projects dir> [--only <booklet id>]
//
// - Projects: each listed image is copied raw (scripts/agent_eval/app_project.py — no segmentation,
//   gates or tracks; the source is only read) into the instance's projects dir, renamed to the name the
//   spec gives it, and owned by every fixture profile. Already copied (recorded, dir present) → skipped.
// - Profiles: created through the app's own API (an existing one is kept), with AI switched on or off
//   as the spec says; the instance is left on `default`.
// - Kiwi: a project's `kiwi` question is asked once, on `default` (the terminal's Claude login: one
//   turn of that account's quota), and the reply saved to the Blackboard. A copy that already has a
//   reply (`kiwi/turns.json`) is not asked again.
// - The record — fixture key → the copy's uids — goes to <config-dir>/booklet-fixtures.json, which
//   capture.mjs reads to resolve a shot's `project` and `image`.
//
// Refuses 8080/5173, and refuses a projects dir outside --config-dir: a copy must never land among the
// lab's real projects.

import { execFileSync } from 'node:child_process'
import { existsSync, readFileSync, writeFileSync } from 'node:fs'
import { join, resolve, sep } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium, findChromium, repo } from '../lib/chromium.mjs'
import { openProject, refuseSlotZero, selectProfile, signIn, vue } from '../lib/app_page.mjs'

const { BOOKLETS } = await import(pathToFileURL(join(repo, 'user-docs', 'booklets', 'index.ts')).href)
const { FIXTURE_RECORD } = await import(pathToFileURL(join(repo, 'user-docs', 'booklets', 'spec.ts')).href)

function parseArgs(argv) {
  const a = { only: '' }
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i]
    if (k === '--allow-default-ports') { a.allowDefaultPorts = true; continue }
    if (!k.startsWith('--')) throw new Error(`unexpected argument ${k}`)
    a[k.slice(2).replace(/-(\w)/g, (_, c) => c.toUpperCase())] = argv[++i]
  }
  if (!a.base || !a.configDir || !a.sourceProjects) throw new Error('need --base URL, --config-dir DIR and --source-projects DIR')
  return a
}

/** The instance's projects dir (custom.toml `[dirs] projects`), which must sit inside the config dir. */
function projectsDir(configDir) {
  const toml = readFileSync(join(configDir, 'custom.toml'), 'utf8')
  const m = toml.match(/^\[dirs\][^[]*?^projects\s*=\s*"([^"]+)"/ms)
  if (!m) throw new Error(`${configDir}/custom.toml names no [dirs] projects (capture.mjs header: setting one up)`)
  const dir = resolve(m[1]), root = resolve(configDir)
  if (!dir.startsWith(root + sep)) throw new Error(`projects dir ${dir} is outside ${root}: refusing to copy lab data there`)
  return dir
}

const readJson = p => JSON.parse(readFileSync(p, 'utf8'))
const writeJson = (p, v) => writeFileSync(p, JSON.stringify(v, null, 4))

function copyProject(src, spec, into, owners, scratch) {
  const images = Object.keys(spec.images)
  const out = execFileSync('python', [join(repo, 'scripts', 'agent_eval', 'app_project.py'),
    '--projects-dir', src, '--source-project', spec.source, ...images.flatMap(i => ['--image', i]),
    '--name', spec.name, '--into', into, '--out', scratch], { encoding: 'utf-8' })
  const info = JSON.parse(out.trim().split('\n').pop())
  const map = {}
  for (const im of info.images) {
    map[im.sourceImageUid] = im.imageUid
    const ccid = join(info.projectDir, '1', im.imageUid, 'ccid.json')
    writeJson(ccid, { ...readJson(ccid), name: spec.images[im.sourceImageUid] })
  }
  const pj = join(info.projectDir, 'project.json')
  writeJson(pj, { ...readJson(pj), owners })
  return { uid: info.projectUid, images: map }
}

/** Ask Kiwi `question` about `imageUid` in project `uid` as the active profile, and save the reply to
 *  the Blackboard through the turn's own Save button. A turn takes minutes. */
async function askAndSave(page, uid, imageUid, question) {
  await selectProfile(page, 'default')
  await openProject(page, uid)
  // the kiwi store exists once its panel has mounted
  await vue(page, g => { g.$pinia._s.get('settings').kiwiOpen = true })
  await page.locator('[data-guide="kiwi.ask"]').waitFor({ state: 'visible', timeout: 20000 })
  await vue(page, (g, a) => {
    const k = g.$pinia._s.get('kiwi')
    k.clearDraft()
    k.addRef({ kind: 'image', imageUid: a.imageUid }, a.uid)
    k.prompt = a.question
    return k.ask()
  }, { uid, imageUid, question })
  const t0 = Date.now()
  while (await vue(page, g => g.$pinia._s.get('kiwi').busy)) {
    if (Date.now() - t0 > 10 * 60_000) throw new Error('Kiwi did not answer within 10 min')
    await page.waitForTimeout(5000)
  }
  const err = await vue(page, g => g.$pinia._s.get('kiwi').error)
  if (err) throw new Error(`Kiwi: ${err}`)
  await page.locator('[data-guide="kiwi.turnActions"]').first().getByText('Save', { exact: true }).click({ timeout: 15000 })
  await page.waitForTimeout(2000)
}

async function main() {
  const a = parseArgs(process.argv.slice(2))
  refuseSlotZero(a.base, a.allowDefaultPorts)
  const into = projectsDir(a.configDir)
  const recordPath = join(a.configDir, FIXTURE_RECORD)
  const record = existsSync(recordPath) ? readJson(recordPath) : {}
  const booklets = BOOKLETS.filter(b => b.fixture && (!a.only || a.only === b.id))
  if (booklets.length === 0) throw new Error(a.only ? `booklet "${a.only}" has no fixture` : 'no booklet has a fixture')

  const browser = await chromium.launch({ executablePath: findChromium(), headless: true })
  try {
    const page = await (await browser.newContext()).newPage()
    await signIn(page, a.base, a.configDir)
    const api = (url, body) => page.evaluate(async ([u, b]) => {
      const r = await fetch(u, b === undefined ? {} : { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(b) })
      return { ok: r.ok, body: await r.json().catch(() => ({})) }
    }, [url, body])

    for (const b of booklets) {
      const profiles = b.fixture.profiles ?? []
      const roster = (await api('/api/kiwi/profiles')).body.profiles?.map(p => p.name) ?? []
      for (const p of profiles) {
        if (!roster.includes(p.name)) {
          const r = await api('/api/kiwi/profiles/create', { name: p.name })
          if (!r.ok) throw new Error(`create profile ${p.name}: ${r.body.error ?? 'failed'}`)
        }
        if (!(await api('/api/kiwi/profiles/select', { name: p.name })).ok) throw new Error(`select profile ${p.name}`)
        if (!(await api('/api/profile/settings/patch', { aiEnabled: p.ai })).ok) throw new Error(`AI ${p.ai ? 'on' : 'off'} for ${p.name}`)
        console.log(`✓ ${b.id}: profile ${p.name}, AI ${p.ai ? 'on' : 'off'}`)
      }
      await api('/api/kiwi/profiles/select', { name: 'default' })

      const owners = [...new Set(['default', ...profiles.map(p => p.name)])]
      const rec = record[b.id] ?? { projects: {} }
      for (const [key, spec] of Object.entries(b.fixture.projects ?? {})) {
        const have = rec.projects[key]
        if (have && existsSync(join(into, have.uid))) { console.log(`· ${b.id}: project ${key} already copied (${have.uid})`); continue }
        rec.projects[key] = copyProject(resolve(a.sourceProjects), spec, into, owners, join(a.configDir, '.fixture-copy.json'))
        console.log(`✓ ${b.id}: project ${key} → ${rec.projects[key].uid}`)
      }
      record[b.id] = rec
      for (const [key, spec] of Object.entries(b.fixture.projects ?? {})) {
        if (!spec.kiwi) continue
        const copy = rec.projects[key]
        if (existsSync(join(into, copy.uid, 'kiwi', 'turns.json'))) { console.log(`· ${b.id}: project ${key} already has a Kiwi reply`); continue }
        await askAndSave(page, copy.uid, copy.images[spec.kiwi.image], spec.kiwi.question)
        console.log(`✓ ${b.id}: project ${key}: Kiwi answered and the reply is on the Blackboard`)
      }
    }
  } finally {
    await browser.close()
  }
  writeJson(recordPath, record)
  console.log(`fixtures → ${recordPath}`)
}

main().catch(e => { console.error(`booklet-fixture: ${e.message}`); process.exit(1) })
