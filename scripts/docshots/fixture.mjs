#!/usr/bin/env node
//
// Set up the doc shots' fixtures (`ShotSet.fixture`) on a running, ISOLATED Cecelia, before capture.
//
//     pixi run doc-fixture -- --base http://127.0.0.1:5223 --config-dir <dir> --source-projects <lab projects dir> [--only <set id>]
//
// - Projects: each listed image is copied raw (scripts/agent_eval/app_project.py — no segmentation,
//   gates or tracks; the source is only read) into the instance's projects dir, renamed to the name the
//   spec gives it, and owned by every fixture profile. Already copied (recorded, dir present) → skipped.
// - Profiles: created through the app's own API (an existing one is kept), with AI switched on or off
//   as the spec says; the instance is left on `default`.
// - Kiwi: a project's `kiwi` question is asked once, on `default` (the terminal's Claude login: one
//   turn of that account's quota), and the reply saved to the Blackboard. A copy that already has a
//   reply (`kiwi/turns.json`) is not asked again.
// - Facts: the numbers a caption may cite (`{{project.image.fact}}`, spec.ts → FACT_TOKEN) are read
//   off the copy through the app — planes and channels from the image's geometry, each gate's count
//   and % of parent from the population manager's stats — and recorded with the uids, so a caption
//   and the screenshot beside it cite one measurement, never a typed number.
// - The record — fixture key → the copy's uids, and the facts — goes to <config-dir>/docshot-fixtures.json, which
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

const { SHOT_SETS } = await import(pathToFileURL(join(repo, 'user-docs', 'shots', 'index.ts')).href)
const { FIXTURE_RECORD, gateKey } = await import(pathToFileURL(join(repo, 'user-docs', 'shots', 'spec.ts')).href)

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
    '--name', spec.name, '--into', into, '--out', scratch, ...(spec.results ? ['--with-results'] : [])], { encoding: 'utf-8' })
  const info = JSON.parse(out.trim().split('\n').pop())
  const copy = { uid: info.projectUid, images: Object.fromEntries(info.images.map(im => [im.sourceImageUid, im.imageUid])) }
  applyNames(into, copy, spec, owners)
  return copy
}

/** The spec's display names and the fixture owners, on a copy — re-applied on every run, so renaming
 *  an image in the spec needs no fresh copy (and keeps its saved Kiwi reply). */
function applyNames(into, copy, spec, owners) {
  for (const [src, uid] of Object.entries(copy.images)) {
    const ccid = join(into, copy.uid, '1', uid, 'ccid.json')
    writeJson(ccid, { ...readJson(ccid), name: spec.images[src] })
  }
  const pj = join(into, copy.uid, 'project.json')
  writeJson(pj, { ...readJson(pj), name: spec.name, owners })
}

/** "2.8k" → 2800: plotmeta's tick labels are raw values, abbreviated. */
const tickValue = s => { const m = String(s).trim().match(/^(-?[\d.]+)\s*([kM]?)$/); return m ? Number(m[1]) * (m[2] === 'k' ? 1e3 : m[2] === 'M' ? 1e6 : 1) : NaN }

/** A raw value's position on a plotted axis, between the two ticks around it (geometric between
 *  positive ticks: logicle is log-like there). The plot's own ticks, so no transform is re-derived. */
function axisPos(raw, ticks, extent, end) {
  if (raw === null) return extent[end === 'min' ? 0 : 1]
  const t = ticks.map(k => ({ pos: k.pos, v: tickValue(k.label) })).filter(k => Number.isFinite(k.v)).sort((a, b) => a.v - b.v)
  for (let i = 0; i < t.length - 1; i++) {
    const a = t[i], b = t[i + 1]
    if (raw < a.v || raw > b.v) continue
    const f = a.v > 0 ? Math.log(raw / a.v) / Math.log(b.v / a.v) : (raw - a.v) / (b.v - a.v)
    return a.pos + f * (b.pos - a.pos)
  }
  throw new Error(`value ${raw} is outside the axis ticks ${t.map(k => k.v).join(', ')}`)
}

/** The source image's own population `g.name` (top level), as its gating file stores it. */
function sourcePop(srcImageDir, g) {
  const file = join(srcImageDir, 'gating', `${g.segmentation ?? 'cells'}${g.popType && g.popType !== 'flow' ? `__${g.popType}` : ''}.json`)
  const pop = (readJson(file).populations ?? []).find(p => p.name === g.name)
  if (!pop) throw new Error(`source ${file} has no population "${g.name}"`)
  return pop
}

/** Recreate gate `g` as a person's gate (the population manager's own route); false when a population
 *  of that name exists already. A box is placed by its channels' names, as the plot lists them; a
 *  `fromSource` one takes the source population's gate or filter as stored. */
async function addGate(api, projectUid, imageUid, g, srcImageDir) {
  const valueName = g.segmentation ?? 'cells', popType = g.popType ?? 'flow'
  const q = `projectUid=${projectUid}&imageUid=${imageUid}&valueName=${valueName}&popType=${popType}`
  const tree = (await api(`/api/gating/popmap?${q}`)).body.tree
  // the root's populations (docs/API.md's `{name,gate,filter,children}` is each node below it)
  if ((tree?.populations ?? []).some(c => c.name === g.name)) return false
  if (g.fromSource) {
    const pop = sourcePop(srcImageDir, g)
    const r = await api('/api/gating/pop/add', { projectUid, imageUid, valueName, popType, name: g.name, parent: 'root',
      colour: pop.colour, ...(pop.gate ? { gate: pop.gate } : { filter: pop.filter }) })
    if (!r.ok) throw new Error(`gate ${valueName}/${g.name}: ${r.body.error ?? JSON.stringify(r.body).slice(0, 160)}`)
    return true
  }
  const ch = (await api(`/api/gating/channels?${q}`)).body
  const col = name => {
    const i = (ch.channelNames ?? []).indexOf(name)
    if (i < 0 || !ch.channels?.[i]) throw new Error(`no gateable column for channel "${name}" (has ${(ch.channelNames ?? []).join(', ')})`)
    return ch.channels[i]
  }
  const x = col(g.x), y = col(g.y)
  const m = (await api(`/api/gating/plotmeta?${q}&x=${x}&y=${y}&pop=root&xt=logicle&yt=logicle`)).body
  if (!m.xTicks) throw new Error(`plotmeta for ${g.x} × ${g.y}: ${JSON.stringify(m).slice(0, 160)}`)
  const gate = { kind: 'rectangle', x_channel: x, y_channel: y, x_transform: m.xSpec, y_transform: m.ySpec,
    x_min: axisPos(g.xRange[0], m.xTicks, m.xExtent, 'min'), x_max: axisPos(g.xRange[1], m.xTicks, m.xExtent, 'max'),
    y_min: axisPos(g.yRange[0], m.yTicks, m.yExtent, 'min'), y_max: axisPos(g.yRange[1], m.yTicks, m.yExtent, 'max') }
  const r = await api('/api/gating/pop/add', { projectUid, imageUid, valueName: 'cells', popType: 'flow', name: g.name, parent: 'root', gate })
  if (!r.ok) throw new Error(`gate ${g.name}: ${r.body.error ?? JSON.stringify(r.body).slice(0, 160)}`)
  return true
}

/** The facts a caption may cite, as the app shows them: the name, planes and channels of each copied image
 *  (its active version's geometry), and each gate's count and % of parent (the population manager's
 *  stats, printed as it prints them: count as is, % to one decimal). Keyed `project.image.fact`. */
async function measure(api, fixture, rec) {
  const facts = {}
  for (const [key, spec] of Object.entries(fixture.projects ?? {})) {
    const copy = rec.projects[key]
    // the names the copies show, as the app lists them (a caption names the sample by these)
    const listing = await api(`/api/images?projectUid=${copy.uid}`)
    if (!listing.ok) throw new Error(`images of ${key}: ${listing.body.error ?? 'failed'}`)
    for (const src of Object.keys(spec.images)) {
      const shown = listing.body.images.find(i => i.uid === copy.images[src])
      if (!shown) throw new Error(`${key}/${src}: the copy is not in its project's image list`)
      facts[`${key}.${src}.name`] = shown.name
      const g = await api(`/api/images/geometry?projectUid=${copy.uid}&imageUid=${copy.images[src]}`)
      if (!g.ok) throw new Error(`geometry of ${key}/${src}: ${g.body.error ?? 'failed'}`)
      Object.assign(facts, { [`${key}.${src}.planes`]: g.body.sizeZ, [`${key}.${src}.channels`]: g.body.sizeC,
                             [`${key}.${src}.timepoints`]: g.body.sizeT })
    }
    const tracked = new Set()
    for (const gate of spec.gates ?? []) {
      const vn = gate.segmentation ?? 'cells'
      const q = `projectUid=${copy.uid}&imageUid=${copy.images[gate.image]}&valueName=${vn}&popType=${gate.popType ?? 'flow'}&pop=${encodeURIComponent(`/${gate.name}`)}`
      const st = await api(`/api/gating/stats?${q}`)
      if (!st.ok) throw new Error(`stats of gate ${vn}/${gate.name}: ${st.body.error ?? 'failed'}`)
      Object.assign(facts, { [`${key}.${gate.image}.${gateKey(gate)}.count`]: st.body.count,
                             [`${key}.${gate.image}.${gateKey(gate)}.pct`]: Number(st.body.pctParent).toFixed(1) })
      if (vn !== 'cells') tracked.add(`${gate.image}|${vn}`)
    }
    // tracks and the frame interval, as the tracking review counts them (an untracked one is left out)
    for (const t of tracked) {
      const [src, vn] = t.split('|')
      const r = await api(`/api/tracking/issues?projectUid=${copy.uid}&imageUid=${copy.images[src]}&valueName=${vn}&limit=1`)
      if (!r.ok || !r.body.tracked) continue
      facts[`${key}.${src}.${vn}.tracks`] = r.body.nTracks
      // timeStep is in minutes (the track measures' unit); the caption says seconds, as the image records them
      if (r.body.timeStep) facts[`${key}.${src}.interval`] = Math.round(r.body.timeStep * 60 * 100) / 100
    }
  }
  return facts
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
  const sets = SHOT_SETS.filter(b => b.fixture && (!a.only || a.only === b.id))
  if (sets.length === 0) throw new Error(a.only ? `shot set "${a.only}" has no fixture` : 'no shot set has a fixture')

  const browser = await chromium.launch({ executablePath: findChromium(), headless: true })
  try {
    const page = await (await browser.newContext()).newPage()
    await signIn(page, a.base, a.configDir)
    const api = (url, body) => page.evaluate(async ([u, b]) => {
      const r = await fetch(u, b === undefined ? {} : { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(b) })
      return { ok: r.ok, body: await r.json().catch(() => ({})) }
    }, [url, body])

    for (const b of sets) {
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
        if (have && existsSync(join(into, have.uid))) {
          applyNames(into, have, spec, owners)
          console.log(`· ${b.id}: project ${key} already copied (${have.uid}); names re-applied`)
          continue
        }
        rec.projects[key] = copyProject(resolve(a.sourceProjects), spec, into, owners, join(a.configDir, '.fixture-copy.json'))
        console.log(`✓ ${b.id}: project ${key} → ${rec.projects[key].uid}`)
      }
      record[b.id] = rec
      for (const [key, spec] of Object.entries(b.fixture.projects ?? {})) {
        const copy = rec.projects[key]
        for (const [src, names] of Object.entries(spec.channels ?? {})) {
          const r = await api('/api/images/channelnames', { projectUid: copy.uid, imageUids: [copy.images[src]], channelNames: names })
          if (!r.ok) throw new Error(`channel names on ${key}/${src}: ${r.body.error ?? 'failed'}`)
        }
        for (const g of spec.gates ?? []) {
          const srcDir = join(resolve(a.sourceProjects), spec.source, '1', g.image)
          const made = await addGate(api, copy.uid, copy.images[g.image], g, srcDir)
          console.log(`${made ? '✓' : '·'} ${b.id}: project ${key}: gate ${g.segmentation ?? 'cells'}/${g.name}${made ? '' : ' already there'}`)
        }
      }
      rec.facts = await measure(api, b.fixture, rec)
      console.log(`✓ ${b.id}: facts ${Object.entries(rec.facts).map(([k, v]) => `${k}=${v}`).join(', ')}`)
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

main().catch(e => { console.error(`doc-fixture: ${e.message}`); process.exit(1) })
