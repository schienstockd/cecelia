// Settings → HPC: API client + the pure display logic (docs/todo/HPC_PLAN.md P1).
//
//   GET  /api/hpc/settings?projectUid=   → HpcSettings
//   POST /api/hpc/settings/set           → HpcSettings, or {error} (400) naming every problem
//   POST /api/hpc/test                   → HpcTestResult (on the SAVED settings)
//
// Storage is split three ways by the backend (`app/src/config/hpc.jl`): site in custom.toml,
// username + key per profile, account override per project. This file only shapes requests.

import { svcPost } from './serviceApi'

export interface HpcClassSettings {
  queue: string
  account: string
  qos: string
  directives: string
  preamble: string
}

export interface HpcSite {
  scheduler: string
  host: string
  port: number
  netQueue: string      // a queue whose nodes reach the internet, for downloads; '' = the CPU queue
  remoteDir: string
  email: string
  cpu: HpcClassSettings
  gpu: HpcClassSettings
}

export interface HpcSettings {
  available: boolean
  schedulers: { name: string; label: string; available: boolean }[]
  site: HpcSite
  identity: { profile: string; user: string; keyFile: string }
  project: { projectUid: string; account: string } | null
}

export interface HpcTestResult {
  ok: boolean
  reachable: boolean
  fingerprint: string
  newHostKey: boolean
  remoteDir: string
  dirOk: boolean
  scheduler: string
  error: string
}

export type HpcClassName = 'cpu' | 'gpu'

/** A partial update: any subset of the three scopes. */
export interface HpcPatch {
  site?: Partial<Omit<HpcSite, 'cpu' | 'gpu'>> & { cpu?: Partial<HpcClassSettings>; gpu?: Partial<HpcClassSettings> }
  identity?: { user?: string; keyFile?: string }
  project?: { projectUid: string; account: string }
}

export async function fetchHpcSettings(projectUid = ''): Promise<HpcSettings> {
  const q = projectUid ? `?projectUid=${encodeURIComponent(projectUid)}` : ''
  const res = await fetch(`/api/hpc/settings${q}`)
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as { error?: string })?.error ?? `HTTP ${res.status}`)
  return data as HpcSettings
}

export async function saveHpcSettings(patch: HpcPatch, projectUid = ''): Promise<HpcSettings> {
  return await svcPost('/api/hpc/settings/set', { ...patch, projectUid }) as HpcSettings
}

/** The backend gives up after 30 s; the extra margin lets its own message arrive first. */
export async function testHpcConnection(): Promise<HpcTestResult> {
  return await svcPost('/api/hpc/test', {}, 40_000) as HpcTestResult
}

export type HpcScope = 'machine' | 'user'

/** Why Test connection cannot run yet, or '' when it can, in the backend's order. `scope` is where the button is: a missing field that lives on
 *  the OTHER surface (Settings → HPC holds the cluster, Preferences → HPC the login) names it; `'run'`
 *  (the task runner) names both. */
export function hpcTestBlocker(s: HpcSettings | null, scope: HpcScope | 'run' = 'machine'): string {
  if (!s) return 'Loading'
  if (!s.available) return 'Not available on Windows yet'
  const where = (home: HpcScope) => home === scope ? '' : ` in ${home === 'machine' ? 'Settings' : 'Preferences'} → HPC`
  if (!s.site.host.trim()) return `Set the cluster host${where('machine')} first`
  if (!s.identity.user.trim()) return `Set your username${where('user')} first`
  if (!s.identity.keyFile.trim()) return `Select your SSH key${where('user')} first`
  if (!s.site.remoteDir.trim()) return `Set the remote folder${where('user')} first`
  return ''
}

export interface HpcTestLine { ok: boolean; text: string }

/** The result as short lines, one per check, in the order they happen. */
export function hpcTestLines(r: HpcTestResult, schedulerLabel = 'SLURM'): HpcTestLine[] {
  const lines: HpcTestLine[] = []
  if (r.fingerprint) {
    lines.push({ ok: true, text: `Host key ${r.fingerprint}${r.newHostKey ? ' (new, now trusted)' : ''}` })
  }
  if (!r.reachable) {
    lines.push({ ok: false, text: r.error || 'Not reachable' })
    return lines
  }
  lines.push({ ok: r.dirOk, text: r.dirOk ? `Folder ${r.remoteDir}` : (r.error || 'Remote folder not writable') })
  if (r.dirOk) {
    lines.push({ ok: !!r.scheduler, text: r.scheduler ? `${schedulerLabel}: ${r.scheduler}` : (r.error || `${schedulerLabel} not found`) })
  }
  return lines
}

/** Folder the key picker opens in: the current key's folder, else `~/.ssh` (relative = from home). */
export function keyPickerStart(keyFile: string): string {
  const k = keyFile.trim()
  if (!k) return '.ssh'
  const cut = Math.max(k.lastIndexOf('/'), k.lastIndexOf('\\'))
  if (cut <= 0) return '.ssh'
  const dir = k.slice(0, cut)
  return dir.startsWith('~/') ? dir.slice(2) : dir === '~' ? '' : dir
}

/** Every editable field, as a dotted path into `HpcSettings`. One draft per path (useHpcDrafts). */
export const HPC_DRAFT_FIELDS = [
  'site.scheduler', 'site.host', 'site.port', 'site.netQueue', 'site.remoteDir', 'site.email',
  'identity.user', 'identity.keyFile', 'project.account',
  ...(['cpu', 'gpu'] as const).flatMap(c =>
    (['queue', 'account', 'qos', 'directives', 'preamble'] as const).map(k => `site.${c}.${k}`)),
] as const

/** The stored value at `path`, as the text a field shows ('' when absent). */
export function hpcFieldValue(s: HpcSettings | null, path: string): string {
  let v: unknown = s
  for (const k of path.split('.')) v = v == null ? undefined : (v as Record<string, unknown>)[k]
  return v == null ? '' : String(v)
}

/** The save request for one field, from its draft text. `site.port` is sent as a number; text that is
 *  not a whole number goes as 0, so the server's range check refuses it instead of keeping 22. */
export function hpcFieldPatch(path: string, text: string, projectUid: string): HpcPatch {
  const [scope, a, b] = path.split('.')
  if (scope === 'project') return { project: { projectUid, account: text } }
  if (scope === 'identity') return { identity: { [a]: text } }
  if (b) return { site: { [a]: { [b]: text } } }
  return { site: { [a]: a === 'port' ? (/^\d+$/.test(text.trim()) ? Number(text) : 0) : text } }
}

// ── The cluster environment (HPC_PLAN P2) ──────────────────────────────────────────────────────────
//
//   GET  /api/hpc/env          → HpcEnvStatus (one ssh call, skipped while installing)
//   POST /api/hpc/env/install  → 202 {jobId}; the job runs on the task rail as `hpc-env-install`
//   POST /api/hpc/env/cleanup  {all} → 202 {jobId} (`hpc-env-cleanup`); 409 {error} = why not now

export interface HpcEnvStatus {
  configured: boolean
  lock: string          // this version's pixi.lock hash: the env dir on the cluster
  installed: boolean
  job: string           // an install job queued or running on the cluster, '' if none
  installing: boolean   // this backend is running the install
  stage?: string        // while installing: 'sync' | 'queued' | 'running' (the row's `task:hpc` stage)
  cleaning?: boolean    // this backend is running Clean up / Delete all
  clusterCleaning?: boolean  // a clean-up is under way on the cluster (any session)
  error: string
}

/** The rail's task id for the install; `stores/ws.ts` tells `onHpcEnvChanged` listeners when it ends. */
export const HPC_ENV_JOB_ID = 'hpc-env-install'
/** The rail's task id for Clean up / Delete all; its end re-reads the env too. */
export const HPC_CLEAN_JOB_ID = 'hpc-env-cleanup'

export async function fetchHpcEnv(): Promise<HpcEnvStatus> {
  const res = await fetch('/api/hpc/env')
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as { error?: string })?.error ?? `HTTP ${res.status}`)
  return data as HpcEnvStatus
}

export async function installHpcEnv(): Promise<void> {
  await svcPost('/api/hpc/env/install', {})
}

/** Clean up (`all = false`: old versions, the download cache, old job folders) or Delete all. Throws
 *  the refusal (a job still on the cluster, an install running) as the error's message. */
export async function cleanupHpcEnv(all: boolean): Promise<void> {
  await svcPost('/api/hpc/env/cleanup', { all }, 70_000)   // asks the scheduler first: one ssh call
}

/** What a running install is doing, from its Task Manager row's stage (else the GET's). */
const INSTALL_STAGE_TEXT: Record<string, (job: string) => string> = {
  sync:    () => 'Uploading the environment files',
  queued:  j => `Queued on the cluster${j ? ` (job ${j})` : ''}`,
  running: j => `Installing on the cluster${j ? ` (job ${j})` : ''}`,
}

/** The environment's state as one line (no severity = in progress, an info note), and whether
 *  Install is offered. `live` is the install row's current stage, which moves without a re-fetch. */
export function hpcEnvLine(e: HpcEnvStatus | null, live?: { stage?: string; job?: string }):
    { severity?: 'ok' | 'fail'; text: string; canInstall: boolean } {
  if (!e) return { text: 'Checking the cluster…', canInstall: false }
  if (e.cleaning || e.clusterCleaning) return { text: 'Deleting files on the cluster', canInstall: false }
  if (e.installing) {
    const stage = live?.stage || e.stage || ''
    const say = INSTALL_STAGE_TEXT[stage]
    return { text: say ? say(live?.job || e.job || '') : 'Installing — follow it in the Task Manager', canInstall: false }
  }
  if (e.error) return { severity: 'fail', text: e.error, canInstall: e.configured }
  if (e.installed) return { severity: 'ok', text: `Environment ready (${e.lock})`, canInstall: false }
  if (e.job) return { text: `Installing on the cluster (job ${e.job})`, canInstall: true }
  return { severity: 'fail', text: 'Not installed for this version', canInstall: true }
}

const _envListeners = new Set<() => void>()

/** Run `cb` when the install job ends. Returns the unsubscribe. */
export function onHpcEnvChanged(cb: () => void): () => void {
  _envListeners.add(cb)
  return () => { _envListeners.delete(cb) }
}

export function notifyHpcEnvChanged(): void {
  for (const cb of [..._envListeners]) cb()
}
