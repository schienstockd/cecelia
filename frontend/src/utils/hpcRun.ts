// Running a task on the cluster from the task runner (docs/todo/HPC_PLAN.md P3).
//
// The env chip under the function name IS the switch: a task whose spec lists "hpc" in `env` offers
// local ⇄ hpc, and on hpc the run carries `target: 'hpc'` plus the resources the user changed from the
// spec's `"hpc"` block. Every ported spec carries the whole block (ratcheted in hpc_dispatch.jl), and
// the backend owns every bound (`hpc_resources`, app/src/hpc/dispatch.jl); this file only shapes the
// form and the request.

import type { TaskDef, HpcMemModel } from '../tasks/types'
import { hpcTestBlocker, hpcEnvLine, type HpcSettings, type HpcEnvStatus } from './hpcSettings'
import { formatBytes, type FrameGeometry } from '../tasks/paramAdvisors'
import type { Severity } from '../lib/severity'

export type RunTarget = 'local' | 'hpc'

export interface HpcResourcesForm {
  cpus: number
  memGb: number
  gpus: number
  walltimeMin: number
}

/** The form's fields: label, wire key, the spec key it defaults from, and its tooltip. */
export const HPC_RESOURCE_FIELDS: { key: keyof HpcResourcesForm; spec: string; label: string; tip: string }[] = [
  { key: 'cpus',        spec: 'cpus',         label: 'CPUs',       tip: 'CPU cores for each image\'s job' },
  { key: 'memGb',       spec: 'mem_gb',       label: 'Memory',     tip: 'Memory for each image\'s job; too little and it is stopped' },
  { key: 'gpus',        spec: 'gpus',         label: 'GPUs',       tip: 'GPUs for each image\'s job' },
  { key: 'walltimeMin', spec: 'walltime_min', label: 'Time limit', tip: 'Each image\'s job is stopped when it reaches this' },
]

/** The slider stops: sizes a cluster hands out, well inside the backend's bounds (dispatch.jl
 *  `_HPC_RESOURCE_KEYS`). Memory and CPUs double; time follows the walltimes queues are cut at. */
export const HPC_STEPS: Record<keyof HpcResourcesForm, number[]> = {
  cpus:        [1, 2, 4, 8, 16, 32, 64],
  memGb:       [4, 8, 16, 32, 64, 128, 256, 512],
  gpus:        [0, 1, 2, 4],
  walltimeMin: [15, 30, 60, 120, 240, 480, 720, 1440, 2880],
}

/** A field's stops, with the task's default and the current value always among them — a spec default
 *  or a remembered size off the ladder stays reachable instead of snapping to a neighbour. */
export function hpcSteps(key: keyof HpcResourcesForm, ...keep: number[]): number[] {
  return [...new Set([...HPC_STEPS[key], ...keep.filter(n => Number.isInteger(n))])].sort((a, b) => a - b)
}

/** A resource value as its slider reads it. */
export function formatResource(key: keyof HpcResourcesForm, v: number): string {
  if (key === 'memGb') return `${v} GB`
  if (key === 'walltimeMin') return formatWalltime(v)
  return String(v)
}

/** The fields this task shows: GPUs only for a GPU task (the class is its pool — `hpc_task_class`). */
export const hpcFieldsFor = (def: TaskDef) =>
  HPC_RESOURCE_FIELDS.filter(f => f.key !== 'gpus' || def.resource_pool === 'gpu')

export function hpcEligible(def: TaskDef | null | undefined): boolean {
  return !!def && def.scope !== 'set' && (def.env ?? []).includes('hpc')
}

/** The spec's `"hpc"` block as form values. A ported spec has every field (ratcheted); a malformed
 *  one shows the smallest job rather than inventing a size. */
export function hpcDefaults(def: TaskDef): HpcResourcesForm {
  const b = def.hpc ?? {}
  return { cpus: b.cpus ?? 1, memGb: b.mem_gb ?? 1, gpus: b.gpus ?? 0, walltimeMin: b.walltime_min ?? 1 }
}

/** A cluster run's stage as the Task Manager shows it (the `task:hpc` frame, `list_tasks` hpc_stage). */
const HPC_STAGE_TEXT: Record<string, string> = {
  sync: 'uploading', queued: 'queued', running: 'running', pull: 'downloading',
}
export function hpcStageLabel(stage?: string, job?: string): { short: string; tip: string } | null {
  if (!stage) return null
  const s = HPC_STAGE_TEXT[stage] ?? stage
  return { short: s, tip: `On the cluster: ${s}${job ? ` (job ${job})` : ''}` }
}

/** What the user chose for this function, remembered per function (profile storage). */
export interface RunTargetChoice { target: RunTarget; resources: Partial<HpcResourcesForm> }

export const runTargetKey = (funName: string) => `cc-run-target:${funName}`

export function parseRunTargetChoice(raw: string | null): RunTargetChoice {
  try {
    const v = raw ? JSON.parse(raw) as Partial<RunTargetChoice> : null
    const resources: Partial<HpcResourcesForm> = {}
    for (const f of HPC_RESOURCE_FIELDS) {
      const n = (v?.resources as Record<string, unknown> | undefined)?.[f.key]
      if (typeof n === 'number' && Number.isFinite(n)) resources[f.key] = n
    }
    return { target: v?.target === 'hpc' ? 'hpc' : 'local', resources }
  } catch {
    return { target: 'local', resources: {} }
  }
}

/** The `task:run` fields for this choice: nothing for a local run (or a task that cannot leave), else
 *  the target and ONLY the resources that differ from the spec, so a changed spec default still applies. */
export function hpcRunFields(def: TaskDef, choice: RunTargetChoice): { target?: RunTarget; hpc?: Partial<HpcResourcesForm> } {
  if (choice.target !== 'hpc' || !hpcEligible(def)) return {}
  const d = hpcDefaults(def)
  const hpc: Partial<HpcResourcesForm> = {}
  for (const f of HPC_RESOURCE_FIELDS) {
    const v = choice.resources[f.key]
    if (v !== undefined && v !== d[f.key]) hpc[f.key] = v
  }
  return { target: 'hpc', hpc }
}

/** A walltime in minutes as a person reads it: `90` → `1 h 30`, `2880` → `2 d`. */
export function formatWalltime(min: number): string {
  const d = Math.floor(min / 1440), h = Math.floor((min % 1440) / 60), m = min % 60
  if (!d && !h) return `${m} min`
  const parts = [d ? `${d} d` : '', h ? `${h} h` : '', m ? String(m).padStart(d || h ? 2 : 1, '0') : '']
  return parts.filter(Boolean).join(' ')
}

/** Where a cluster run of this task goes, as the runner's one line: the class (the task's pool, so a
 *  GPU task with 0 GPUs still uses the GPU queue — dispatch.jl `hpc_task_class`), its queue and the
 *  host; or, while the setup is incomplete, the first missing field and where to set it. */
export function hpcRunDestination(s: HpcSettings | null, def: TaskDef): { short: string; detail: string; severity?: Severity } {
  if (!s) return { short: 'Reading the cluster settings', detail: '' }
  const blocker = hpcTestBlocker(s, 'run')
  if (blocker) return { short: blocker, detail: 'A cluster run needs the cluster, your login and a remote folder', severity: 'warn' }
  const cls = def.resource_pool === 'gpu' ? 'gpu' : 'cpu'
  const c = s.site[cls]
  const account = s.project?.account || c.account
  const label = cls.toUpperCase()
  return {
    short: `${label} job → ${c.queue ? `queue ${c.queue}` : 'default queue'} on ${s.site.host}`,
    detail: [account ? `Account ${account}${s.project?.account ? ' (this project)' : ''}` : 'Your default account',
             c.qos ? `QoS ${c.qos}` : '', `${label} jobs are set in Preferences → HPC`].filter(Boolean).join(' · '),
  }
}

/** The cluster env as the task runner shows it once hpc is picked: nothing while unknown, ready, or
 *  when the settings are incomplete (`hpcRunDestination` names what is missing); otherwise a short
 *  line, with Install offered when this version's env is not on the cluster. The detail is the
 *  Settings → HPC env line (`hpcEnvLine`). */
export function hpcRunEnvNote(e: HpcEnvStatus | null):
    { short: string; detail: string; severity?: Severity; canInstall: boolean } | null {
  if (!e || !e.configured || e.installed) return null
  const detail = hpcEnvLine(e).text
  if (e.installing || e.job) return { short: 'Cluster env installing', detail, canInstall: false }
  if (e.error) return { short: 'Cluster env not checked', detail, severity: 'warn', canInstall: false }
  return { short: 'Cluster env out of date — reinstall', detail, severity: 'warn', canInstall: true }
}

/** One timepoint of an image in memory, in GB — what a segmentation run holds at a time. */
export type FrameSize = Pick<FrameGeometry, 'sizeX' | 'sizeY' | 'sizeZ' | 'sizeC' | 'bytesPerVoxel'>
export const frameGb = (g: FrameSize) =>
  g.sizeX * g.sizeY * (g.sizeZ ?? 1) * (g.sizeC ?? 1) * (g.bytesPerVoxel ?? 2) / 1e9


/** One step's peak memory for one image (GB), from its measured model. */
export function hpcMemNeed(m: HpcMemModel, g: FrameSize, cpus: number): number {
  const f = frameGb(g)
  return m.base_gb + (m.per_plane_gb ?? 0) * (g.sizeZ ?? 1) + (m.per_frame ?? 0) * f +
         cpus * ((m.per_cpu_gb ?? 0) + (m.per_cpu_frame ?? 0) * f)
}

/** The memory the most demanding selected image needs (the largest step's model, from the spec's
 *  `"hpc".mem`), and the smallest memory stop that covers it. `null` when the spec has no model or no
 *  image size is known yet. */
export function hpcMemoryHint(def: TaskDef, frames: FrameSize[], form: HpcResourcesForm):
    { short: string; detail: string; severity?: Severity; suggestGb?: number } | null {
  const models = def.hpc?.mem ?? []
  if (!models.length || !frames.length) return null
  let need = 0, worst = frames[0]
  for (const g of frames) for (const m of models) {
    const n = hpcMemNeed(m, g, form.cpus)
    if (n > need) { need = n; worst = g }
  }
  need = Math.ceil(need)
  const detail = `Largest: ${formatBytes(frameGb(worst) * 1e9)} per timepoint, ${worst.sizeZ ?? 1} planes; held one timepoint at a time`
  if (form.memGb >= need) return { short: `Enough memory: needs about ${need} GB`, detail, severity: 'ok' }
  const suggestGb = HPC_STEPS.memGb.find(s => s >= need) ?? need
  return { short: `Too little memory: needs about ${need} GB`, detail, severity: 'warn', suggestGb }
}
