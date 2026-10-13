// Running a task on the cluster from the task runner (docs/todo/HPC_PLAN.md P3).
//
// The env chip under the function name IS the switch: a task whose spec lists "hpc" in `env` offers
// local ⇄ hpc, and on hpc the run carries `target: 'hpc'` plus the resources the user changed from the
// spec's `"hpc"` block. Every ported spec carries the whole block (ratcheted in hpc_dispatch.jl), and
// the backend owns every bound (`hpc_resources`, app/src/hpc/dispatch.jl); this file only shapes the
// form and the request.

import type { TaskDef } from '../tasks/types'

export type RunTarget = 'local' | 'hpc'

export interface HpcResourcesForm {
  cpus: number
  memGb: number
  gpus: number
  walltimeMin: number
}

/** The form's fields: label, wire key, the spec key it defaults from, and its tooltip. */
export const HPC_RESOURCE_FIELDS: { key: keyof HpcResourcesForm; spec: string; label: string; tip: string }[] = [
  { key: 'cpus',        spec: 'cpus',         label: 'CPUs',    tip: 'CPU cores for the cluster job' },
  { key: 'memGb',       spec: 'mem_gb',       label: 'GB',      tip: 'Memory for the cluster job, in GB' },
  { key: 'gpus',        spec: 'gpus',         label: 'GPUs',    tip: 'GPUs for the cluster job' },
  { key: 'walltimeMin', spec: 'walltime_min', label: 'Minutes', tip: 'Time limit for the cluster job' },
]

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
