import { describe, it, expect } from 'vitest'
import { hpcEligible, hpcDefaults, parseRunTargetChoice, hpcRunFields, hpcStageLabel } from './hpcRun'
import type { TaskDef } from '../tasks/types'

const def = (over: Partial<TaskDef> = {}): TaskDef => ({
  fun_name: 'segment.cellpose', task: 'cellpose', label: 'Cellpose', category: 'segment',
  env: ['local', 'hpc'], params: [], resource_pool: 'gpu',
  hpc: { cpus: 8, mem_gb: 64, gpus: 1, walltime_min: 240 }, ...over,
})

describe('hpcRun', () => {
  it('offers the cluster only to a ported, per-image task', () => {
    expect(hpcEligible(def())).toBe(true)
    expect(hpcEligible(def({ env: ['local'] }))).toBe(false)
    expect(hpcEligible(def({ scope: 'set' }))).toBe(false)
    expect(hpcEligible(null)).toBe(false)
  })

  it('defaults from the spec block', () => {
    expect(hpcDefaults(def())).toEqual({ cpus: 8, memGb: 64, gpus: 1, walltimeMin: 240 })
    expect(hpcDefaults(def({ hpc: undefined }))).toEqual({ cpus: 1, memGb: 1, gpus: 0, walltimeMin: 1 })
  })

  it('remembers a choice, and a bad stored value reads as local', () => {
    expect(parseRunTargetChoice(null)).toEqual({ target: 'local', resources: {} })
    expect(parseRunTargetChoice('{')).toEqual({ target: 'local', resources: {} })
    expect(parseRunTargetChoice('{"target":"hpc","resources":{"memGb":128,"cpus":"x","bogus":1}}'))
      .toEqual({ target: 'hpc', resources: { memGb: 128 } })
  })

  it('sends only the target and what differs from the spec', () => {
    expect(hpcRunFields(def(), { target: 'local', resources: { memGb: 128 } })).toEqual({})
    expect(hpcRunFields(def(), { target: 'hpc', resources: {} })).toEqual({ target: 'hpc', hpc: {} })
    expect(hpcRunFields(def(), { target: 'hpc', resources: { memGb: 128, cpus: 8 } }))
      .toEqual({ target: 'hpc', hpc: { memGb: 128 } })
    // a stored hpc choice for a task that is no longer ported runs locally
    expect(hpcRunFields(def({ env: ['local'] }), { target: 'hpc', resources: {} })).toEqual({})
  })

  it('names the stage of a cluster run, with its job once it has one', () => {
    expect(hpcStageLabel(undefined)).toBeNull()
    expect(hpcStageLabel('')).toBeNull()
    expect(hpcStageLabel('sync')).toEqual({ short: 'uploading', tip: 'On the cluster: uploading' })
    expect(hpcStageLabel('queued', '4211')).toEqual({ short: 'queued', tip: 'On the cluster: queued (job 4211)' })
    expect(hpcStageLabel('pull', '4211')?.short).toBe('downloading')
  })
})
