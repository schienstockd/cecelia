import { describe, it, expect } from 'vitest'
import { hpcEligible, hpcDefaults, parseRunTargetChoice, hpcRunFields, hpcStageLabel, formatWalltime, hpcRunDestination, hpcRunEnvNote,
  hpcSteps, hpcFieldsFor, formatResource, hpcMemoryHint, frameGb } from './hpcRun'
import type { HpcSettings, HpcEnvStatus } from './hpcSettings'
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

  it('reads a walltime as a person would', () => {
    expect(formatWalltime(45)).toBe('45 min')
    expect(formatWalltime(60)).toBe('1 h')
    expect(formatWalltime(90)).toBe('1 h 30')
    expect(formatWalltime(240)).toBe('4 h')
    expect(formatWalltime(2880)).toBe('2 d')
    expect(formatWalltime(1505)).toBe('1 d 1 h 05')
  })

  const cls = (over = {}) => ({ queue: '', account: '', qos: '', directives: '', preamble: '', ...over })
  const settings = (over: Partial<HpcSettings> = {}): HpcSettings => ({
    available: true, schedulers: [],
    site: { scheduler: 'slurm', host: 'hpc.example.edu', port: 22, netQueue: '', remoteDir: '~/cecelia', email: '',
            cpu: cls({ queue: 'cpu,long', account: 'p1' }), gpu: cls({ queue: 'gpu-a100', account: 'p2', qos: 'gpu' }) },
    identity: { profile: 'default', user: 'jsmith', keyFile: '~/.ssh/id_ed25519' },
    project: null, ...over,
  })

  it('names where a cluster run goes, by the task pool', () => {
    expect(hpcRunDestination(settings(), def()))
      .toEqual({ short: 'GPU job → queue gpu-a100 on hpc.example.edu',
                 detail: 'Account p2 · QoS gpu · GPU jobs are set in Preferences → HPC' })
    // the class follows the task's pool, not its GPU count (dispatch.jl hpc_task_class)
    expect(hpcRunDestination(settings(), def({ resource_pool: 'cpu' })).short).toBe('CPU job → queue cpu,long on hpc.example.edu')
    const s = settings({ project: { projectUid: 'x', account: 'grant9' } })
    expect(hpcRunDestination(s, def()).detail).toMatch(/^Account grant9 \(this project\)/)
    const empty = settings()
    empty.site.gpu.queue = ''
    expect(hpcRunDestination(empty, def()).short).toBe('GPU job → default queue on hpc.example.edu')
  })

  it('names the missing setup, and where it lives, before a cluster run can go', () => {
    const noHost = settings(); noHost.site.host = ''
    expect(hpcRunDestination(noHost, def())).toMatchObject({ short: 'Set the cluster host in Settings → HPC first', severity: 'warn' })
    const noUser = settings({ identity: { profile: 'default', user: '', keyFile: '' } })
    expect(hpcRunDestination(noUser, def()).short).toBe('Set your username in Preferences → HPC first')
    expect(hpcRunDestination(null, def()).severity).toBeUndefined()
  })

  it('slider stops keep the default and the current size reachable', () => {
    expect(hpcSteps('cpus')).toEqual([1, 2, 4, 8, 16, 32, 64])
    expect(hpcSteps('cpus', 8, 12)).toEqual([1, 2, 4, 8, 12, 16, 32, 64])
    expect(hpcSteps('memGb', NaN)).toEqual([4, 8, 16, 32, 64, 128, 256, 512])
    expect(formatResource('memGb', 64)).toBe('64 GB')
    expect(formatResource('walltimeMin', 480)).toBe('8 h')
    expect(formatResource('cpus', 8)).toBe('8')
  })

  it('shows GPUs only for a GPU task', () => {
    expect(hpcFieldsFor(def()).map(f => f.key)).toContain('gpus')
    expect(hpcFieldsFor(def({ resource_pool: 'cpu' })).map(f => f.key)).not.toContain('gpus')
  })

  it('sizes one frame in memory', () => {
    expect(frameGb({ sizeX: 1000, sizeY: 1000, sizeZ: 10, sizeC: 2, bytesPerVoxel: 2 })).toBeCloseTo(0.04)
    expect(frameGb({ sizeX: 1000, sizeY: 1000 })).toBeCloseTo(0.002)     // 2D, one channel, uint16
  })

  it('names the memory the most demanding image needs, from the largest step model', () => {
    const seg = { base_gb: 2, per_plane_gb: 0.05, per_frame: 4 }
    const measure = { base_gb: 1, per_cpu_gb: 0.5, per_cpu_frame: 1 }
    const d = def({ hpc: { cpus: 8, mem_gb: 64, gpus: 1, walltime_min: 240, mem: [seg, measure] } })
    const form = { cpus: 8, memGb: 16, gpus: 1, walltimeMin: 240 }
    const small = { sizeX: 1000, sizeY: 1000, sizeZ: 10, sizeC: 2, bytesPerVoxel: 2 }      // 0.04 GB
    const big = { sizeX: 2000, sizeY: 2000, sizeZ: 50, sizeC: 2, bytesPerVoxel: 2 }        // 0.8 GB
    // seg: 2 + 2.5 + 3.2 = 7.7 · measure: 1 + 8·(0.5 + 0.8) = 11.4 → 12, fits in 16
    expect(hpcMemoryHint(d, [small, big], form)).toMatchObject({ severity: 'ok', short: 'Enough memory: needs about 12 GB' })
    expect(hpcMemoryHint(d, [small, big], form)?.detail).toMatch(/^Largest: 800 MB|^Largest: 0\.8/)
    // more CPUs → more measure workers: 1 + 12·1.3 = 16.6 → 17 > 16: warn, offer the next stop
    expect(hpcMemoryHint(d, [big], { ...form, cpus: 12 })).toMatchObject({ severity: 'warn', suggestGb: 32 })
    expect(hpcMemoryHint(d, [], form)).toBeNull()
    expect(hpcMemoryHint(def(), [big], form)).toBeNull()     // no model in the spec
  })
})

describe('hpcRunEnvNote — the cluster env, when hpc is picked', () => {
  const env = (p: Partial<HpcEnvStatus> = {}): HpcEnvStatus =>
    ({ configured: true, lock: 'abc123def456', installed: false, job: '', installing: false, error: '', ...p })

  it('says nothing while unknown, when ready, or when the settings are incomplete', () => {
    expect(hpcRunEnvNote(null)).toBeNull()
    expect(hpcRunEnvNote(env({ installed: true }))).toBeNull()
    expect(hpcRunEnvNote(env({ configured: false, error: 'Set the cluster host first' }))).toBeNull()
  })

  it('warns and offers Install when this version has no env on the cluster', () => {
    const n = hpcRunEnvNote(env())!
    expect(n.short).toBe('Cluster env out of date — reinstall')
    expect(n.severity).toBe('warn')
    expect(n.canInstall).toBe(true)
    expect(n.detail).toBe('Not installed for this version')
  })

  it('an install in progress is info, not a second Install', () => {
    for (const e of [env({ installing: true }), env({ job: '4242' })]) {
      const n = hpcRunEnvNote(e)!
      expect(n.severity).toBeUndefined()
      expect(n.canInstall).toBe(false)
    }
  })

  it('a failed check warns without offering Install', () => {
    const n = hpcRunEnvNote(env({ error: 'Not reachable' }))!
    expect(n).toMatchObject({ short: 'Cluster env not checked', detail: 'Not reachable', severity: 'warn', canInstall: false })
  })
})
