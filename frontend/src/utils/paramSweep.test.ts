import { describe, it, expect } from 'vitest'
import type { ParamDef, TaskDef } from '../tasks/types'
import type { CciaImage } from '../stores/project'
import { parseSweepValues, sweepRunName, expandSweep, sweepTakenNames, sweepableParams, SWEEP_MAX_RUNS, parseSweepState,
         type SweepState } from './paramSweep'

const intP: ParamDef = { key: 'numStates', label: 'States', type: 'int', min: 2, max: 10 }
const floatP: ParamDef = { key: 'resolution', label: 'Resolution', type: 'float', min: 0.05, max: 5 }
const selP: ParamDef = { key: 'mode', label: 'Mode', type: 'select',
                         options: [{ label: 'A', value: 'a' }, { label: 'B', value: 'b' }] }

describe('parseSweepValues', () => {
  it('reads lists and ranges, in order, without duplicates', () => {
    expect(parseSweepValues('2, 3 5', intP).values).toEqual([2, 3, 5])
    expect(parseSweepValues('2:5', intP).values).toEqual([2, 3, 4, 5])
    expect(parseSweepValues('2:4, 3, 6', intP).values).toEqual([2, 3, 4, 6])
  })
  it('steps a float range without accumulating error', () => {
    expect(parseSweepValues('0.1:0.5:0.1', floatP).values).toEqual([0.1, 0.2, 0.3, 0.4, 0.5])
  })
  it('refuses values the param would not run as typed', () => {
    expect(parseSweepValues('1, 2', intP).error).toMatch(/below 2/)
    expect(parseSweepValues('2.5', intP).error).toMatch(/whole number/)
    expect(parseSweepValues('11', intP).error).toMatch(/above 10/)
    expect(parseSweepValues('x', intP).error).toMatch(/Can't read/)
    expect(parseSweepValues('5:2', intP).error).toMatch(/backwards/)
    expect(parseSweepValues('2:5:0', intP).error).toMatch(/Step/)
    expect(parseSweepValues('', intP).error).toBeTruthy()
  })
  it('caps the number of runs', () => {
    expect(parseSweepValues(`0.05:5:0.05`, floatP).error).toMatch(String(SWEEP_MAX_RUNS))
  })
  it('takes a select param by option value', () => {
    expect(parseSweepValues('a b', selP).values).toEqual(['a', 'b'])
    expect(parseSweepValues('c', selP).error).toMatch(/not an option/)
  })
})

describe('sweepRunName', () => {
  it('fills {name} and {v}', () => {
    expect(sweepRunName('{name}_{v}', 'hmm', 3)).toBe('hmm_3')
    expect(sweepRunName('k{v}', 'hmm', 3)).toBe('k3')
  })
})

const hmmDef = {
  fun_name: 'behaviour.hmm', task: 'hmm', label: 'HMM', category: 'behaviour', env: [],
  params: [{ key: 'colName', label: 'Name', type: 'text' }, intP,
           { key: 'filtering', label: 'Filtering', type: 'section', params: [floatP] }],
  sweep: { output: 'colName', params: ['numStates', 'resolution', 'missing'] },
} as TaskDef
const clustDef = {
  fun_name: 'clustPops.cluster', task: 'cluster', label: 'Cluster', category: 'clustPops', env: [],
  params: [{ key: 'valueNameSuffix', label: 'Suffix', type: 'valueNameInput', namespace: 'clusters' }, floatP],
  sweep: { output: 'valueNameSuffix', params: ['resolution'] },
} as unknown as TaskDef
const img = (o: Partial<CciaImage>) => ({ uid: 'i', filepaths: {}, ...o }) as CciaImage
const state = (o: Partial<SweepState>): SweepState => ({ on: true, param: 'numStates', values: '2:4', names: 'k{v}', ...o })

describe('sweepableParams', () => {
  it('finds params inside sections and drops keys the spec does not have', () => {
    expect(sweepableParams(hmmDef).map(p => p.key)).toEqual(['numStates', 'resolution'])
  })
})

describe('expandSweep', () => {
  it('varies the param and names each run from the template', () => {
    const plan = expandSweep(hmmDef, { colName: 'hmm', numStates: 3, x: 1 }, state({}), [])
    expect(plan.error).toBeUndefined()
    expect(plan.runs.map(r => [r.params.numStates, r.params.colName, r.params.x]))
      .toEqual([[2, 'k2', 1], [3, 'k3', 1], [4, 'k4', 1]])
    expect(plan.replaces).toBe(true)        // colName has no name list → an existing run is replaced
  })
  it('needs {v} so the runs do not share a name', () => {
    expect(expandSweep(hmmDef, { colName: 'hmm' }, state({ names: '{name}' }), []).error).toMatch(/\{v\}/)
  })
  it('refuses a name a selected image already has', () => {
    const imgs = [img({ clusterSuffixes: ['r1'] }), img({ clusterSuffixes: [] })]
    const plan = expandSweep(clustDef, { valueNameSuffix: 'x' },
                             state({ param: 'resolution', values: '0.5, 1', names: 'r{v}' }), imgs)
    expect(plan.error).toBe('Already exists: r1')
    expect(plan.runs).toEqual([])
    const ok = expandSweep(clustDef, { valueNameSuffix: 'x' },
                           state({ param: 'resolution', values: '0.5, 2', names: 'r{v}' }), imgs)
    expect(ok.runs.map(r => r.name)).toEqual(['r0.5', 'r2'])
    expect(ok.replaces).toBe(false)
  })
  it('refuses a param the spec does not offer', () => {
    expect(expandSweep(hmmDef, {}, state({ param: 'missing' }), []).error).toBeTruthy()
  })
})

describe('sweepTakenNames', () => {
  it('reads label sets, branch sets and the model vault', () => {
    const lab = { key: 'o', label: 'o', type: 'valueNameInput', namespace: 'labels' } as ParamDef
    expect([...sweepTakenNames(lab, [img({ labels: { a: ['a.zarr'] } })])!]).toEqual(['a'])
    const br = { ...lab, outputRegistry: 'branch_labels' } as ParamDef
    expect([...sweepTakenNames(br, [img({ branchLabels: { b: ['b'] } })])!]).toEqual(['b'])
    const models = { key: 'm', label: 'm', type: 'valueNameInput', namespace: 'models',
                     options: [{ label: 'flow1', value: 'flow1' }] } as unknown as ParamDef
    expect([...sweepTakenNames(models, [])!]).toEqual(['flow1'])
  })
})

describe('parseSweepState', () => {
  it('falls back to the first offered param and the default names', () => {
    expect(parseSweepState(null, hmmDef)).toEqual({ on: false, param: 'numStates', values: '', names: '{name}_{v}' })
    expect(parseSweepState('{"on":true,"param":"gone","values":"2:5","names":"k{v}"}', hmmDef))
      .toEqual({ on: true, param: 'numStates', values: '2:5', names: 'k{v}' })
    expect(parseSweepState('not json', hmmDef).param).toBe('numStates')
  })
  it('is never on for a task that offers no sweep', () => {
    expect(parseSweepState('{"on":true}', { ...clustDef, sweep: undefined }).on).toBe(false)
  })
})
