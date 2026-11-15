// RESULT-REVIEW: result-93842d20
// Chain from history's default pick: versions of a task on DIFFERENT populations are not alternatives.
// An unread HMM on the T cell tracks and an unread HMM on the B cell tracks are both kept; within one
// population's sweep the newest version nobody moved past is the pick.
import { describe, it, expect } from 'vitest'
import { groupSteps, defaultKeep, type HistoryFinal } from './chainFromHistory'

const hmm = (id: string, at: string, detail: string, inputs: string, siblingRead = false): HistoryFinal =>
  ({ id, fn: 'behaviour.hmm', at, detail, inputs, inputsKey: `pops=${inputs}`, siblingRead, multi: false, output: '', imageUids: [], kept: true })

describe('defaultKeep across populations', () => {
  it('keeps one version per population, not one per task', () => {
    const finals = [hmm('hT', '2026-11-15T14:20', 'tcells', 'Tcells/_tracked'),
                    hmm('hB', '2026-11-15T14:30', 'bcells', 'Bcells/_tracked'),
                    hmm('m2', '2026-11-15T14:10', 'movement_2', 'Tcells/_tracked', true)]
    expect(defaultKeep(groupSteps(finals)).sort()).toEqual(['hB', 'hT'])
  })
  it('names the populations when one task has several groups', () => {
    const g = groupSteps([hmm('hT', '2026-11-15T14:20', 'tcells', 'Tcells/_tracked'),
                          hmm('hB', '2026-11-15T14:30', 'bcells', 'Bcells/_tracked')], () => 'HMM')
    expect(g.map(x => x.label).sort()).toEqual(['HMM · Bcells/_tracked', 'HMM · Tcells/_tracked'])
    expect(new Set(g.map(x => x.key)).size).toBe(2)
  })
})
