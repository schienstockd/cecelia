// RESULT-REVIEW: result-1aa53d61
// A sequential sweep sends runs 2…N minutes after Run. They read their images, set, project and pool from
// a target pinned at Run time (`pinSweepTarget`), so changing the selection meanwhile cannot send the rest
// of the sweep to other images.
import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useTaskStore } from '../stores/tasks'
import { pinSweepTarget, runSweepInSequence, type SweepTarget } from './sweepSequence'

describe('pinSweepTarget', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it('every run of a sequential sweep goes to the images selected at Run', () => {
    const store = useTaskStore()
    const selection = { uids: ['imgA'], names: ['A'] }
    const at = pinSweepTarget({ ...selection, projectUid: 'p1', setUid: 's1', pool: 'cpu', onHpc: false,
                                hpcFields: {} })
    const sentTo: string[][] = []
    const send = (t: SweepTarget) => () => {
      sentTo.push([...t.uids])
      return t.uids.map(u => store.add({ module: 'behaviour', label: 'HMM', imageUid: u, imageName: u,
        status: 'queued', taskName: 'hmm', funName: 'behaviour.hmm', params: {}, projectUid: t.projectUid }).id)
    }
    runSweepInSequence([send(at), send(at)])
    selection.uids.splice(0, 1, 'imgB')      // the user selects another image while run 1 is going
    store.setStatus(store.tasks[0].id, 'done')
    expect(sentTo).toEqual([['imgA'], ['imgA']])
  })
})
