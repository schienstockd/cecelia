import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useTaskStore } from '../stores/tasks'
import { runSweepInSequence } from './sweepSequence'

const row = {
  module: 'behaviour', label: 'HMM', imageUid: 'i1', imageName: 'A', status: 'queued' as const,
  taskName: 'hmm', funName: 'behaviour.hmm', params: {}, projectUid: 'p1',
}

// Each step adds `perStep` rows, as `TaskRunner.dispatch` does (one per image), and records that it was sent.
function steps(n: number, perStep = 1) {
  const store = useTaskStore()
  const sent: string[][] = []
  const fns = Array.from({ length: n }, () => () => {
    const ids = Array.from({ length: perStep }, () => store.add(row).id)
    sent.push(ids)
    return ids
  })
  return { store, sent, fns }
}

describe('runSweepInSequence', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it('sends the next run only when the previous one has finished', () => {
    const { store, sent, fns } = steps(3)
    runSweepInSequence(fns)
    expect(sent).toHaveLength(1)
    store.setStatus(sent[0][0], 'running')
    expect(sent).toHaveLength(1)
    store.setStatus(sent[0][0], 'done')
    expect(sent).toHaveLength(2)
    store.setStatus(sent[1][0], 'done')
    expect(sent).toHaveLength(3)
  })

  it('waits for every image of a run before the next', () => {
    const { store, sent, fns } = steps(2, 2)
    runSweepInSequence(fns)
    store.setStatus(sent[0][0], 'done')
    expect(sent).toHaveLength(1)
    store.setStatus(sent[0][1], 'failed')
    expect(sent).toHaveLength(2)
  })

  it('goes on after a failed run, stops after a cancelled one', () => {
    const { store, sent, fns } = steps(4)
    runSweepInSequence(fns)
    store.setStatus(sent[0][0], 'failed')
    expect(sent).toHaveLength(2)
    store.setStatus(sent[1][0], 'cancelled')
    expect(sent).toHaveLength(2)
    expect(store.tasks.find(t => t.id === sent[1][0])!.log.at(-1)).toMatch(/2 runs not started/)
  })

  it('treats a removed row as finished', () => {
    const { store, sent, fns } = steps(2)
    runSweepInSequence(fns)
    store.remove(sent[0][0])
    expect(sent).toHaveLength(2)
  })
})

describe('runSweepInSequence — a step that sent nothing', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it('stops the sweep rather than sending the rest at once', () => {
    let calls = 0
    runSweepInSequence([() => { calls++; return [] }, () => { calls++; return [] }, () => { calls++; return [] }])
    expect(calls).toBe(1)
  })
})
