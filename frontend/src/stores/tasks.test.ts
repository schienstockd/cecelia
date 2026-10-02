import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useTaskStore } from './tasks'
import { useAppControlStore } from './appControl'

const base = {
  module: 'segment', label: 'Cellpose', imageUid: 'i1', imageName: 'A', status: 'queued' as const,
  taskName: 'cellpose', funName: 'segment.cellpose', params: {}, projectUid: 'p1',
}

describe('task store — who launched a row', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it("stamps this tab's own rows with the active profile, like the server does", () => {
    useAppControlStore().activeProfileName = 'alice'
    const tasks = useTaskStore()
    expect(tasks.add(base).by).toBe('alice')
    expect(tasks.addMany([base, base]).map(t => t.by)).toEqual(['alice', 'alice'])
  })

  it('takes a chain row\'s launcher from the frame — chain frames reach every tab', () => {
    useAppControlStore().activeProfileName = 'alice'
    const tasks = useTaskStore()
    const ev = { runId: 'r', nodeId: 'n', imageUid: 'i1', fn: 'segment.cellpose', status: 'queued' as const,
                 projectUid: 'p1' }
    expect(tasks.addFromChainEvent({ ...ev, by: 'bob' }).by).toBe('bob')
    expect(tasks.addFromChainEvent({ ...ev, runId: 'r2' }).by).toBeUndefined()   // unknown, not 'alice'
    // a resume keeps the row (same runId::nodeId::imageUid) — the frame's resumer replaces the launcher
    expect(tasks.addFromChainEvent({ ...ev, by: 'carol' }).by).toBe('carol')
  })

  it('a re-run from this tab is this tab\'s launch', () => {
    useAppControlStore().activeProfileName = 'alice'
    const tasks = useTaskStore()
    const t = tasks.add({ ...base, by: 'bob' })
    tasks.restart(t.id)
    expect(tasks.tasks.find(x => x.id === t.id)?.by).toBe('alice')
  })

  it('keeps a launcher the caller already knows', () => {
    useAppControlStore().activeProfileName = 'alice'
    expect(useTaskStore().add({ ...base, by: 'bob' }).by).toBe('bob')
  })
})
