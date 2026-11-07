import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useTaskDefsStore, summarizeTaskDefs } from './taskDefs'
import type { TaskDef } from '../tasks/types'

// The store fetches the definitions once per session. Param options (`optionsFrom`, e.g. the
// cellpose model list) change mid-session, so the store must not hand out a stale copy of them:
// it keeps labels/identity only, and params/defaults are read through `useTaskDefs` (refetches).
const defs = {
  segment: [{
    fun_name: 'segment.cellpose', task: 'cellposeSegment', label: 'Cellpose', category: '', env: [],
    params: [{ key: 'model', type: 'select', default: 'cyto3', options: ['cyto3', 'myModel'] }],
  }],
} as unknown as Record<string, TaskDef[]>

describe('taskDefs store holds no params', () => {
  beforeEach(() => setActivePinia(createPinia()))
  afterEach(() => vi.unstubAllGlobals())

  it('summarizeTaskDefs strips params and stamps the category', () => {
    const d = summarizeTaskDefs(defs).get('segment.cellpose') as Record<string, unknown>
    expect(d.label).toBe('Cellpose')
    expect(d.category).toBe('segment')
    expect('params' in d).toBe(false)
  })

  it('a caller reading param options or defaults from the once-fetched store gets nothing', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => ({ ok: true, json: async () => defs })))
    const s = useTaskDefsStore()
    await s.ensureLoaded()
    expect(s.labelFor('segment.cellpose')).toBe('Cellpose')
    expect((s.byFn.get('segment.cellpose') as Record<string, unknown>).params).toBeUndefined()
    expect((s.all()[0] as Record<string, unknown>).params).toBeUndefined()
    expect('defaultParamsFor' in s).toBe(false)
  })
})
