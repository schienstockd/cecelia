import { describe, expect, it } from 'vitest'
import { discoveryTooltipHtml, whichStepGroups } from './taskDiscovery'
import type { TaskDef } from '../tasks/types'

const def = (fun_name: string, extra: Partial<TaskDef> = {}): TaskDef => ({
  fun_name, task: fun_name.split('.')[1] + 'Task', label: fun_name.split('.')[1], category: '',
  env: [], params: [], ...extra,
})

describe('discoveryTooltipHtml', () => {
  it('lists use-when then not-when, escaped', () => {
    const html = discoveryTooltipHtml({ useWhen: ['Photon-limited <dim> data'], notWhen: ['Saturated'] })
    expect(html.indexOf('Use when')).toBeLessThan(html.indexOf('Not when'))
    expect(html).toContain('Photon-limited &lt;dim&gt; data')
    expect(html).not.toContain('<dim>')
  })
  it('omits an empty list, and is empty with neither', () => {
    expect(discoveryTooltipHtml({ useWhen: ['A'], notWhen: [] })).not.toContain('Not when')
    expect(discoveryTooltipHtml({})).toBe('')
  })
})

describe('whichStepGroups', () => {
  const defs = [
    def('segment.cellpose', { purpose: 'Find cells', useWhen: ['Outlines'], label: 'Cellpose' }),
    def('cleanupImages.denoise', { purpose: 'Remove noise', notWhen: ['Bright'] }),
    def('cleanupImages.afCorrect', { purpose: 'Remove AF', label: 'AF correction' }),
    def('importImages.remove', { purpose: 'x', hidden: true }),
    def('segment.noPurpose'),
    def('someCustom.task', { purpose: 'A custom module' }),
  ]
  const groups = whichStepGroups(defs)

  it('groups by page in sidebar order, labels sorted', () => {
    expect(groups.map(g => g.page)).toEqual(['Cleanup', 'Segment'])
    expect(groups[0].tasks.map(t => t.label)).toEqual(['AF correction', 'denoise'])
  })
  it('leaves out hidden tasks, tasks without a purpose and pages it cannot route to', () => {
    const funs = groups.flatMap(g => g.tasks.map(t => t.funName))
    expect(funs).not.toContain('importImages.remove')
    expect(funs).not.toContain('segment.noPurpose')
    expect(funs).not.toContain('someCustom.task')
  })
  it('carries what pre-selects the task on its page: the def.task key', () => {
    const t = groups[1].tasks[0]
    expect([t.path, t.module, t.task]).toEqual(['/segment', 'segment', 'cellposeTask'])
    expect(groups[0].tasks[1].useWhen).toEqual([])
  })
})
