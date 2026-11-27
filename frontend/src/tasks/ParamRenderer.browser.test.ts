// A collapsible section whose every entry is ruled out by `showIf` is not drawn at all — the neighbour
// graph's "Advanced" (only `perTimepoint`) under Fixed windows used to sit there as an empty heading.
// Real DOM, the real task spec.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import ParamRenderer from './ParamRenderer.vue'
import { frames, mountInto } from '../test/browser/mount'
import type { ParamDef, TaskDef } from './types'
import cellNeighbours from '../../../app/src/tasks/spatialAnalysis/cellNeighbours.json'

const advanced = (cellNeighbours as unknown as TaskDef).params.find(p => p.key === 'advanced') as ParamDef

async function render(param: ParamDef, values: Record<string, unknown>) {
  const host = mountInto({
    render: () => h(ParamRenderer, { param, modelValue: undefined, context: { images: [], values } }),
  })
  await frames()
  return host
}

describe('ParamRenderer — empty sections', () => {
  it('draws no Advanced heading under Fixed windows', async () => {
    const host = await render(advanced, { neighbourMethod: 'windows' })
    expect(host.querySelector('.section-toggle')).toBeNull()
  })

  it('draws it for Delaunay, where Per timepoint applies', async () => {
    const host = await render(advanced, { neighbourMethod: 'delaunay' })
    expect(host.querySelector('.section-toggle')?.textContent?.trim()).toBe('Advanced')
  })

  it('a section inside a group entry follows that entry’s own values', async () => {
    const group = {
      key: 'passes', label: 'Passes', type: 'group', params: [
        { key: 'mode', label: 'Mode', type: 'select', default: 'a',
          options: [{ value: 'a', label: 'A' }, { value: 'b', label: 'B' }] },
        { key: 'adv', label: 'More', type: 'section', params: [
          { key: 'k', label: 'K', type: 'int', default: 1, showIf: { mode: 'b' } },
        ] },
      ],
    } as unknown as ParamDef
    const host = mountInto({
      render: () => h(ParamRenderer, {
        param: group, modelValue: { 0: { mode: 'a' }, 1: { mode: 'b' } },
        context: { images: [], values: {} },
      }),
    })
    await frames()
    expect(host.querySelectorAll('.group-section-toggle').length).toBe(1)
  })
})
