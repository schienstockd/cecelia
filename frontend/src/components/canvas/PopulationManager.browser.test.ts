// The population manager's multi-selection in a real browser: ctrl/shift-click pick several rows (the
// same `fileManagerPick` as SelectionTable), the ⋯ menu of a selected row then acts on all of them in
// ONE store call, and the single-only actions stay listed but disabled. The pure parts (targets,
// counts, which action is multi) are `utils/popSelection.test.ts`; this pins the wiring.
import { describe, expect, it, vi } from 'vitest'
import { defineComponent, h } from 'vue'
import { userEvent } from 'vitest/browser'
import PopulationManager from './PopulationManager.vue'
import { provideGatingStore, type GatingStore, type PopNode } from '../../stores/gating'
import { frames, mountInto } from '../../test/browser/mount'

const node = (name: string, children: PopNode[] = []): PopNode =>
  ({ name, colour: '#888', show: true, uid: name, children,
     gate: { kind: 'rectangle', x_channel: 'a', y_channel: 'b', x_min: 0, x_max: 1, y_min: 0, y_max: 1 } as never })

function mount() {
  let g!: GatingStore
  const picked: string[] = []
  const host = mountInto(defineComponent({
    setup() {
      g = provideGatingStore('flow')
      g.tree = { value_name: 'default', pop_type: 'flow',
                 populations: [node('qc'), node('A', [node('A1')]), node('B'), node('C')] }
      return () => h(PopulationManager, { selected: '', highlighted: [], scope: 'global', lineWidth: 1,
        gateLabels: true, axisFromZero: true, docked: true, 'onUpdate:selected': (p: string) => picked.push(p) })
    },
  }))
  const movePops = vi.fn(async () => true), deletePops = vi.fn(async () => true)
  const movePop = vi.fn(async () => true), deletePop = vi.fn(async () => true)
  Object.assign(g, { movePops, deletePops, movePop, deletePop })
  return { host, picked, movePops, deletePops, movePop, deletePop }
}

const rows = (host: HTMLElement) => [...host.querySelectorAll<HTMLElement>('.pm-row:not(.pm-allrow)')]
const row = (host: HTMLElement, name: string) => rows(host).find(r => r.querySelector('.pm-name')?.textContent === name)!
const pickedNames = (host: HTMLElement) =>
  rows(host).filter(r => r.classList.contains('picked')).map(r => r.querySelector('.pm-name')!.textContent)
const menuItem = (label: RegExp) =>
  [...document.querySelectorAll<HTMLElement>('.pm-actions .cc-actions-item')].find(b => label.test(b.textContent ?? ''))!
async function openMenu(host: HTMLElement, name: string) {
  await userEvent.click(row(host, name).querySelector<HTMLElement>('.pm-actions-btn')!)
  await frames()
}

describe('PopulationManager multi-selection', () => {
  it('ctrl-click adds to the plain-clicked row, shift-click takes the range; neither moves the plot', async () => {
    const { host, picked } = mount()
    await frames()
    await userEvent.click(row(host, 'A'))
    expect(picked).toEqual(['/A'])                                     // a plain click still picks
    await userEvent.click(row(host, 'C'), { modifiers: ['Control'] } as never)
    await frames()
    expect(pickedNames(host)).toEqual(['A', 'C'])
    expect(host.querySelector('.pm-multi')?.textContent).toContain('2 selected')
    await userEvent.keyboard('{Shift>}')
    await userEvent.click(row(host, 'B'))
    await userEvent.keyboard('{/Shift}')
    await frames()
    expect(pickedNames(host)).toEqual(['B', 'C'])                      // range from the anchor (C, clicked last)
    expect(picked).toEqual(['/A'])
  })

  it('the menu of a selected row deletes the selection in one call; single-only items are disabled', async () => {
    const { host, deletePops, deletePop } = mount()
    await frames()
    await userEvent.click(row(host, 'A'))
    await userEvent.click(row(host, 'B'), { modifiers: ['Control'] } as never)
    await userEvent.click(row(host, 'A1'), { modifiers: ['Control'] } as never)
    await openMenu(host, 'B')
    expect(document.querySelector('.pm-bulk-head')?.textContent).toContain('3 populations')
    expect(menuItem(/Rename/).getAttribute('aria-disabled')).toBe('true')
    expect(menuItem(/gate's plot/).getAttribute('aria-disabled')).toBe('true')
    expect(menuItem(/Move under/).getAttribute('aria-disabled')).toBeNull()
    const del = menuItem(/^\s*Delete 3 populations/)
    await userEvent.click(del); await frames()
    await userEvent.click(menuItem(/^\s*Delete 3 populations/)); await frames()
    expect(deletePops).toHaveBeenCalledWith(['/A', '/B'])            // A1 goes with A, not twice
    expect(deletePop).not.toHaveBeenCalled()
  })

  it('moves the selection in one call, and never offers a target inside it', async () => {
    const { host, movePops } = mount()
    await frames()
    await userEvent.click(row(host, 'A'))
    await userEvent.click(row(host, 'B'), { modifiers: ['Control'] } as never)
    await openMenu(host, 'A')
    await userEvent.click(menuItem(/Move under/)); await frames()
    const targets = [...document.querySelectorAll('.pm-move-target')].map(b => b.textContent?.trim())
    expect(targets).toEqual(['/qc', '/C'])       // not A, A1 or B (a cycle), not all cells (both are there)
    await userEvent.click([...document.querySelectorAll<HTMLElement>('.pm-move-target')].find(b => b.textContent?.trim() === '/qc')!)
    expect(movePops).toHaveBeenCalledWith(['/A', '/B'], '/qc')
  })

  it('the menu of a row OUTSIDE the selection acts on that row alone', async () => {
    const { host, deletePop, deletePops } = mount()
    await frames()
    await userEvent.click(row(host, 'A'))
    await userEvent.click(row(host, 'B'), { modifiers: ['Control'] } as never)
    await openMenu(host, 'C')
    expect(document.querySelector('.pm-bulk-head')).toBeNull()
    expect(menuItem(/Rename/).getAttribute('aria-disabled')).not.toBe('true')
    await userEvent.click(menuItem(/Delete population/)); await frames()
    await userEvent.click(menuItem(/Delete population/))
    expect(deletePop).toHaveBeenCalledWith('/C')
    expect(deletePops).not.toHaveBeenCalled()
  })
})
