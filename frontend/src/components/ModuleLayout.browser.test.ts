// ModuleLayout's result row in a narrow panel: the page's actions (Manage images: Add images, Import
// from…, Copy, Move, Delete) wrap as ONE unit, like the Checks box in the set row. Each button used to
// be its own flex item, so the row broke between them — Add images beside the result line, the rest
// orphaned on the line below.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import { createMemoryHistory, createRouter } from 'vue-router'
import ModuleLayout from './ModuleLayout.vue'
import { useProjectStore } from '../stores/project'
import { useTaskDefsStore, type TaskDefSummary } from '../stores/taskDefs'
import { frames, mountInto } from '../test/browser/mount'
import { APPLY_WAIT } from '../utils/attrFilter'

const LABELS = ['Add images', 'Import from…', 'Copy', 'Move', 'Delete']
const rowsOf = (els: Element[]) => new Set(els.map(e => Math.round(e.getBoundingClientRect().top))).size

function mountAt(width: number) {
  // the result line links to /tasks (RouterLink) and ImageTable reads the route — a bare router is enough
  const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/:p(.*)*', component: { render: () => null } }] })
  const host = mountInto({
    setup() {
      const project = useProjectStore()
      // one image with a finished import run → the result line ("Convert to OME-ZARR finished…") shares
      // the row with the actions, which is what made them split
      project.sets = [{ uid: 'set1', name: 'T cell types', images: [{ uid: 'img1', name: 'img1', status: 'done',
        runLog: [{ fun: 'importImages.omezarr', status: 'done', at: '2026-11-10T10:00:00' }] }] }]
      project.activeSetUid = 'set1'
      // already loaded → no /api/tasks/definitions fetch (the setup file fails any network call)
      useTaskDefsStore().byFn = new Map([['importImages.omezarr', { label: 'Convert to OME-ZARR' } as TaskDefSummary]])
      return () => h('div', { style: `width:${width}px;height:600px;display:flex` }, [
        h(ModuleLayout, { module: 'manageImages', showFilter: false }, {
          actions: () => LABELS.map(l => h('button', { class: 'cc-btn' }, l)),
        }),
      ])
    },
  }, app => app.use(router))
  const buttons = () => [...host.querySelectorAll<HTMLElement>('.result-row button.cc-btn')]
  return { host, buttons }
}

describe('ModuleLayout result row', () => {
  it('the actions stay on one line together when the row is too narrow for everything', async () => {
    const { host, buttons } = mountAt(560)
    await frames()
    expect(host.querySelector('.result-line')?.textContent).toContain('Convert to OME-ZARR')
    const btns = buttons()
    expect(btns.map(b => b.textContent?.trim())).toEqual(LABELS)
    expect(rowsOf(btns)).toBe(1)
  })
})

// The Filter pane: a search box (uid or name) and attribute chips, both filtering LIVE once the
// typing / clicking pauses (APPLY_WAIT) — no Apply button.
describe('ModuleLayout filter pane', () => {
  const IMGS = [
    { uid: 'aaa111', name: 'LN_ctrl_1', attr: { Treatment: 'control' } },
    { uid: 'bbb222', name: 'LN_mertk_1', attr: { Treatment: 'MERTK' } },
    { uid: 'ccc333', name: 'spleen_ctrl', attr: { Treatment: 'control' } },
  ]
  function mountPane() {
    // the pane open without clicking its toggle (a click would persist → PATCH the profile)
    localStorage.setItem('cc-filters-open:filterTest', '1')
    const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/:p(.*)*', component: { render: () => null } }] })
    const host = mountInto({
      setup() {
        const project = useProjectStore()
        project.sets = [{ uid: 'set1', name: 'S', images: IMGS.map(i => ({ ...i, status: 'done' as const })) }]
        project.activeSetUid = 'set1'
        // already loaded → no /api/tasks/definitions fetch
        useTaskDefsStore().byFn = new Map([['importImages.omezarr', { label: 'Convert' } as TaskDefSummary]])
        return () => h('div', { style: 'width:900px;height:600px;display:flex' }, [
          h(ModuleLayout, { module: 'filterTest' })])
      },
    }, app => app.use(router))
    return host
  }
  const names = (host: HTMLElement) => [...host.querySelectorAll('tbody tr[data-sel-id]')].map(r => r.textContent ?? '')
  const settle = async () => { await new Promise(r => setTimeout(r, APPLY_WAIT + 50)); await frames() }
  const chip = (host: HTMLElement, v: string) =>
    [...host.querySelectorAll<HTMLElement>('.cc-filter-chips button')].find(b => b.textContent?.trim() === v)!

  it('the search box narrows the table by name or uid, case-insensitively', async () => {
    const host = mountPane()
    await frames()
    const input = host.querySelector<HTMLInputElement>('.image-search input')!
    input.value = 'CTRL'; input.dispatchEvent(new Event('input'))
    await settle()
    expect(names(host)).toHaveLength(2)
    input.value = 'bbb2'; input.dispatchEvent(new Event('input'))
    await settle()
    expect(names(host)).toHaveLength(1)
    expect(names(host)[0]).toContain('LN_mertk_1')
  })

  it('picking a chip filters without an Apply button', async () => {
    const host = mountPane()
    await frames()
    expect([...host.querySelectorAll('button')].some(b => b.textContent?.trim() === 'Apply')).toBe(false)
    chip(host, 'MERTK').click()
    await settle()
    expect(names(host)).toHaveLength(1)
    chip(host, 'control').click()          // a second value of the same key widens (ANY within a key)
    await settle()
    expect(names(host)).toHaveLength(3)
  })
})
