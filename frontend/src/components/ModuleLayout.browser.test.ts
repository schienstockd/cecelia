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
