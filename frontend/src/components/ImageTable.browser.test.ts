// The image table renders the rows near the viewport, not the whole set (SelectionTable `windowed`).
// With every row in the DOM, a 2000-image set cost ~80 ms a frame for ANY change on the page — a
// keystroke in an unrelated input — and seconds to bring the rows back after a filter, though no Vue
// component re-rendered: the browser re-laid and re-painted 2000 rows. What must survive the window:
// the scroll height (spacers stand in for the rows not rendered), every row reachable by scrolling,
// the pinned header, and a shift-click range that spans rows which were never rendered.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import { createMemoryHistory, createRouter } from 'vue-router'
import ImageTable from './ImageTable.vue'
import { useProjectStore } from '../stores/project'
import { useTaskDefsStore, type TaskDefSummary } from '../stores/taskDefs'
import { frames, mountInto, scrollTo } from '../test/browser/mount'

const N = 2000
const uidOf = (i: number) => `u${String(i).padStart(5, '0')}`

function mountTable() {
  const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/:p(.*)*', component: { render: () => null } }] })
  let project!: ReturnType<typeof useProjectStore>
  const host = mountInto({
    setup() {
      project = useProjectStore()
      project.sets = [{ uid: 'set1', name: 'big', images: Array.from({ length: N }, (_, i) => ({
        uid: uidOf(i), name: `img_${i}`, status: 'done' as const, sizeC: 2 })) }]
      project.activeSetUid = 'set1'
      // already loaded → no /api/tasks/definitions fetch (the setup file fails any network call)
      useTaskDefsStore().byFn = new Map([['importImages.omezarr', { label: 'Convert' } as TaskDefSummary]])
      return () => h('div', { class: 'box', style: 'width:900px;height:600px;overflow-y:auto' }, [
        h(ImageTable, { setUid: 'set1' })])
    },
  }, app => app.use(router))
  const box = host.querySelector<HTMLElement>('.box')!
  const rows = () => [...host.querySelectorAll<HTMLElement>('tbody tr[data-sel-id]')]
  return { host, box, rows, selection: () => project.getImageSelection('default', 'set1') }
}

describe('ImageTable on a large set', () => {
  it('renders a screenful of rows, and the spacers keep the full scroll height', async () => {
    const { box, rows } = mountTable()
    await frames(4)
    const shown = rows()
    expect(shown.length).toBeGreaterThan(5)                 // the box is filled…
    expect(shown.length).toBeLessThan(100)                  // …but not with all 2000
    const rowH = shown[0].getBoundingClientRect().height
    expect(box.scrollHeight).toBeGreaterThan(N * rowH * 0.95)
    // the last rendered row reaches past the bottom of the box: no blank strip under the rows
    expect(shown.at(-1)!.getBoundingClientRect().bottom).toBeGreaterThan(box.getBoundingClientRect().bottom)
  })

  it('scrolling brings the rows there into the DOM, under the pinned header', async () => {
    const { host, box, rows } = mountTable()
    await frames(4)
    await scrollTo(box, box.scrollHeight / 2)
    await frames(2)
    const b = box.getBoundingClientRect()
    const inView = rows().filter(r => { const q = r.getBoundingClientRect(); return q.bottom > b.top && q.top < b.bottom })
    expect(inView.length).toBeGreaterThan(5)
    const mid = Number(inView[0].dataset.selId!.slice(1))
    expect(mid).toBeGreaterThan(N * 0.4)
    expect(mid).toBeLessThan(N * 0.6)
    // the header is at the top of the box, not scrolled away with the rows
    const head = host.querySelector('thead')!.getBoundingClientRect()
    expect(Math.abs(head.top - b.top)).toBeLessThan(2)
    await scrollTo(box, box.scrollHeight)
    await frames(2)
    expect(rows().at(-1)!.dataset.selId).toBe(uidOf(N - 1))
  })

  it('a shift-click range covers the rows that were never rendered', async () => {
    const { box, rows, selection } = mountTable()
    await frames(4)
    rows()[0].click()
    expect(selection()).toEqual([uidOf(0)])
    await scrollTo(box, box.scrollHeight)
    await frames(2)
    rows().at(-1)!.dispatchEvent(new MouseEvent('click', { bubbles: true, shiftKey: true }))
    await frames(1)
    expect(selection().length).toBe(N)
  })
})
