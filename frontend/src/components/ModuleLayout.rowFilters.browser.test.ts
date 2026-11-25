// A row filter saved ON whose rows have since gone: its button must stay drawn (else nothing can switch
// it off), and a table the filters emptied must say so and offer Show all — not the first-run import state.
import { afterEach, describe, expect, it } from 'vitest'
import { h } from 'vue'
import { createMemoryHistory, createRouter } from 'vue-router'
import ModuleLayout from './ModuleLayout.vue'
import { useProjectStore } from '../stores/project'
import { useTaskDefsStore, type TaskDefSummary } from '../stores/taskDefs'
import { rowFilterKey } from '../utils/rowFilters'
import { frames, mountInto } from '../test/browser/mount'

const KEY = rowFilterKey('unstarred', 'gating')

function mount() {
  const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/:p(.*)*', component: { render: () => null } }] })
  return mountInto({
    setup() {
      const project = useProjectStore()
      // three imported images, none rated — the stars were all removed after the filter was saved
      project.sets = [{ uid: 'set1', name: 'Set', images: ['a', 'b', 'c'].map(uid =>
        ({ uid, name: `img ${uid}`, status: 'done', filepaths: { default: `${uid}.ome.zarr` } })) }]
      project.activeSetUid = 'set1'
      // already loaded → no /api/tasks/definitions fetch (the setup file fails any network call)
      useTaskDefsStore().byFn = new Map([['gate.x', { label: 'x' } as TaskDefSummary]])
      return () => h('div', { style: 'width:900px;height:600px;display:flex' }, [
        h(ModuleLayout, { module: 'gating', showFilter: true }),
      ])
    },
  }, app => app.use(router))
}

// Seeded straight into the localStorage mirror profileStorage reads from — a profileStorage write would
// PATCH the profile bag, and the setup file fails any network call.
afterEach(() => localStorage.removeItem(KEY))

/** Accept the profile-bag PATCH that switching the filter off sends (debounced), recording what it saved. */
function stubProfilePatch(): Record<string, unknown>[] {
  const sent: Record<string, unknown>[] = []
  const guarded = window.fetch
  window.fetch = (input: RequestInfo | URL, init?: RequestInit) => {
    const url = typeof input === 'string' ? input : input instanceof URL ? input.href : input.url
    if (!url.endsWith('/api/profile/settings/patch')) return guarded(input, init)
    sent.push(JSON.parse(String(init?.body)))
    return Promise.resolve(new Response(JSON.stringify({ profile: 'p', settings: {} })))
  }
  return sent
}

describe('ModuleLayout — a saved row filter whose rows have gone', () => {
  it('keeps the ON filter button and says the images are filtered out, with a way back', async () => {
    localStorage.setItem(KEY, 'good')
    const sent = stubProfilePatch()
    const host = mount()
    await frames()

    const scored = [...host.querySelectorAll<HTMLElement>('.filter-toggle')]
      .find(b => b.textContent?.includes('Scored'))
    expect(scored, 'the ON Scored filter must stay drawn').toBeTruthy()
    expect(scored!.classList.contains('active')).toBe(true)

    expect(host.textContent).not.toContain('No images yet')
    const empty = host.querySelector<HTMLElement>('.filtered-empty')
    expect(empty?.textContent).toContain('All 3 images hidden by filters')

    empty!.querySelector<HTMLButtonElement>('button')!.click()
    await frames()
    expect(host.querySelector('.filtered-empty')).toBeNull()
    expect(host.textContent).toContain('img a')
    expect(localStorage.getItem(KEY)).toBe('0')
    await expect.poll(() => sent.some(p => p[`ls:${KEY}`] === '0'), { timeout: 2000 }).toBe(true)
    // off again, the Scored button has nothing to offer and goes away as before
    expect([...host.querySelectorAll('.filter-toggle')].some(b => b.textContent?.includes('Scored'))).toBe(false)
  })
})
