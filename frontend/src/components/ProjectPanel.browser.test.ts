// The Project manager's default selection is a row the user can see: under the "Mine" scope, a newer
// project of another profile (first in the server's order) is hidden, so it must not be the row that
// "Open project" opens.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { h } from 'vue'
import ProjectPanel from './ProjectPanel.vue'
import { frames, mountInto } from '../test/browser/mount'

const rec = (uid: string, owners: string[]) =>
  ({ uid, name: `P ${uid}`, path: `/p/${uid}`, createdAt: '2026-11-01', lastOpenedAt: null, owners })

function stubApi() {
  vi.stubGlobal('fetch', vi.fn(async (url: string) => {
    const u = String(url)
    const body = u.includes('/api/kiwi/profiles')
      ? { active: 'alice', profiles: [{ name: 'alice' }, { name: 'bob' }], legacyReserved: [] }
      : u.includes('/api/projects')
        ? { projects: [rec('theirs', ['bob']), rec('mine', ['alice'])], projectsDir: '/p' }
        : {}
    return new Response(JSON.stringify(body), { headers: { 'Content-Type': 'application/json' } })
  }))
}

afterEach(() => vi.unstubAllGlobals())

describe('ProjectPanel default selection', () => {
  it('selects the first project in scope, not a hidden one', async () => {
    stubApi()
    mountInto({ render: () => h(ProjectPanel) })
    const pickedId = () => document.querySelector('tr[data-sel-id] input:checked')?.closest('tr')?.dataset.selId
    for (let i = 0; i < 80 && !document.querySelector('tr[data-sel-id]'); i++) await new Promise(r => setTimeout(r, 25))
    await frames(4)
    expect([...document.querySelectorAll<HTMLElement>('tr[data-sel-id]')].map(r => r.dataset.selId)).toEqual(['mine'])
    expect(pickedId()).toBe('mine')
  })
})
