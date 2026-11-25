// The Welcome page's project list shows the active profile's own projects plus shared ones (no
// owners) — never another profile's. Those are opened from the Project manager's "All" scope.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import { createMemoryHistory, createRouter } from 'vue-router'
import WelcomeModule from './WelcomeModule.vue'
import { useProjectMetaStore, type ProjectRecord } from '../stores/projectMeta'
import { useAppControlStore } from '../stores/appControl'
import { frames, mountInto } from '../test/browser/mount'

const rec = (uid: string, owners?: string[]): ProjectRecord =>
  ({ uid, name: `P ${uid}`, path: `/p/${uid}`, createdAt: '2026-11-01', lastOpenedAt: null, owners })
// Theirs first: the server orders by last open, so another profile's project can top the list.
const PROJECTS = [rec('theirs', ['bob']), rec('mine', ['alice']), rec('shared', []), rec('legacy'), rec('both', ['bob', 'alice'])]

async function mountAs(active: string, profileCount: number) {
  const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/:p(.*)*', component: { render: () => null } }] })
  const host = mountInto({
    setup() {
      // recent already loaded → no /api/projects fetch (the setup file fails any network call)
      useProjectMetaStore().recent = PROJECTS
      const ac = useAppControlStore()
      ac.activeProfileName = active
      ac.profileCount = profileCount
      return () => h(WelcomeModule)
    },
  }, app => app.use(router))
  await frames()
  const text = host.textContent ?? ''
  return { listed: PROJECTS.map(p => p.name).filter(n => text.includes(n)), text }
}

describe('WelcomeModule project list', () => {
  it('lists only own and shared projects for the active profile', async () => {
    const { listed, text } = await mountAs('alice', 2)
    expect(listed).toEqual(['P mine', 'P shared', 'P legacy', 'P both'])
    // the one-click button opens the first VISIBLE project, not another profile's
    expect(text).toContain('Open P mine')
  })

  it('lists every project on a single-profile install', async () => {
    const { listed } = await mountAs('alice', 1)
    expect(listed).toEqual(PROJECTS.map(p => p.name))
  })
})
