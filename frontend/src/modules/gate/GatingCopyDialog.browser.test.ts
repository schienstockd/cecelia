// The copy-gating dialog's target list: each image's uid beside its name, a GATED chip on the targets that
// already have this gating (the copy replaces it), and the footer's count of how many picked ones that is.
// The count's wording is in utils/gatingCopyWarnings.test.ts; this checks the dialog shows it.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { h } from 'vue'
import GatingCopyDialog from './GatingCopyDialog.vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { frames, mountInto } from '../../test/browser/mount'

const IMAGES = [{ uid: 'SRC111', name: 'source' }, { uid: 'AAA111', name: 'fresh' },
                { uid: 'BBB222', name: 'already' }, { uid: 'CCC333', name: 'noseg' }]

function mount(fail = false) {
  const bodies: Record<string, unknown>[] = []
  vi.stubGlobal('fetch', vi.fn(async (url: string, init?: RequestInit) => {
    expect(String(url)).toContain('/api/images/value-name-check')
    bodies.push(JSON.parse(String(init?.body)))
    if (fail) return new Response('{}', { status: 500 })
    return new Response(JSON.stringify({ available: ['AAA111', 'BBB222'], missing: ['CCC333'], gated: ['BBB222'] }),
                        { headers: { 'Content-Type': 'application/json' } })
  }))
  const host = mountInto({
    setup() {
      useProjectStore().sets = [{ uid: 'set1', name: 'set', images: IMAGES.map(i => ({ ...i, status: 'done' })) }] as never
      useProjectMetaStore().current = { uid: 'proj1', name: 'p' } as never
      return () => h(GatingCopyDialog, { setUid: 'set1', sourceUid: 'SRC111', valueName: 'B', popType: 'flow' })
    },
  })
  return { host, bodies }
}

async function settle() {
  for (let i = 0; i < 80 && !document.querySelector('.cg-item'); i++) await new Promise(r => setTimeout(r, 25))
  await frames()
}

afterEach(() => vi.unstubAllGlobals())

describe('GatingCopyDialog', () => {
  it('shows uids, marks already-gated targets, and counts the picked ones that will be overwritten', async () => {
    const { bodies } = mount()
    await settle()
    expect(bodies[0]).toMatchObject({ projectUid: 'proj1', valueName: 'B', popType: 'flow' })
    const rows = [...document.querySelectorAll<HTMLElement>('.cg-item')]
    expect(rows.map(r => r.querySelector('.cg-name')?.textContent)).toEqual(['fresh', 'already'])
    expect(rows.map(r => r.querySelector('.cc-uid')?.textContent)).toEqual(['AAA111', 'BBB222'])
    expect(rows.map(r => !!r.querySelector('.cg-gated'))).toEqual([false, true])
    // the uid is muted: dimmer than the name
    expect(getComputedStyle(rows[0].querySelector('.cc-uid')!).color)
      .not.toBe(getComputedStyle(rows[0].querySelector('.cg-name')!).color)

    expect(document.querySelector('.cg-overwrite')).toBeNull()       // nothing picked yet
    rows[0].querySelector<HTMLInputElement>('input')!.click()        // the ungated one: still no note
    await frames()
    expect(document.querySelector('.cg-overwrite')).toBeNull()
    rows[1].querySelector<HTMLInputElement>('input')!.click()
    await frames()
    expect(document.querySelector('.cg-overwrite')?.textContent?.trim()).toBe('1 of 2 selected will be overwritten')
  })

  it('a failed check still warns once something is picked', async () => {
    mount(true)
    await settle()
    expect(document.querySelectorAll('.cg-gated').length).toBe(0)
    document.querySelector<HTMLInputElement>('.cg-item input')!.click()
    await frames()
    expect(document.querySelector('.cg-overwrite')?.textContent?.trim()).toBe('Overwrites any existing gating')
  })
})
