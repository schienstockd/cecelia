// SeriesPicker's "All populations" row in a real browser: one click plots every listed population,
// the next clears them, and another family's picks are left alone. The selection maths is
// `toggleAllSelection` (utils/tableSelection.ts); this pins the wiring — row shown, one `reorder` emitted.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import { userEvent } from 'vitest/browser'
import SeriesPicker from './SeriesPicker.vue'
import { frames, mountInto } from '../../test/browser/mount'
import { tkey } from '../../plots/series'

const pop = (path: string) => ({ path, name: path.slice(1), colour: '#888', popType: 'region', uid: '' })
const groups = [{ valueName: 'A', populations: [pop('/r1'), pop('/r2')] },
                { valueName: 'B', populations: [pop('/r1')] }]
const listed = [tkey('region', 'A', '/r1'), tkey('region', 'A', '/r2'), tkey('region', 'B', '/r1')]

function mount(initial: string[], extra: Record<string, unknown> = {}) {
  const sel = ref<string[]>(initial)
  const host = mountInto({ render: () => h(SeriesPicker, { groups, selected: sel.value, scope: 'global', docked: true,
    ...extra, onReorder: (k: string[]) => (sel.value = k) }) })
  return { host, sel }
}

describe('SeriesPicker All row', () => {
  it('plots every listed population, then clears them; other families stay picked', async () => {
    const other = tkey('flow', 'A', '/T')
    const { host, sel } = mount([other])
    await frames()
    const eye = host.querySelector<HTMLElement>('.pick-allrow .pick-eye')!
    await userEvent.click(eye)
    expect(sel.value).toEqual([other, ...listed])
    await userEvent.click(eye)
    expect(sel.value).toEqual([other])
  })

  it('is absent on a one-at-a-time plot', async () => {
    const { host } = mount([], { single: true })
    await frames()
    expect(host.querySelector('.pick-allrow')).toBeNull()
  })
})
