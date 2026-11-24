// The naming bar for a drawn gate on a NARROW gating plot: one row, inside the plot, whatever the kind —
// "4 populations" used to wrap to a second line on a quadrant. A long name stays whole in the box
// (it scrolls in the input), never cut. docs/POPULATION.md → Gating plot — rendering & UX hacks.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import GateNameBar from './GateNameBar.vue'
import { frames, mountInto } from '../../test/browser/mount'

const PLOT = 200                         // px — narrower than the smallest plot a panel leaves (~220)

function bar(kind: string, extra: Record<string, unknown> = {}) {
  const name = ref('')
  const host = mountInto({ render: () => h('div', { style: `position:relative;width:${PLOT}px;height:${PLOT}px` }, [
    h(GateNameBar, { kind, icon: 'pi pi-stop', nameTip: 'Name', modelValue: name.value,
      'onUpdate:modelValue': (v: string) => { name.value = v }, ...extra })]) })
  return { host, name, root: host.querySelector('.gate-name') as HTMLElement }
}
// every control on the row sits on one line, and the bar stays inside the plot
function oneRowInside(host: HTMLElement, root: HTMLElement) {
  const row = root.querySelector('.gate-name-row')!
  const tops = [...row.children].map(c => Math.round(c.getBoundingClientRect().top + c.getBoundingClientRect().height / 2))
  expect(Math.max(...tops) - Math.min(...tops)).toBeLessThanOrEqual(2)
  for (const c of row.children) expect(getComputedStyle(c).whiteSpace === 'nowrap' || c.tagName === 'INPUT').toBe(true)
  for (const c of row.querySelectorAll('span')) expect(c.getBoundingClientRect().height).toBeLessThan(20)  // no 2-line label
  const plot = host.firstElementChild!.getBoundingClientRect(), b = root.getBoundingClientRect()
  expect(b.right).toBeLessThanOrEqual(plot.right)
}

describe('GateNameBar on a narrow plot', () => {
  it('a quadrant: icon, "4 populations", Add, × on one row', async () => {
    const { host, root } = bar('quadrant', { count: 4 }); await frames()
    expect(root.textContent).toContain('4 populations')
    oneRowInside(host, root)
  })
  it('a rectangle with a long name keeps one row, and the whole name', async () => {
    const { host, root, name } = bar('rectangle'); await frames()
    const input = root.querySelector('input')!
    input.value = 'CD45RA-Kat+ CD62L-hi+ effector memory'; input.dispatchEvent(new Event('input'))
    await frames()
    expect(name.value).toBe('CD45RA-Kat+ CD62L-hi+ effector memory')
    oneRowInside(host, root)
  })
  it('a bisector with its valley toggle fits too', async () => {
    const { host, root } = bar('bisector', { rule: false }); await frames()
    oneRowInside(host, root)
  })
})
