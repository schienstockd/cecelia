// ChipSelect and CcToggle with real clicks, keys and drags, and the layout each dialect promises.
// docs/todo/GUI_TESTING_PLAN.md, P3c.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import { userEvent } from 'vitest/browser'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import CcToggle from './CcToggle.vue'
import { frames, mountInto } from '../test/browser/mount'

const opts = (n: number) => Array.from({ length: n }, (_, i) => ({ value: `v${i}`, label: `Chip ${i}` }))

function chips(props: { options: ChipOption[] } & Record<string, unknown>, initial: string | string[] | null,
               style = 'width:600px') {
  const model = ref<string | string[] | null>(initial)
  const host = mountInto({
    render: () => h('div', { style }, [h(ChipSelect, { ...props, modelValue: model.value,
      'onUpdate:modelValue': (v: string | string[]) => { model.value = v } })]),
  })
  const chip = (label: string) => [...host.querySelectorAll<HTMLElement>('button.chip')]
    .find(b => b.textContent?.trim().startsWith(label))!
  return { host, model, chip }
}
const rowsOf = (els: Element[]) => new Set(els.map(e => Math.round(e.getBoundingClientRect().top))).size

describe('ChipSelect picking', () => {
  it('single: a click picks; with allowEmpty a second click clears', async () => {
    const { model, chip } = chips({ options: opts(4), allowEmpty: true }, '')
    await userEvent.click(chip('Chip 2'))
    expect(model.value).toBe('v2')
    await userEvent.click(chip('Chip 2'))
    expect(model.value).toBe('')
  })

  it('multiple keeps PICK order; All fills, and clears a full selection', async () => {
    const { model, chip } = chips({ options: opts(4), multiple: true, selectAll: true }, [])
    await userEvent.click(chip('Chip 3'))
    await userEvent.click(chip('Chip 1'))
    expect(model.value).toEqual(['v3', 'v1'])
    await userEvent.click(chip('All'))
    expect([...(model.value as string[])].sort()).toEqual(['v0', 'v1', 'v2', 'v3'])
    await userEvent.click(chip('All'))
    expect(model.value).toEqual([])
  })

  it('a chip is a button: focus it and press Space', async () => {
    const { model, chip } = chips({ options: opts(3) }, 'v0')
    chip('Chip 1').focus()
    await userEvent.keyboard(' ')
    expect(model.value).toBe('v1')
  })

  it('reorderable: dragging a picked chip onto another reorders the selection', async () => {
    const { model, chip } = chips({ options: opts(4), multiple: true, reorderable: true }, ['v0', 'v1', 'v2'])
    await frames()
    await userEvent.dragAndDrop(chip('Chip 2'), chip('Chip 0'))
    expect((model.value as string[])[0]).toBe('v2')
    expect([...(model.value as string[])].sort()).toEqual(['v0', 'v1', 'v2'])
  })
})

describe('ChipSelect layout', () => {
  it('grid: `columns` chips per row', async () => {
    // 3, not the CSS fallback of 4, so a lost `columns` shows
    const { host } = chips({ options: opts(9), variant: 'grid', columns: 3 }, 'v0')
    await frames()
    const tops = [...host.querySelectorAll('button.chip')].map(c => Math.round(c.getBoundingClientRect().top))
    expect(tops.filter(t => t === tops[0]).length).toBe(3)        // chips in the first row
    expect(rowsOf([...host.querySelectorAll('button.chip')])).toBe(3)
  })

  it('pill wraps in a narrow rail; segmented stays one row', async () => {
    const pill = chips({ options: opts(10) }, 'v0', 'width:220px')
    const seg = chips({ options: opts(4), variant: 'segmented' }, 'v0', 'width:600px')
    await frames()
    expect(rowsOf([...pill.host.querySelectorAll('button.chip')])).toBeGreaterThan(1)
    expect(rowsOf([...seg.host.querySelectorAll('button.chip')])).toBe(1)
    // wrapping never pushes a chip past the rail
    const rail = pill.host.firstElementChild!.getBoundingClientRect()
    for (const c of pill.host.querySelectorAll('button.chip')) expect(c.getBoundingClientRect().right).toBeLessThanOrEqual(rail.right + 1)
  })

  it('a long list collapses to the picked chips, "+N more" expands, the filter narrows', async () => {
    const { host, chip } = chips({ options: opts(30), multiple: true }, ['v5'])
    await frames()
    const shown = () => [...host.querySelectorAll<HTMLElement>('button.chip:not(.chip-all)')].map(b => b.textContent?.trim())
    expect(shown()).toEqual(['Chip 5'])
    await userEvent.click(chip('+'))
    expect(shown().length).toBe(30)
    // every word must appear somewhere (textQuery's rule), so 'chip 2' also keeps Chip 12 — but not Chip 13
    await userEvent.fill(host.querySelector<HTMLInputElement>('.chip-filter')!, 'chip 2')
    await frames()
    expect(shown()).toEqual(['Chip 2', 'Chip 12', ...Array.from({ length: 10 }, (_, i) => `Chip ${20 + i}`)])
  })
})

describe('CcToggle', () => {
  function toggle(refuse = false) {
    const on = ref(false)
    const host = mountInto({
      render: () => h(CcToggle, { modelValue: on.value, label: 'Autoplay',
        'onUpdate:modelValue': (v: boolean) => { if (!refuse) on.value = v } }),
    })
    return { on, host, input: () => host.querySelector<HTMLInputElement>('input')! }
  }

  it('clicking the label flips it', async () => {
    const { on, host } = toggle()
    await userEvent.click(host.querySelector('.cc-toggle-label')!)
    expect(on.value).toBe(true)
  })

  it('a refused change leaves the switch showing the model, not the click', async () => {
    const { on, host, input } = toggle(true)
    await userEvent.click(host.querySelector('.cc-toggle-label')!)
    await frames()
    expect([on.value, input().checked]).toEqual([false, false])
  })

  it('reachable from the keyboard: Tab focuses it, Space flips it', async () => {
    const { on, host } = toggle()
    host.insertAdjacentHTML('afterbegin', '<button class="t-before">before</button>')
    host.querySelector<HTMLElement>('.t-before')!.focus()
    await userEvent.keyboard('{Tab}')
    await userEvent.keyboard(' ')
    expect(on.value).toBe(true)
  })
})
