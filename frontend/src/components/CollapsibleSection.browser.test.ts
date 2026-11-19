// CollapsibleSection in a real browser: where its bar sits while the page scrolls (`sticky`), what it
// tells a pinned header inside (`--cc-sticky-above`), the one-open accordion, and the heights `fill`
// and `maxHeight` give the body. docs/todo/GUI_TESTING_PLAN.md, P3a.
import { describe, expect, it } from 'vitest'
import { h, type VNode } from 'vue'
import { userEvent } from 'vitest/browser'
import CollapsibleSection from './CollapsibleSection.vue'
import { frames, mountInto, scrollTo } from '../test/browser/mount'

let n = 0
const tall = (px = 1200) => h('div', { class: 't-content', style: `height:${px}px` }, 'content')
const box = (children: VNode[], style = 'height:300px;width:500px;overflow-y:auto') =>
  h('div', { class: 't-box', style }, children)
const rect = (el: Element) => el.getBoundingClientRect()

describe('CollapsibleSection sticky', () => {
  it('the bar stays on the scroll box top while the body scrolls under it', async () => {
    const host = mountInto({ render: () => box([h(CollapsibleSection, { label: `S${n++}`, maxHeight: 'none', sticky: true }, () => tall())]) })
    const scroller = host.querySelector<HTMLElement>('.t-box')!
    const bar = host.querySelector<HTMLElement>('.cs-toggle')!
    await scrollTo(scroller, 500)
    expect(rect(bar).top).toBeCloseTo(rect(scroller).top + scroller.clientTop, 0)
    // …and is still the thing you click: the section collapses from there
    await userEvent.click(bar)
    expect(host.querySelector<HTMLElement>('.cs-body')!.style.display).toBe('none')
  })

  it("publishes the bar's height to its body, for a pinned header inside", async () => {
    const host = mountInto({ render: () => box([h(CollapsibleSection, { label: `S${n++}`, maxHeight: 'none', sticky: true }, () => tall())]) })
    await frames()
    const body = host.querySelector<HTMLElement>('.cs-body')!
    const above = parseFloat(getComputedStyle(body).getPropertyValue('--cc-sticky-above'))
    expect(above).toBeCloseTo(rect(host.querySelector('.cs-toggle')!).height, 0)
    expect(above).toBeGreaterThan(0)
  })

  it('without `sticky` the bar scrolls away with the page', async () => {
    const host = mountInto({ render: () => box([h(CollapsibleSection, { label: `S${n++}`, maxHeight: 'none' }, () => tall())]) })
    const scroller = host.querySelector<HTMLElement>('.t-box')!
    await scrollTo(scroller, 500)
    expect(rect(host.querySelector('.cs-toggle')!).bottom).toBeLessThan(rect(scroller).top)
  })
})

describe('CollapsibleSection heights', () => {
  it('maxHeight caps the body, which then scrolls itself', async () => {
    const host = mountInto({ render: () => box([h(CollapsibleSection, { label: `S${n++}`, maxHeight: '100px' }, () => tall(400))], 'width:500px') })
    await frames()
    const body = host.querySelector<HTMLElement>('.cs-body')!
    expect(rect(body).height).toBeCloseTo(100, 0)
    expect(body.scrollHeight).toBeGreaterThan(body.clientHeight)
  })

  it('fill: the open section takes the rest of a bounded flex column, the others keep their bars', async () => {
    const acc = `gui-test-fill-${n++}`
    const host = mountInto({
      render: () => h('div', { class: 't-col', style: 'height:400px;width:400px;display:flex;flex-direction:column' }, [
        h(CollapsibleSection, { label: 'One', accordion: acc, section: 'one', fill: true, defaultOpen: true }, () => tall(1000)),
        h(CollapsibleSection, { label: 'Two', accordion: acc, section: 'two', fill: true }, () => tall(1000)),
      ]),
    })
    await frames(3)
    const col = host.querySelector<HTMLElement>('.t-col')!
    const [one, two] = host.querySelectorAll<HTMLElement>('.collapsible-section')
    expect(rect(two).bottom).toBeCloseTo(rect(col).bottom, 0)                    // nothing overflows the column
    expect(rect(two).height).toBeCloseTo(rect(two.querySelector('.cs-toggle')!).height + 1, 0)  // closed: bar (+ its top border)
    const body = one.querySelector<HTMLElement>('.cs-body')!
    expect(body.scrollHeight).toBeGreaterThan(body.clientHeight)                // the open body scrolls inside
  })
})

describe('CollapsibleSection accordion', () => {
  it('opening one closes the others', async () => {
    const acc = `gui-test-acc-${n++}`
    const host = mountInto({
      render: () => h('div', [
        h(CollapsibleSection, { label: 'A', accordion: acc, section: 'a', defaultOpen: true }, () => 'a body'),
        h(CollapsibleSection, { label: 'B', accordion: acc, section: 'b' }, () => 'b body'),
        h(CollapsibleSection, { label: 'C', accordion: acc, section: 'c' }, () => 'c body'),
      ]),
    })
    await frames()
    const open = () => [...host.querySelectorAll<HTMLElement>('.cs-body')].map(b => b.style.display !== 'none')
    expect(open()).toEqual([true, false, false])
    await userEvent.click(host.querySelectorAll<HTMLElement>('.cs-toggle')[1])
    expect(open()).toEqual([false, true, false])
    await userEvent.click(host.querySelectorAll<HTMLElement>('.cs-toggle')[1])
    expect(open()).toEqual([false, false, false])
  })
})
