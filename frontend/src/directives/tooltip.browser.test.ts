// `v-tooltip` in a real browser: where the tip lands against its target and the viewport, and every way
// it goes away. docs/todo/GUI_TESTING_PLAN.md, P3c.
import { describe, expect, it } from 'vitest'
import { h, ref, withDirectives, resolveDirective } from 'vue'
import { userEvent } from 'vitest/browser'
import { frames, mountInto } from '../test/browser/mount'

const tip = () => document.querySelector<HTMLElement>('.cc-tooltip')
const rect = (el: Element) => el.getBoundingClientRect()

/** A button with `v-tooltip.<side>="text"`, placed by `style`. */
function target(text: { value: string }, side: string, style: string) {
  return mountInto({
    render() {
      const dir = resolveDirective('tooltip')!
      return h('div', [
        withDirectives(h('button', { class: 't-target', style }, 'target'), [[dir, text.value, undefined, { [side]: true }]]),
        h('button', { class: 't-other', style: 'position:fixed;left:600px;top:400px' }, 'other'),
      ])
    },
  })
}
const T = (host: HTMLElement) => host.querySelector<HTMLElement>('.t-target')!

describe('v-tooltip placement', () => {
  it('shows on hover on the requested side, linked for screen readers', async () => {
    const host = target(ref('Delete the set'), 'bottom', 'position:fixed;left:300px;top:200px')
    await userEvent.hover(T(host))
    expect(tip()!.textContent).toBe('Delete the set')
    expect(rect(tip()!).top).toBeGreaterThanOrEqual(rect(T(host)).bottom)
    expect(T(host).getAttribute('aria-describedby')).toBe(tip()!.id)
  })

  it('no room on the requested side: flips', async () => {
    const host = target(ref('tip'), 'bottom', `position:fixed;left:300px;top:${window.innerHeight - 24}px`)
    await userEvent.hover(T(host))
    expect(rect(tip()!).bottom).toBeLessThanOrEqual(rect(T(host)).top)
  })

  it('a long tip by the edge is clamped inside the viewport', async () => {
    const host = target(ref('a fairly long tooltip that would run off the right edge of the window'), 'right',
      `position:fixed;left:${window.innerWidth - 50}px;top:200px`)
    await userEvent.hover(T(host))
    const r = rect(tip()!)
    expect(r.right).toBeLessThanOrEqual(window.innerWidth)
    expect(r.left).toBeGreaterThanOrEqual(0)
  })
})

describe('v-tooltip hides', () => {
  const shown = async () => {
    const host = target(ref('tip'), 'bottom', 'position:fixed;left:300px;top:200px')
    await userEvent.hover(T(host))
    expect(tip()).not.toBeNull()
    return host
  }
  it('when the pointer leaves', async () => {
    const host = await shown()
    await userEvent.hover(host.querySelector('.t-other')!)
    expect(tip()).toBeNull()
  })
  it('on a click', async () => {
    const host = await shown()
    await userEvent.click(T(host))
    expect(tip()).toBeNull()
  })
  it('on Escape', async () => {
    await shown()
    await userEvent.keyboard('{Escape}')
    expect(tip()).toBeNull()
  })
  it('on any scroll', async () => {
    await shown()
    window.dispatchEvent(new Event('scroll'))
    expect(tip()).toBeNull()
  })
  it('when its target leaves the document under a still pointer', async () => {
    const host = await shown()
    T(host).remove()
    await userEvent.hover(host.querySelector('.t-other')!)
    expect(tip()).toBeNull()
  })
})

describe('v-tooltip text', () => {
  it('a visible tip follows a changed binding', async () => {
    const text = ref('first')
    const host = target(text, 'bottom', 'position:fixed;left:300px;top:200px')
    await userEvent.hover(T(host))
    text.value = 'second'
    await frames()
    expect(tip()!.textContent).toBe('second')
  })
})
