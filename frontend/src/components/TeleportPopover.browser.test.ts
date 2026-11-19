// TeleportPopover and BaseModal in a real browser: where the popover lands against its anchor and the
// viewport, that it escapes a clipping box, follows a scroll, re-places when it grows, and how both
// surfaces dismiss — Esc reaching only the TOPMOST one. docs/todo/GUI_TESTING_PLAN.md, P3b.
import { describe, expect, it } from 'vitest'
import { defineComponent, h, ref, type VNode } from 'vue'
import { userEvent } from 'vitest/browser'
import TeleportPopover from './TeleportPopover.vue'
import BaseModal from './BaseModal.vue'
import { frames, mountInto, scrollTo } from '../test/browser/mount'

const rect = (el: Element) => el.getBoundingClientRect()
const pop = () => document.querySelector<HTMLElement>('.cc-popover')

/** An anchor button and its popover, opened as the app opens one — closed at mount, then set open
 *  (placement and dismissal start on that change). `wrap` puts the anchor inside a scenario box. */
function withPopover(opts: { placement?: 'bottom-start' | 'bottom-end'; content?: () => VNode | string;
                             wrap?: (anchor: VNode) => VNode; anchorStyle?: string } = {}) {
  const open = ref(false)
  const anchor = ref<HTMLElement | null>(null)
  const host = mountInto(defineComponent({
    setup: () => () => {
      const a = h('button', { ref: anchor, class: 't-anchor', style: opts.anchorStyle ?? 'width:60px;height:24px' }, 'open')
      return h('div', [
        opts.wrap ? opts.wrap(a) : a,
        h('button', { class: 't-outside' }, 'elsewhere'),
        h(TeleportPopover, { modelValue: open.value, anchor: anchor.value, placement: opts.placement,
          'onUpdate:modelValue': (v: boolean) => { open.value = v } },
          () => (opts.content ?? (() => h('div', { class: 't-pop', style: 'width:200px;height:120px' }, 'menu')))()),
      ])
    },
  }))
  return { host, open, anchor: () => host.querySelector<HTMLElement>('.t-anchor')!,
           ready: async () => { open.value = true; await frames(3) } }
}

describe('TeleportPopover placement', () => {
  it('bottom-start: under the anchor, left edges aligned, `gap` below it', async () => {
    const { anchor, ready } = withPopover({ anchorStyle: 'margin:40px 0 0 100px;width:60px;height:24px' })
    await ready()
    expect(rect(pop()!).left).toBeCloseTo(rect(anchor()).left, 0)
    expect(rect(pop()!).top).toBeCloseTo(rect(anchor()).bottom + 4, 0)
  })

  it('bottom-end: right edges aligned', async () => {
    const { anchor, ready } = withPopover({ placement: 'bottom-end', anchorStyle: 'margin:40px 0 0 400px;width:60px;height:24px' })
    await ready()
    expect(rect(pop()!).right).toBeCloseTo(rect(anchor()).right, 0)
  })

  it('no room below: flips above the anchor, still inside the viewport', async () => {
    const { anchor, ready } = withPopover({ anchorStyle: `position:fixed;top:${window.innerHeight - 40}px;left:100px;width:60px;height:24px` })
    await ready()
    expect(rect(pop()!).bottom).toBeLessThanOrEqual(rect(anchor()).top)
    expect(rect(pop()!).top).toBeGreaterThanOrEqual(0)
  })

  it('escapes a clipping, scrolling ancestor — the whole box is hit-testable', async () => {
    const { ready } = withPopover({ wrap: a => h('div', { style: 'margin-top:40px;width:120px;height:40px;overflow:hidden' }, [a]) })
    await ready()
    const r = rect(pop()!)
    expect(r.height).toBeGreaterThan(100)
    expect(pop()!.contains(document.elementFromPoint(r.left + r.width / 2, r.bottom - 5))).toBe(true)
  })

  it('follows its anchor when the box around it scrolls', async () => {
    const { host, anchor, ready } = withPopover({
      wrap: a => h('div', { class: 't-box', style: 'height:200px;overflow-y:auto' },
        [h('div', { style: 'height:60px' }), a, h('div', { style: 'height:1000px' })]),
    })
    await ready()
    await scrollTo(host.querySelector('.t-box')!, 40)
    expect(rect(pop()!).top).toBeCloseTo(rect(anchor()).bottom + 4, 0)
  })

  it('content that grows after opening is re-placed, not left running off the viewport', async () => {
    const tall = ref(40)
    const { anchor, ready } = withPopover({
      anchorStyle: `position:fixed;top:${window.innerHeight - 300}px;left:100px;width:60px;height:24px`,
      content: () => h('div', { style: `width:200px;height:${tall.value}px` }, 'grows'),
    })
    await ready()
    expect(rect(pop()!).top).toBeCloseTo(rect(anchor()).bottom + 4, 0)   // fits below at first
    tall.value = 400
    await frames(4)
    expect(rect(pop()!).bottom).toBeLessThanOrEqual(window.innerHeight)
  })
})

describe('TeleportPopover dismissal', () => {
  it('a click inside keeps it, outside closes it', async () => {
    const { open, ready } = withPopover()
    await ready()
    await userEvent.click(document.querySelector('.t-pop')!)
    expect(open.value).toBe(true)
    await userEvent.click(document.querySelector('.t-outside')!)
    expect(open.value).toBe(false)
  })

  it('Escape closes it', async () => {
    const { open, ready } = withPopover()
    await ready()
    await userEvent.keyboard('{Escape}')
    expect(open.value).toBe(false)
  })
})

describe('TeleportPopover nested', () => {
  it('Escape closes the inner popover only; the next one closes the outer', async () => {
    const outer = ref(false)
    const inner = ref(false)
    const a1 = ref<HTMLElement | null>(null)
    const a2 = ref<HTMLElement | null>(null)
    mountInto(defineComponent({
      setup: () => () => h('div', [
        h('button', { ref: a1 }, 'outer'),
        h(TeleportPopover, { modelValue: outer.value, anchor: a1.value,
          'onUpdate:modelValue': (v: boolean) => { outer.value = v } }, () => [
          h('button', { ref: a2 }, 'inner'),
          h(TeleportPopover, { modelValue: inner.value, anchor: a2.value,
            'onUpdate:modelValue': (v: boolean) => { inner.value = v } }, () => 'inner menu'),
        ]),
      ]),
    }))
    outer.value = true
    await frames(3)
    inner.value = true
    await frames(3)
    await userEvent.keyboard('{Escape}')
    expect([outer.value, inner.value]).toEqual([true, false])
    await frames()
    await userEvent.keyboard('{Escape}')
    expect(outer.value).toBe(false)
  })
})

// ── BaseModal ─────────────────────────────────────────────────────────────────────────────────────
function modal(body: () => VNode | VNode[] | string, extra: Record<string, unknown> = {}) {
  const closed = ref(0)
  const host = mountInto({ render: () => h(BaseModal, { title: 'T', onClose: () => { closed.value++ }, ...extra }, body) })
  return { host, closed }
}

describe('BaseModal', () => {
  it('Escape and the close button close it', async () => {
    const { host, closed } = modal(() => 'body')
    await frames()
    await userEvent.keyboard('{Escape}')
    expect(closed.value).toBe(1)
    await userEvent.click(host.querySelector('.cc-modal-close')!)
    expect(closed.value).toBe(2)
  })

  it('a press that starts inside the dialog and ends on the backdrop does not close it; a backdrop click does', async () => {
    const { host, closed } = modal(() => 'body')
    await frames()
    const overlay = host.querySelector<HTMLElement>('.cc-modal-overlay')!
    const body = host.querySelector<HTMLElement>('.cc-modal-body')!
    // the browser's own click target after a drag-release is the common ancestor: the overlay
    body.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }))
    overlay.dispatchEvent(new MouseEvent('click', { bubbles: true }))
    expect(closed.value).toBe(0)
    await userEvent.click(overlay, { position: { x: 5, y: 5 } })
    expect(closed.value).toBe(1)
  })

  it('Escape reaches only the topmost surface: a popover open in it, then a dialog inside it', async () => {
    const openPop = ref(false)
    const anchor = ref<HTMLElement | null>(null)
    const outer = ref(0)
    const inner = ref(0)
    mountInto(defineComponent({
      setup: () => () => h(BaseModal, { title: 'Outer', onClose: () => { outer.value++ } }, () => [
        h('button', { ref: anchor }, 'menu'),
        h(TeleportPopover, { modelValue: openPop.value, anchor: anchor.value,
          'onUpdate:modelValue': (v: boolean) => { openPop.value = v } }, () => 'popover'),
        h(BaseModal, { title: 'Inner', onClose: () => { inner.value++ } }, () => 'nested'),
      ]),
    }))
    openPop.value = true
    await frames(3)
    await userEvent.keyboard('{Escape}')                 // the popover is last in the DOM: it goes first
    expect([openPop.value, inner.value, outer.value]).toEqual([false, 0, 0])
    await frames()
    await userEvent.keyboard('{Escape}')                 // then the nested dialog, not its parent
    expect([inner.value, outer.value]).toEqual([1, 0])
  })

  it('Escape on a menu open in a dialog closes the menu, not the dialog', async () => {
    const openPop = ref(false)
    const anchor = ref<HTMLElement | null>(null)
    const closed = ref(0)
    mountInto(defineComponent({
      setup: () => () => h(BaseModal, { title: 'M', onClose: () => { closed.value++ } }, () => [
        h('button', { ref: anchor }, 'menu'),
        h(TeleportPopover, { modelValue: openPop.value, anchor: anchor.value,
          'onUpdate:modelValue': (v: boolean) => { openPop.value = v } }, () => 'popover'),
      ]),
    }))
    await frames()
    openPop.value = true
    await frames(3)
    await userEvent.keyboard('{Escape}')
    expect([openPop.value, closed.value]).toEqual([false, 0])
  })

  it('a popover opened from a dialog paints above it', async () => {
    const anchor = ref<HTMLElement | null>(null)
    const open = ref(false)
    mountInto(defineComponent({
      setup: () => () => h(BaseModal, { title: 'M' }, () => [
        h('button', { ref: anchor }, 'menu'),
        h(TeleportPopover, { modelValue: open.value, anchor: anchor.value }, () => h('div', { style: 'width:150px;height:60px' }, 'pop')),
      ]),
    }))
    await frames()
    open.value = true
    await frames(3)
    const r = rect(pop()!)
    expect(pop()!.contains(document.elementFromPoint(r.left + 10, r.top + 10))).toBe(true)
  })

  it('long content scrolls in the body; header and footer stay on screen', async () => {
    const { host } = modal(() => h('div', { style: 'height:3000px' }, 'long'), {})
    await frames()
    const body = host.querySelector<HTMLElement>('.cc-modal-body')!
    const box = rect(host.querySelector('.cc-modal')!)
    expect(body.scrollHeight).toBeGreaterThan(body.clientHeight)
    expect(box.height).toBeLessThanOrEqual(window.innerHeight * 0.9 + 1)
    expect(rect(host.querySelector('.cc-modal-header')!).top).toBeGreaterThanOrEqual(0)
  })
})
