// SelectionTable's pinned header and frozen columns, measured in a real browser.
//
// Both are placed by more than one mechanism — CSS sticky, the `fit` wrapper that traps it, the hand
// lift (`utils/stickyHead.ts`), a pinned CollapsibleSection bar above (`--cc-sticky-above`), a padded
// modal body (`--cc-scroll-pad-top`) — and three times those disagreed in a way only an eye caught
// (#2361 the last). Each scenario here is one of those arrangements. Expected positions come from the
// scroll box's own geometry, never from the code under test. docs/todo/GUI_TESTING_PLAN.md, PR 1.
import { describe, expect, it } from 'vitest'
import { h, type VNode } from 'vue'
import SelectionTable from './SelectionTable.vue'
import CollapsibleSection from './CollapsibleSection.vue'
import BaseModal from './BaseModal.vue'
import { frames, mountInto, scrollTo } from '../test/browser/mount'

const ROWS = Array.from({ length: 40 }, (_, i) => {
  const row: Record<string, string> = { id: `r${i}`, name: `row ${i}` }
  for (let c = 0; c < 14; c++) row[`c${c}`] = `${i}.${c}`
  return row
})
const NARROW = [
  { key: 'name', label: 'Name', sticky: true, width: 160 },
  { key: 'c0', label: 'A', width: 100 },
  { key: 'c1', label: 'B', width: 100 },
]
// wider than the 600px box, so `fit="content"` puts the table in a sideways scroller
const WIDE = [
  { key: 'name', label: 'Name', sticky: true, width: 160 },
  ...Array.from({ length: 14 }, (_, c) => ({ key: `c${c}`, label: `Col ${c}`, width: 120 })),
]

let keyN = 0
function table(columns: typeof NARROW, fit: 'fill' | 'content'): VNode {
  return h(SelectionTable, {
    columns, rows: ROWS, idKey: 'id', selectionMode: 'multi', selected: [],
    columnWidthKey: `gui-test-${keyN++}`, fit, stickyHeader: true,
  })
}
// the page's own vertical scroll box: fixed size so every run measures the same layout
const box = (child: VNode) => h('div', { class: 't-box', style: 'height:300px;width:600px;overflow-y:auto' }, [child])
// what ImageTable does around a wide table (`.table-scroll`)
const sideways = (child: VNode) => h('div', { style: 'overflow-x:auto;width:100%' }, [child])

const top = (el: Element) => el.getBoundingClientRect().top
const els = (host: HTMLElement) => {
  const tableEl = host.querySelector<HTMLTableElement>('table.sel-table')!
  return {
    tableEl,
    th: tableEl.querySelector<HTMLElement>('thead th')!,
    frozenTh: tableEl.querySelector<HTMLElement>('thead th.sel-sticky:not(.sel-sticky-pick)')!,
    frozenTd: () => {
      // the first body row still below the header
      const headBottom = tableEl.tHead!.getBoundingClientRect().bottom
      const tr = [...tableEl.tBodies[0].rows].find(r => r.getBoundingClientRect().top >= headBottom)!
      return tr.querySelector<HTMLElement>('td.sel-sticky:not(.sel-sticky-pick)')!
    },
  }
}
/** The line a pinned header must sit on: the scroll box's padding-box top, plus a pinned bar's height. */
const pinLine = (scroller: HTMLElement, bar?: HTMLElement) =>
  top(scroller) + scroller.clientTop + (bar ? bar.getBoundingClientRect().height : 0)

async function checkPinned(host: HTMLElement, scroller: HTMLElement, bar?: HTMLElement) {
  const { tableEl, th } = els(host)
  await frames()
  // at rest the header is in the table's flow — not pushed into the rows
  expect(top(th)).toBeCloseTo(top(tableEl), 0)
  // scrolled: on the pin line, whatever the arrangement
  await scrollTo(scroller, 300)
  expect(Math.abs(top(th) - pinLine(scroller, bar))).toBeLessThanOrEqual(1)
  // scrolled past the end: it leaves with the table, never below its last row
  await scrollTo(scroller, 5000)
  expect(th.getBoundingClientRect().bottom).toBeLessThanOrEqual(tableEl.getBoundingClientRect().bottom + 1)
}

async function checkFrozen(host: HTMLElement, scroller: HTMLElement) {
  const { frozenTh, frozenTd } = els(host)
  await scrollTo(scroller, 300)
  const wrapper = host.querySelector<HTMLElement>('table.sel-table')!.parentElement!
  await scrollTo(wrapper, wrapper.scrollTop, 400)
  expect(wrapper.scrollLeft).toBeGreaterThan(0)              // the scenario really scrolled sideways
  expect(Math.abs(frozenTh.getBoundingClientRect().left - frozenTd().getBoundingClientRect().left)).toBeLessThanOrEqual(1)
}

describe('SelectionTable stickyHeader', () => {
  it('plain: pins to the scroll box it sits in (CSS sticky)', async () => {
    const host = mountInto({ render: () => box(table(NARROW, 'fill')) })
    await checkPinned(host, host.querySelector('.t-box')!)
  })

  it('inside an overflow-x wrapper: pins to the outer box, which is the one that scrolls', async () => {
    const host = mountInto({ render: () => box(sideways(table(WIDE, 'content'))) })
    const scroller = host.querySelector<HTMLElement>('.t-box')!
    await checkPinned(host, scroller)
    await checkFrozen(host, scroller)
  })

  it('under a pinned CollapsibleSection bar: stops below the bar, not under it or a bar-height lower', async () => {
    const host = mountInto({
      render: () => box(h(CollapsibleSection, { label: 'Images', maxHeight: 'none', sticky: true },
        () => sideways(table(WIDE, 'content')))),
    })
    const scroller = host.querySelector<HTMLElement>('.t-box')!
    await checkPinned(host, scroller, host.querySelector<HTMLElement>('.cs-toggle')!)
    await checkFrozen(host, scroller)
  })

  it('in a padded BaseModal body: no strip of rows above the header', async () => {
    const host = mountInto({ render: () => h(BaseModal, { title: 'Pick', height: '400px' }, () => table(NARROW, 'fill')) })
    const body = host.querySelector<HTMLElement>('.cc-modal-body')!
    expect(parseFloat(getComputedStyle(body).paddingTop)).toBeGreaterThan(0)   // the scenario has padding
    expect(body.scrollHeight).toBeGreaterThan(body.clientHeight)              // …and really scrolls
    await checkPinned(host, body)
  })
})
