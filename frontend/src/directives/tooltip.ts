// `v-tooltip` — the app's one tooltip, registered in main.ts. Replaces PrimeVue's directive
// (docs/todo/PRIMEVUE_RETIRE_PLAN.md, P2) with the same binding (utils/tooltipOptions.ts parses it),
// placed by the app's one positioner, `utils/anchorPosition.ts`: the preferred side, PrimeVue's flip
// order when it doesn't fit, and clamped into the viewport on all four sides, where PrimeVue only
// clamped `.top`/`.bottom` horizontally.
//
// DOM contract (docs/ui/PRIMITIVES.md): the tip is appended to <body>, so it inherits only the
// :root tokens, never a panel's; style it through `.cc-tooltip` / `.cc-tooltip-text` in style.css,
// and SIZE THE ROOT, NEVER THE TEXT — the root is the box that gets measured.
//
// One tip at a time. It shows on mouseenter and hides on mouseleave, click, Escape, any scroll,
// a window resize, or the pointer reaching anything outside its target — including when the target
// left the document (a KeepAlive'd page going inactive does not unmount its directives). A visible
// tip follows a changed binding.
import type { Directive, DirectiveBinding } from 'vue'
import { placeBox } from '../utils/anchorPosition'
import { tooltipSpec, type TooltipBinding, type TooltipSpec } from '../utils/tooltipOptions'

const GAP = 4      // px between target and tip
const EDGE = 4     // px a tip keeps from the viewport edge

interface Target extends HTMLElement { _ccTip?: TooltipSpec | null }

let active: { el: Target; tip: HTMLElement; stop: () => void } | null = null
let nextId = 0

function render(tip: HTMLElement, spec: TooltipSpec) {
  tip.className = spec.cls ? `cc-tooltip ${spec.cls}` : 'cc-tooltip'
  const text = tip.firstElementChild as HTMLElement
  if (spec.escape) text.textContent = spec.text
  else text.innerHTML = spec.text
}

function place(el: Target, tip: HTMLElement) {
  const spec = el._ccTip
  if (!spec) return
  if (!el.isConnected) { hide(); return }
  const a = el.getBoundingClientRect()
  const { top, left } = placeBox({
    anchor: { top: a.top, left: a.left, width: a.width, height: a.height },
    box: { width: tip.offsetWidth, height: tip.offsetHeight },
    viewport: { width: window.innerWidth, height: window.innerHeight },
    placement: spec.side, fallbacks: spec.fallbacks, gap: GAP, margin: EDGE,
  })
  tip.style.left = `${left}px`
  tip.style.top = `${top}px`
}

function onKey(e: KeyboardEvent) { if (e.key === 'Escape') hide() }

function show(el: Target) {
  const spec = el._ccTip
  if (!spec) return
  hide()
  const tip = document.createElement('div')
  tip.id = `cc-tooltip-${++nextId}`
  tip.setAttribute('role', 'tooltip')
  tip.appendChild(document.createElement('div')).className = 'cc-tooltip-text'
  render(tip, spec)
  document.body.appendChild(tip)
  el.setAttribute('aria-describedby', tip.id)

  place(el, tip)
  // the pointer over anything outside the target hides it — which also catches a target that left the
  // document without a mouseleave (a KeepAlive'd page going inactive)
  const onOver = (e: MouseEvent) => { if (!el.isConnected || !el.contains(e.target as Node)) hide() }
  document.addEventListener('mouseover', onOver, { passive: true })
  window.addEventListener('scroll', hide, { capture: true, passive: true })
  window.addEventListener('resize', hide)
  document.addEventListener('keydown', onKey)
  active = {
    el, tip,
    stop: () => {
      document.removeEventListener('mouseover', onOver)
      window.removeEventListener('scroll', hide, { capture: true })
      window.removeEventListener('resize', hide)
      document.removeEventListener('keydown', onKey)
    },
  }
}

function hide() {
  if (!active) return
  const { el, tip, stop } = active
  active = null
  stop()
  tip.remove()
  if (el.getAttribute('aria-describedby') === tip.id) el.removeAttribute('aria-describedby')
}

function onEnter(e: Event) { show(e.currentTarget as Target) }
function onLeave(e: Event) { if (active?.el === e.currentTarget) hide() }

function bind(el: Target, binding: DirectiveBinding<TooltipBinding>) {
  el._ccTip = tooltipSpec(binding.value, binding.modifiers)
  if (active?.el !== el) return
  if (!el._ccTip) { hide(); return }
  render(active.tip, el._ccTip)
  place(el, active.tip)
}

export const tooltip: Directive<Target, TooltipBinding> = {
  mounted(el, binding) {
    bind(el, binding)
    el.addEventListener('mouseenter', onEnter)
    el.addEventListener('mouseleave', onLeave)
    el.addEventListener('click', onLeave)
  },
  updated(el, binding) {
    if (binding.value !== binding.oldValue) bind(el, binding)
  },
  beforeUnmount(el) {
    if (active?.el === el) hide()
    el.removeEventListener('mouseenter', onEnter)
    el.removeEventListener('mouseleave', onLeave)
    el.removeEventListener('click', onLeave)
    el._ccTip = null
  },
}
