<!--
  Generalised popover: teleported to <body> so it ESCAPES any clipping/scroll/transform ancestor
  (the recurring "my ⚙ popup gets clipped by the panel" bug — see ImageStripView, the run-log cog).
  Positioned `fixed` from the anchor element's rect, re-anchored on scroll/resize while open, and
  closed on outside-click or Escape. Teleported out of the app's `.cc-dark` shell, so it carries the
  theme tokens itself (var(--cc-*) would otherwise be undefined → transparent).

  Usage:
    <button ref="gear" @click="open = !open">⚙</button>
    <TeleportPopover v-model="open" :anchor="gear" placement="bottom-end">
      …popover content…
    </TeleportPopover>

  The popover owns the shell (teleport + position + theme + dismiss) AND the padding — the same split
  BaseModal uses, so a caller must NOT pad its own content or the two add up. `flush` tightens it for
  full-width menu rows whose hover highlight has to reach the edges. The caller supplies the content
  and its layout (width, gap, direction). Reuse this instead of hand-rolling another absolute-positioned
  popover that will clip.
-->
<script setup lang="ts">
import { ref, computed, watch, onBeforeUnmount, nextTick } from 'vue'
import { placeBox } from '../utils/anchorPosition'
import { rafCoalesce } from '../utils/rafCoalesce'

const props = withDefaults(defineProps<{
  modelValue: boolean
  anchor: HTMLElement | null            // the trigger element (its rect drives placement)
  placement?: 'bottom-start' | 'bottom-end'   // left- or right-aligned under the anchor
  gap?: number                          // px gap between anchor and popover
  flush?: boolean                       // menu content: tighter padding so hover rows reach the edges
}>(), { placement: 'bottom-start', gap: 4, flush: false })

const emit = defineEmits<{ 'update:modelValue': [boolean] }>()
const popEl = ref<HTMLElement | null>(null)
const pos = ref<{ top: number; left: number }>({ top: 0, left: 0 })

// re-measure from the anchor rect, then clamp to the viewport and flip above when there's no room
// below — so the popover is never clipped or off-screen regardless of where the (draggable) anchor
// sits. bottom-end right-aligns to the anchor's right edge (width known post-render); bottom-start
// left-aligns. The arithmetic is the shared `utils/anchorPosition.ts` (extracted FROM here, and
// covered by its own test) — `GuideBubble` uses the same positioner, so a clamping fix lands in both.
function reposition() {
  const a = props.anchor
  if (!a) return
  const r = a.getBoundingClientRect()
  pos.value = placeBox({
    anchor: { top: r.top, left: r.left, width: r.width, height: r.height },
    box: { width: popEl.value?.offsetWidth ?? 0, height: popEl.value?.offsetHeight ?? 0 },
    viewport: { width: window.innerWidth, height: window.innerHeight },
    placement: props.placement,
    gap: props.gap,
  })
}

const style = computed(() => ({ position: 'fixed' as const, top: `${pos.value.top}px`, left: `${pos.value.left}px` }))

function onDocPointer(e: PointerEvent) {
  const t = e.target as Node
  if (popEl.value?.contains(t) || props.anchor?.contains(t)) return   // click inside popover / anchor
  emit('update:modelValue', false)
}
function onKey(e: KeyboardEvent) { if (e.key === 'Escape') emit('update:modelValue', false) }

// The content can GROW after the first measurement — a popover whose body waits on a fetch (the
// thread budget in `PoolThrottle`) renders short, then gets taller once the response lands. The
// initial `reposition()` measured the short box, so the grown one runs off the bottom of the viewport
// and is clipped, with nothing to trigger a re-place: `scroll`/`resize` are about the WINDOW moving,
// not the box. Observing the box itself is the only signal that covers it, and it belongs here rather
// than in each slot's component — every popover with async or collapsible content has this bug.
//
// Scheduled through `rafCoalesce`, not applied in the callback: a growing box fires the observer per
// step, and a re-place is a paint. It also keeps the write out of delivery, which is the house rule
// for any observer that touches layout (`continuousControls.test.ts`). Safe to observe what we move
// because `reposition` writes only `top`/`left` on a `position: fixed` element — the box's SIZE comes
// from the slot content and a viewport-relative `max-height`, neither of which depends on where the
// box sits, so this cannot feed itself.
const placeSoon = rafCoalesce(() => reposition())

let ro: ResizeObserver | undefined
function observeBox() {
  if (!popEl.value || typeof ResizeObserver === 'undefined') return
  ro = new ResizeObserver(() => placeSoon.schedule())
  ro.observe(popEl.value)
}
function unobserveBox() { ro?.disconnect(); ro = undefined; placeSoon.cancel() }

watch(() => props.modelValue, async (open) => {
  if (open) {
    await nextTick()                     // popover mounted → width known for bottom-end
    reposition()
    observeBox()
    // capture-phase so a click inside a scroll container still dismisses; scroll/resize re-anchor.
    document.addEventListener('pointerdown', onDocPointer, true)
    document.addEventListener('keydown', onKey)
    window.addEventListener('scroll', reposition, true)
    window.addEventListener('resize', reposition)
  } else {
    unobserveBox()
    document.removeEventListener('pointerdown', onDocPointer, true)
    document.removeEventListener('keydown', onKey)
    window.removeEventListener('scroll', reposition, true)
    window.removeEventListener('resize', reposition)
  }
})
onBeforeUnmount(() => {
  unobserveBox()
  document.removeEventListener('pointerdown', onDocPointer, true)
  document.removeEventListener('keydown', onKey)
  window.removeEventListener('scroll', reposition, true)
  window.removeEventListener('resize', reposition)
})
</script>

<template>
  <Teleport to="body">
    <div v-if="modelValue" ref="popEl" class="cc-popover cc-dark" :class="{ flush }" :style="style">
      <slot />
    </div>
  </Teleport>
</template>

<style scoped>
/* teleported out of the shell → carry theme surface/border/shadow here; callers style their content.
   PADDING IS OWNED HERE, as BaseModal owns its body padding — callers must NOT re-add it or it doubles
   up. Each of the six call sites used to set its own and they had drifted to five different values
   (10px ×3, 8px 10px, 6px 8px, 0.25rem), which is how a new popover ended up with none at all. */
.cc-popover {
  z-index: 1000;
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md);
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.35);
  color: var(--cc-text);
  padding: 0.5rem 0.65rem;
  /* Last-resort safety net: the positioner clamps the box into the viewport, but a box TALLER than
     the viewport has nowhere to go and its bottom is simply cut off with no way to reach it. Scroll
     it instead. `8px` matches twice the positioner's default margin, so the cap and the clamp agree
     about the edge. */
  max-height: calc(100vh - 8px);
  overflow-y: auto;
}
/* `flush` is for full-width MENU content, whose rows carry their own padding and whose hover highlight
   must reach the popover's edges — an inset there leaves the highlight floating in a margin. */
.cc-popover.flush { padding: 0.25rem; }
</style>
