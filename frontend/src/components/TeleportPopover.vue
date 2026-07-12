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

  The popover owns ONLY the shell (teleport + position + theme + dismiss); the caller supplies the
  content + its own inner styling. Reuse this instead of hand-rolling another absolute-positioned
  popover that will clip.
-->
<script setup lang="ts">
import { ref, computed, watch, onBeforeUnmount, nextTick } from 'vue'

const props = withDefaults(defineProps<{
  modelValue: boolean
  anchor: HTMLElement | null            // the trigger element (its rect drives placement)
  placement?: 'bottom-start' | 'bottom-end'   // left- or right-aligned under the anchor
  gap?: number                          // px gap between anchor and popover
}>(), { placement: 'bottom-start', gap: 4 })

const emit = defineEmits<{ 'update:modelValue': [boolean] }>()
const popEl = ref<HTMLElement | null>(null)
const pos = ref<{ top: number; left: number }>({ top: 0, left: 0 })

// re-measure from the anchor rect, then clamp to the viewport and flip above when there's no room
// below — so the popover is never clipped or off-screen regardless of where the (draggable) anchor
// sits. bottom-end right-aligns to the anchor's right edge (width known post-render); bottom-start
// left-aligns.
function reposition() {
  const a = props.anchor
  if (!a) return
  const r = a.getBoundingClientRect()
  const w = popEl.value?.offsetWidth ?? 0
  const h = popEl.value?.offsetHeight ?? 0
  const vw = window.innerWidth, vh = window.innerHeight
  let left = props.placement === 'bottom-end' ? r.right - w : r.left
  left = Math.max(4, Math.min(left, vw - w - 4))
  let top = r.bottom + props.gap
  if (top + h > vh - 4) top = Math.max(4, r.top - h - props.gap)   // open above if it would overflow below
  pos.value = { top: Math.round(top), left: Math.round(left) }
}

const style = computed(() => ({ position: 'fixed' as const, top: `${pos.value.top}px`, left: `${pos.value.left}px` }))

function onDocPointer(e: PointerEvent) {
  const t = e.target as Node
  if (popEl.value?.contains(t) || props.anchor?.contains(t)) return   // click inside popover / anchor
  emit('update:modelValue', false)
}
function onKey(e: KeyboardEvent) { if (e.key === 'Escape') emit('update:modelValue', false) }

watch(() => props.modelValue, async (open) => {
  if (open) {
    await nextTick()                     // popover mounted → width known for bottom-end
    reposition()
    // capture-phase so a click inside a scroll container still dismisses; scroll/resize re-anchor.
    document.addEventListener('pointerdown', onDocPointer, true)
    document.addEventListener('keydown', onKey)
    window.addEventListener('scroll', reposition, true)
    window.addEventListener('resize', reposition)
  } else {
    document.removeEventListener('pointerdown', onDocPointer, true)
    document.removeEventListener('keydown', onKey)
    window.removeEventListener('scroll', reposition, true)
    window.removeEventListener('resize', reposition)
  }
})
onBeforeUnmount(() => {
  document.removeEventListener('pointerdown', onDocPointer, true)
  document.removeEventListener('keydown', onKey)
  window.removeEventListener('scroll', reposition, true)
  window.removeEventListener('resize', reposition)
})
</script>

<template>
  <Teleport to="body">
    <div v-if="modelValue" ref="popEl" class="cc-popover cc-dark" :style="style">
      <slot />
    </div>
  </Teleport>
</template>

<style scoped>
/* teleported out of the shell → carry theme surface/border/shadow here; callers style their content */
.cc-popover {
  z-index: 1000;
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
  border-radius: 6px;
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.35);
  color: var(--cc-text);
}
</style>
