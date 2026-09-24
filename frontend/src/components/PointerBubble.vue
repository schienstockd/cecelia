<!--
  PointerBubble — the bare "look here" bubble for Claude's UI-anchor point-outs (BIDIR PR #5 of
  docs/todo/BIDIR_CONTEXT_PLAN.md). Renders one small pulsing dot + label chip per active
  `viewerStore.uiMarks` entry, anchored to whatever `utils/guideAnchor.ts::resolveAnchor` returns
  for the mark's `anchor` id. Deliberately smaller than `GuideBubble.vue`: no gates, no reveal
  logic, no back/forward — a point-out is one direction, ephemeral, dismissable.

  DIVISION OF LABOUR.
    - `stores/viewer.ts::uiMarks` — the ephemeral bag (session-only; TTL-pruned by setTimeout).
    - `utils/guideAnchor.ts::resolveAnchor` — id → live element, same scheme the guide runtime uses.
    - This SFC — poll the anchors (they may mount/unmount), position a floating dot beside each,
      dismiss on click.

  Why a poll and not ResizeObserver / MutationObserver: the anchor MOUNT is what we care about,
  and it may happen minutes after the mark arrives (a panel the user hasn't opened yet). One
  ~250ms interval alive only while there are marks — same shape as `stores/guide.ts`'s poll. Cheap
  compared to observing the whole DOM.
-->
<script setup lang="ts">
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { useViewerStore } from '../stores/viewer'
import { resolveAnchor, isReachable } from '../utils/guideAnchor'
import { PANEL_Z_BASE } from '../utils/panelStack'

const viewer = useViewerStore()
const POLL_MS = 250

// A ref that ticks on every poll — computed positions read it so they re-evaluate against a fresh
// getBoundingClientRect. Same trick `stores/guide.ts::domTick` uses.
const domTick = ref(0)
let poll: ReturnType<typeof setInterval> | null = null

function startPoll() {
  if (poll) return
  poll = setInterval(() => { domTick.value++ }, POLL_MS)
}
function stopPoll() { if (poll) { clearInterval(poll); poll = null } }

// Start/stop the poll with the bag — no marks, no work.
watch(() => viewer.uiMarks.length, n => (n ? startPoll() : stopPoll()), { immediate: true })
onBeforeUnmount(() => stopPoll())

interface Placed {
  markerId: string
  label: string
  anchor: string
  reachable: boolean
  top: number
  left: number
  width: number
  height: number
  // the anchor sits inside a floating panel (Kiwi, Viewer, Lab log, corrections…) — see the template
  inPanel: boolean
}

// Two layers: a mark on page content sits just UNDER the floating panels, so an open panel covers
// it (a plot highlight used to paint over Kiwi when Kiwi was expanded again); a mark on something
// inside a panel sits above them all, or its own panel would hide it.
const PAGE_Z = PANEL_Z_BASE - 1
const placed = computed<Placed[]>(() => {
  void domTick.value
  return viewer.uiMarks.map(m => {
    const el = resolveAnchor(m.anchor)
    const reachable = isReachable(el)
    const r = el?.getBoundingClientRect()
    return {
      markerId: m.markerId,
      label: m.label || '',
      anchor: m.anchor,
      reachable,
      top: r ? r.top : 0,
      left: r ? r.left : 0,
      width: r ? r.width : 0,
      height: r ? r.height : 0,
      inPanel: !!el?.closest('.fp'),
    }
  })
})

function dismiss(id: string) { viewer.dismissUiMark(id) }
</script>

<template>
  <!-- Fixed layers below modals: page marks under the floating panels (PAGE_Z), panel marks above
       them (1400, under the guide bubble at 1500). Only the dots + chips receive pointer events; the
       layers themselves are transparent to clicks. -->
  <div v-for="top in [false, true]" :key="String(top)" class="pb-layer"
       :style="top ? undefined : { zIndex: PAGE_Z }">
    <template v-for="p in placed.filter(x => x.inPanel === top)" :key="p.markerId">
      <div v-if="p.reachable" class="pb-dot"
           :style="{ top: (p.top + p.height / 2) + 'px',
                     left: (p.left + p.width  + 8)  + 'px' }">
        <span class="pb-pulse" />
        <span v-if="p.label" class="pb-label cc-fs-2xs">{{ p.label }}</span>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro pb-dismiss"
                @click="dismiss(p.markerId)" v-tooltip.top="'Dismiss'"
                aria-label="Dismiss pointer"><i class="pi pi-times" /></button>
      </div>
    </template>
  </div>
</template>

<style scoped>
.pb-layer {
  position: fixed; inset: 0; pointer-events: none; z-index: 1400;
}
.pb-dot {
  position: absolute; transform: translateY(-50%);
  display: flex; align-items: center; gap: 0.35rem;
  padding: 0.15rem 0.4rem 0.15rem 0.55rem;
  background: var(--cc-surface-1); border: 1px solid var(--cc-warn);
  border-radius: var(--cc-radius-pill);
  pointer-events: auto;
  box-shadow: 0 0 0 2px rgba(0,0,0,0.35);
}
.pb-pulse {
  width: 0.5rem; height: 0.5rem; border-radius: 50%;
  background: var(--cc-warn);
  animation: pb-pulse 1.4s ease-out infinite;
  flex-shrink: 0;
}
/* Halo colour is a fixed rgba to match --cc-warn (#f59e0b) — the token is a hex variable and
 * `rgba(var(...), a)` can't wrap it. Amber, not the app's accent purple, so a Claude pointer
 * stands out against surfaces already coloured with the brand accent. */
@keyframes pb-pulse {
  0%   { box-shadow: 0 0 0 0 rgba(245, 158, 11, 0.7); }
  70%  { box-shadow: 0 0 0 10px rgba(245, 158, 11, 0); }
  100% { box-shadow: 0 0 0 0   rgba(245, 158, 11, 0); }
}
.pb-label { color: var(--cc-text); white-space: nowrap; max-width: 20rem;
            overflow: hidden; text-overflow: ellipsis; }
</style>
