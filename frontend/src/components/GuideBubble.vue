<!--
  The guide bubble: a teleported card beside the control a step is about, plus a ring drawn around
  that control. Mounted ONCE in App.vue (it has to survive route changes); everything it shows comes
  from `stores/guide.ts`. Design: docs/todo/GUIDE_SYSTEM_PLAN.md.

  Deliberately NOT `TeleportPopover` (plan D10). The two share their positioning maths — both call
  `utils/anchorPosition.placeBox` — but the event models genuinely conflict: a popover dismisses on
  outside-click, whereas for a guide an outside click is usually THE ACTION BEING TAUGHT. Bending one
  component around both contracts would give it two contradictory dismissal rules.

  There is no dimmed backdrop and nothing is blocked (plan D9): a spotlight overlay would fight
  FloatingPanel's stacking and PrimeVue's overlays, and dimming is hostile when the whole point is for
  the user to work in the app while reading. Ring + bubble, `pointer-events: none` on the ring.

  Four things it renders, in priority order:
    off-route  — the step belongs to another page; point at the sidebar item instead of navigating
    failed     — the parked task failed; offer the log and a retry
    waiting    — parked on a long task (plan D3)
    step       — the ordinary case
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick } from 'vue'
import { useGuideStore } from '../stores/guide'
import { openGuides } from '../lib/guideOpen'
import { placeBox, arrowOffset, type Placed } from '../utils/anchorPosition'
import { resolveAnchor, isReachable, scrollAnchorIntoView, NAV_PREFIX } from '../utils/guideAnchor'

const guide = useGuideStore()

const boxEl = ref<HTMLElement | null>(null)
const anchorEl = ref<HTMLElement | null>(null)
const placed = ref<Placed | null>(null)
const arrowAt = ref(0)
const ringRect = ref<{ top: number; left: number; width: number; height: number } | null>(null)

const step = computed(() => guide.step)
// Off-route: the guide never navigates (plan D1), so redirect the bubble to the nav item for the page
// the step lives on and let the user click it.
const offRouteAnchor = computed(() =>
  guide.offRoute && guide.currentStep?.route ? `${NAV_PREFIX}${guide.currentStep.route}` : null)
const activeAnchorId = computed(() => offRouteAnchor.value ?? step.value?.anchor)

const isLast = computed(() => guide.index >= guide.total - 1)

// ── placement ───────────────────────────────────────────────────────────────────────────────────
function reposition() {
  const el = anchorEl.value
  const box = boxEl.value
  if (!box) return
  const size = { width: box.offsetWidth, height: box.offsetHeight }
  const vp = { width: window.innerWidth, height: window.innerHeight }

  if (!el || !isReachable(el)) {
    // No anchor (or it's hidden): degrade to a centred card rather than dead-ending (plan D4).
    placed.value = null
    ringRect.value = null
    return
  }
  const r = el.getBoundingClientRect()
  const anchor = { top: r.top, left: r.left, width: r.width, height: r.height }
  const p = placeBox({
    anchor, box: size, viewport: vp,
    placement: offRouteAnchor.value ? 'right' : (step.value?.placement ?? 'right'),
    gap: 12,
  })
  placed.value = p
  arrowAt.value = arrowOffset(p, anchor, size, 14)
  ringRect.value = anchor
}

const boxStyle = computed(() => placed.value
  ? { position: 'fixed' as const, top: `${placed.value.top}px`, left: `${placed.value.left}px` }
  // centred fallback — a step with no reachable anchor still reads
  : { position: 'fixed' as const, top: '50%', left: '50%', transform: 'translate(-50%, -50%)' })

const ringStyle = computed(() => ringRect.value
  ? {
      top: `${ringRect.value.top - 4}px`, left: `${ringRect.value.left - 4}px`,
      width: `${ringRect.value.width + 8}px`, height: `${ringRect.value.height + 8}px`,
    }
  : undefined)

const arrowStyle = computed(() => {
  const p = placed.value
  if (!p) return undefined
  return p.side === 'top' || p.side === 'bottom'
    ? { left: `${arrowAt.value}px` }
    : { top: `${arrowAt.value}px` }
})

// ── anchor tracking ─────────────────────────────────────────────────────────────────────────────
// The anchor is re-resolved on the store's poll: a step's target may not exist yet (a lazily loaded
// module page, a dialog that opens on the previous step), and rows re-render underneath us.
let clickTarget: HTMLElement | null = null
function detachClick() {
  clickTarget?.removeEventListener('click', onAnchorClick)
  clickTarget = null
}
function onAnchorClick() { guide.satisfyClick() }

function syncAnchor() {
  const found = resolveAnchor(activeAnchorId.value)
  if (found !== anchorEl.value) {
    anchorEl.value = found
    if (found) scrollAnchorIntoView(found)
  }
  // (Re)bind the click listener when the element identity changes — a `v-for` re-render swaps the
  // node out from under us, which is why this is re-checked rather than bound once (plan R7).
  const wantClick = !!step.value?.clickAnchor && !guide.offRoute
  if (!wantClick || found !== clickTarget) detachClick()
  if (wantClick && found && clickTarget !== found) {
    clickTarget = found
    found.addEventListener('click', onAnchorClick)
  }
  reposition()
}

// The store's poll drives re-resolution + re-measure; scroll/resize re-anchor immediately.
watch(() => guide.domTick, syncAnchor)
// Watch PRIMITIVES, not `step` itself: while a `reveal` is showing, the store's `step` computed
// returns a fresh object literal on every poll tick, so watching it would re-run this (and churn the
// click listener) four times a second for no change.
watch([() => guide.active?.id, () => guide.index, activeAnchorId, () => guide.needsReveal], async () => {
  detachClick()
  await nextTick()
  syncAnchor()
}, { immediate: true })

onMounted(async () => {
  await nextTick()
  syncAnchor()
  window.addEventListener('scroll', reposition, true)
  window.addEventListener('resize', reposition)
  window.addEventListener('keydown', onKey)
})
onBeforeUnmount(() => {
  detachClick()
  window.removeEventListener('scroll', reposition, true)
  window.removeEventListener('resize', reposition)
  window.removeEventListener('keydown', onKey)
})

// Escape leaves the guide — but NOT when a dialog is open, because Escape is how you close the dialog
// (`BaseModal` listens for it too). The import guide opens the FileBrowser mid-walkthrough, so without
// this guard dismissing the picker would silently kill the guide as well.
function onKey(e: KeyboardEvent) {
  if (e.key !== 'Escape') return
  if (document.querySelector('.cc-modal-overlay')) return
  guide.exit()
}

// ── the parked-task footer ──────────────────────────────────────────────────────────────────────
const waitLabel = computed(() => guide.currentStep?.awaitTask?.label ?? 'Running')
// Note what is deliberately absent: a "show me the run" button. It would have to un-collapse the
// functions panel and scroll the task list — i.e. the guide reaching into app state, which is the one
// thing the runtime does not do (plan D1). It says where to look instead.
</script>

<template>
  <Teleport to="body">
    <!-- the ring: never intercepts a click, so the control it surrounds stays usable -->
    <div v-if="ringStyle" class="guide-ring cc-dark" :style="ringStyle" aria-hidden="true" />

    <div v-if="step" ref="boxEl" class="guide-bubble cc-dark" :style="boxStyle"
         role="dialog" aria-live="polite" :aria-label="`Guide: ${guide.active?.title}`">
      <div v-if="placed" class="guide-arrow" :class="`side-${placed.side}`" :style="arrowStyle" />

      <header class="gb-head">
        <span class="gb-guide cc-eyebrow cc-fs-2xs">{{ guide.active?.title }}</span>
        <span class="gb-count cc-readout cc-fs-2xs">{{ guide.index + 1 }} / {{ guide.total }}</span>
        <button class="gb-x cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="guide.exit()"
                v-tooltip.left="'Leave the guide'" aria-label="Leave the guide">
          <i class="pi pi-times" />
        </button>
      </header>

      <!-- finished: acknowledge it, and offer the next guide rather than just vanishing -->
      <template v-if="guide.phase === 'done'">
        <p class="gb-title"><i class="pi pi-check-circle gb-ok" /> {{ guide.active?.title }} — done</p>
        <p class="gb-text cc-muted">Ticked off in the guide list. Come back any time.</p>
      </template>

      <!-- off-route: the step is about another page -->
      <template v-else-if="guide.offRoute">
        <p class="gb-text"><strong>Back to {{ guide.currentStep?.route }}</strong></p>
        <p class="gb-text cc-muted">This step is on another page — open it from the menu.</p>
      </template>

      <!-- the parked task failed -->
      <template v-else-if="guide.phase === 'failed'">
        <p class="gb-text gb-fail">
          <i class="pi pi-exclamation-triangle" /> {{ guide.failureNote }}
        </p>
        <p class="gb-text cc-muted">Click the run in the task list to read its log, then try again.</p>
        <div class="gb-actions">
          <button class="cc-btn cc-btn-ghost cc-fs-xs" @click="guide.rearm()"
                  v-tooltip.top="'Wait for the next run of this function'">Wait again</button>
        </div>
      </template>

      <!-- parked on a long task -->
      <template v-else-if="guide.phase === 'waiting'">
        <p class="gb-text">
          <i class="pi pi-spin pi-spinner" /> {{ waitLabel }} — I'll pick up when it finishes.
        </p>
        <p v-if="guide.awaitedTask" class="gb-text cc-muted cc-fs-xs">
          {{ guide.awaitedTask.imageName }}
          <template v-if="guide.awaitedTask.progress != null">
            · {{ Math.round(guide.awaitedTask.progress * 100) }}%
          </template>
        </p>
        <p class="gb-text cc-muted cc-fs-xs">Runs are server-side — you can keep working.</p>
      </template>

      <!-- the ordinary case -->
      <template v-else>
        <p v-if="step.title" class="gb-title">{{ step.title }}</p>
        <p class="gb-text">{{ step.text }}</p>
        <ul v-if="step.bullets?.length" class="gb-bullets cc-muted cc-fs-xs">
          <li v-for="(b, i) in step.bullets" :key="i">{{ b }}</li>
        </ul>
        <p v-if="!anchorEl && step.anchor" class="gb-text cc-muted cc-fs-xs">
          <i class="pi pi-info-circle" /> That control isn't on screen right now.
        </p>
      </template>

      <footer class="gb-foot">
        <template v-if="guide.phase === 'done'">
          <button class="cc-btn cc-btn-bare cc-btn-micro cc-fs-xs" @click="openGuides()"
                  v-tooltip.top="'Back to the guide list'">More guides</button>
          <span class="gb-spacer" />
          <button class="cc-btn cc-btn-primary cc-btn-micro cc-fs-xs" @click="guide.exit()"
                  v-tooltip.top="'Close the guide'">Close</button>
        </template>
        <template v-else>
          <button class="cc-btn cc-btn-bare cc-btn-micro cc-fs-xs" :disabled="guide.index === 0"
                  @click="guide.back()" v-tooltip.top="'Previous step'">Back</button>
          <span class="gb-spacer" />
          <span v-if="guide.gateSatisfied" class="gb-tick" v-tooltip.top="'Done — moving on'">
            <i class="pi pi-check" />
          </span>
          <button class="cc-btn cc-btn-primary cc-btn-micro cc-fs-xs" @click="guide.next()"
                  v-tooltip.top="isLast ? 'Finish the guide' : 'Skip to the next step'">
            {{ isLast ? 'Done' : 'Next' }}
          </button>
        </template>
      </footer>
    </div>
  </Teleport>
</template>

<style scoped>
/* One stacking level above `cc-popover` (1000) and BaseModal's overlay (500) — a guide must be
   readable over whatever it is pointing at, including a dialog. Kept here, next to the only two
   elements that use it, rather than added as a token nothing else would reference. */
.guide-bubble {
  z-index: 1500;
  width: 20rem;
  max-width: calc(100vw - 1rem);
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-guide);
  border-radius: var(--cc-radius-md);
  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.5);
  color: var(--cc-text);
  padding: 0.55rem 0.7rem 0.45rem;
  font-size: var(--cc-fs-md);
}

/* Pointer-events off is what keeps the highlighted control clickable THROUGH the ring — the whole
   reason this is a ring and not a spotlight with a hole (plan D9). */
.guide-ring {
  position: fixed;
  z-index: 1499;
  pointer-events: none;
  border: 2px solid var(--cc-guide);
  border-radius: var(--cc-radius-sm);
  box-shadow: 0 0 0 3px color-mix(in srgb, var(--cc-guide) 25%, transparent);
  transition: top 0.12s, left 0.12s, width 0.12s, height 0.12s;
}

.guide-arrow {
  position: absolute;
  width: 9px; height: 9px;
  background: var(--cc-surface-1);
  border-left: 1px solid var(--cc-guide);
  border-top: 1px solid var(--cc-guide);
}
/* the arrow sits on the edge FACING the anchor: a bubble placed below points up, and so on */
.guide-arrow.side-bottom { top: -5px; transform: rotate(45deg); }
.guide-arrow.side-top    { bottom: -5px; transform: rotate(225deg); }
.guide-arrow.side-right  { left: -5px; transform: rotate(-45deg); }
.guide-arrow.side-left   { right: -5px; transform: rotate(135deg); }

.gb-head { display: flex; align-items: center; gap: 0.4rem; margin-bottom: 0.3rem; }
/* no `color` here — `cc-eyebrow` owns it, and shadowing a utility's own property makes the utility a
   no-op (enforced by cssScenarios.test.ts). Layout only. */
.gb-guide { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
/* .gb-x → cc-btn cc-btn-bare cc-btn-icon cc-btn-micro */
.gb-x:hover { background: var(--cc-surface-2); color: var(--cc-text); }

.gb-title { font-weight: 600; margin: 0 0 0.15rem; }
.gb-text { margin: 0 0 0.3rem; line-height: 1.35; }
.gb-fail { color: var(--cc-sev-fail); }
.gb-ok { color: var(--cc-sev-ok); }
.gb-bullets { margin: 0 0 0.35rem; padding-left: 1.1rem; line-height: 1.4; }
.gb-bullets li { margin-bottom: 0.1rem; }

.gb-actions { display: flex; gap: 0.3rem; margin-bottom: 0.3rem; }

.gb-foot {
  display: flex; align-items: center; gap: 0.35rem;
  padding-top: 0.35rem;
  border-top: 1px solid var(--cc-border);
}
.gb-spacer { flex: 1; }
.gb-tick { color: var(--cc-sev-ok); display: inline-flex; }
</style>
