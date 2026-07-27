<!--
  WhatsNewDialog — the release-notes + tips modal. Built on BaseModal, patterned after
  ClaudeOverviewDialog. Opened from Settings and (W3) the header update badge. See
  docs/todo/WHATS_NEW_PLAN.md.

  Cards come from `lib/whatsNew.ts` (update) and `lib/tips.ts` (tips — W4). Content is data,
  not code; the layout stays the same regardless of source.
-->
<script setup lang="ts">
import { computed } from 'vue'
import BaseModal from './BaseModal.vue'
import WhatNewCard from './WhatNewCard.vue'
import { useAppControlStore } from '../stores/appControl'
import { useUpdateCard, openWithTip, viewedTipIndex, debugForceInstallable, type WhatNewCard as WhatNewCardT } from '../lib/whatsNew'
import { TIPS, todayTipIndex } from '../lib/tips'

const props = withDefaults(defineProps<{
  extraCards?: WhatNewCardT[]   // any additional cards a caller wants to prepend
}>(), { extraCards: () => [] })

defineEmits<{ (e: 'close'): void }>()

const app = useAppControlStore()
const updateCard = useUpdateCard()

// Today's tip goes to the top when the launch trigger opened the dialog (openWhatsNew({withTip:true})).
// A pagination dot row lets the user browse the rest of the catalogue in-session; today's index is
// the anchor (highlighted), viewedTipIndex tracks the user's current pick.
const dailyIndex = computed(() => todayTipIndex())
const currentIndex = computed(() => viewedTipIndex.value ?? dailyIndex.value)
const tipCard = computed<WhatNewCardT | null>(() => {
  if (!openWithTip.value) return null
  return TIPS[currentIndex.value] ?? null
})
const canCycle = computed(() => openWithTip.value && TIPS.length > 1)
const tipCounter = computed(() => {
  if (!openWithTip.value || TIPS.length < 1) return ''
  const label = `${currentIndex.value + 1} / ${TIPS.length}`
  return currentIndex.value === dailyIndex.value ? `${label} · today` : label
})
function goToTip(i: number) {
  const n = TIPS.length
  if (n < 1) return
  viewedTipIndex.value = ((i % n) + n) % n
}
function prevTip() { goToTip(currentIndex.value - 1) }
function nextTip() { goToTip(currentIndex.value + 1) }

// Non-tip cards render after the tip block (counter + card + edge-click nav) so the counter
// stays visually tied to the tip it pages, not stranded at the bottom of the modal.
const otherCards = computed<WhatNewCardT[]>(() => {
  const list: WhatNewCardT[] = []
  if (updateCard.value) list.push(updateCard.value)
  list.push(...props.extraCards)
  return list
})
const hasAnyCard = computed(() => !!tipCard.value || otherCards.value.length > 0)

// Show an inline "Install {version}" in the dialog footer when the user can self-apply. Uses the
// same appControl.applyUpdate action as the Settings panel — no divergent update path. The dev
// override (`debugForceInstallable`) lets dev checkouts preview the button.
const canInstall = computed(() =>
  debugForceInstallable.value || (app.updateAvailable && app.canApplyUpdate && !!app.updateLatest)
)
</script>

<template>
  <BaseModal title="What's new" icon="pi-sparkles" width="640px" @close="$emit('close')">
    <div v-if="hasAnyCard" class="wn-list">
      <template v-if="tipCard">
        <!-- Position counter ABOVE the card so it doesn't move vertically when sketch aspect
             ratios differ. Nav happens by clicking the sketch's left/right edges. -->
        <div v-if="canCycle" class="wn-counter cc-muted cc-fs-2xs"
             :class="{ 'wn-counter-today': currentIndex === dailyIndex }">
          {{ tipCounter }}
        </div>
        <WhatNewCard :card="tipCard" :navigable="canCycle" @nav-prev="prevTip" @nav-next="nextTip" />
      </template>
      <WhatNewCard v-for="c in otherCards" :key="c.id" :card="c" />
    </div>
    <div v-else class="wn-empty cc-muted cc-fs-md">
      Nothing new right now — you're up to date.
    </div>

    <template #footer>
      <a href="https://github.com/schienstockd/cecelia/releases" target="_blank" rel="noopener" class="wn-foot-link cc-muted">
        All releases <i class="pi pi-external-link" />
      </a>
      <span v-if="app.updateMsg" class="wn-foot-msg cc-muted cc-fs-xs">{{ app.updateMsg }}</span>
      <button v-if="canInstall" class="cc-btn cc-btn-primary cc-btn-dense wn-install-btn"
              :disabled="app.updateBusy || !app.updateLatest" @click="app.applyUpdate">
        <i :class="['pi', app.updateBusy ? 'pi-spin pi-cog' : 'pi-download']" />
        {{ app.updateBusy ? 'Installing…' : `Install ${app.updateLatest ?? '(no version)'}` }}
      </button>
      <button class="cc-btn cc-btn-ghost cc-btn-dense" :class="{ 'wn-close-btn': !canInstall }" @click="$emit('close')">Close</button>
    </template>
  </BaseModal>
</template>

<style scoped>
.wn-list { display: flex; flex-direction: column; gap: 14px; }
.wn-empty { padding: 24px 0; text-align: center; }

/* Tip position counter — static above the card so it doesn't jump when sketch heights differ.
   The "· today" suffix flips the label to accent when the user is viewing today's tip. */
.wn-counter { text-align: center; letter-spacing: 0.06em; text-transform: uppercase; }
.wn-counter-today { color: var(--cc-accent); }

.wn-foot-link {
  text-decoration: none;
  display: inline-flex;
  align-items: center;
  gap: 4px;
}
.wn-foot-link:hover { color: var(--cc-accent); }
.wn-foot-link .pi { font-size: 0.85em; }
.wn-foot-msg { margin-left: 12px; }
.wn-install-btn { margin-left: auto; }
.wn-close-btn { margin-left: auto; }
</style>
