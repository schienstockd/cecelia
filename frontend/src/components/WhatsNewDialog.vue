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
const showTipDots = computed(() => openWithTip.value && TIPS.length > 1)
function goToTip(i: number) { viewedTipIndex.value = i }

const cards = computed<WhatNewCardT[]>(() => {
  const list: WhatNewCardT[] = []
  if (tipCard.value)    list.push(tipCard.value)
  if (updateCard.value) list.push(updateCard.value)
  list.push(...props.extraCards)
  return list
})

// Show an inline "Install {version}" in the dialog footer when the user can self-apply. Uses the
// same appControl.applyUpdate action as the Settings panel — no divergent update path. The dev
// override (`debugForceInstallable`) lets dev checkouts preview the button.
const canInstall = computed(() =>
  debugForceInstallable.value || (app.updateAvailable && app.canApplyUpdate && !!app.updateLatest)
)
</script>

<template>
  <BaseModal title="What's new" icon="pi-sparkles" width="640px" @close="$emit('close')">
    <div v-if="cards.length" class="wn-list">
      <WhatNewCard v-for="c in cards" :key="c.id" :card="c" />
      <nav v-if="showTipDots" class="wn-dots" aria-label="Browse tips">
        <button
          v-for="(t, i) in TIPS" :key="t.id"
          class="wn-dot"
          :class="{ 'wn-dot-current': i === currentIndex, 'wn-dot-today': i === dailyIndex }"
          :aria-label="t.title"
          :aria-current="i === currentIndex ? 'true' : undefined"
          v-tooltip.bottom="i === dailyIndex ? `${t.title} · today's tip` : t.title"
          @click="goToTip(i)"
        />
      </nav>
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

/* Tip pagination — one dot per TIPS entry; today's tip has the anchor ring, the currently-viewed
   dot is filled. Not the same primitive as ChipSelect (this is an indicator + jump, not a picker
   of parallel choices). */
.wn-dots { display: flex; gap: 8px; justify-content: center; padding: 2px 0 0; }
.wn-dot {
  width: 9px; height: 9px; padding: 0;
  border-radius: 50%;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  cursor: pointer;
  transition: transform 100ms ease, background 100ms ease, border-color 100ms ease;
}
.wn-dot:hover { transform: scale(1.15); border-color: var(--cc-text-dim); }
.wn-dot-today { border-color: var(--cc-accent); }
.wn-dot-current { background: var(--cc-accent); border-color: var(--cc-accent); }

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
